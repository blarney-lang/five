module Blarney.Five.BranchPredictor where

import Data.Proxy

import Blarney
import Blarney.Five.Util
import Blarney.Five.Interface

-- Branch predictor
-- ================

makeBranchPredictor ::
     KnownNat xlen
  => PipelineParams xlen ilen instr lregs mreq
  -> PipelineState xlen instr
  -> Module (BranchPredictor xlen)
makeBranchPredictor p s =
  case p.branchPredMethod of
    NaivePredictor     -> makeNaivePredictor p.iset s
    ArbitraryPredictor -> makeArbitraryPredictor p.iset s
    BTBPredictor n     -> makeBTBPredictor n p.iset s

-- Naive branch predictor
-- ======================

-- Naive predictor always predicts the next instruction
makeNaivePredictor ::
     KnownNat xlen
  => InstrSet xlen ilen instr lregs mreq
  -> PipelineState xlen instr
  -> Module (BranchPredictor xlen)
makeNaivePredictor iset s = do
  predPC <- makeReg dontCare
  return
    BranchPredictor {
      predict = \fetchPC -> do
        predPC <== fetchPC + fromIntegral iset.incPC
    , val = predPC.val
    }

-- Arbirarty predictor for verification
-- ====================================

-- Predict using a universtally-quantified variable, for verification
makeArbitraryPredictor ::
     KnownNat xlen
  => InstrSet xlen ilen instr lregs mreq
  -> PipelineState xlen instr
  -> Module (BranchPredictor xlen)
makeArbitraryPredictor iset s = do
  return
    BranchPredictor {
      predict = \fetchPC -> return ()
    , val = var "predicted_pc"
    }

-- Branch predictor using BTB
-- ==========================

-- Entry in the branch target buffer (BTB)
data BTBEntry xlen =
  BTBEntry {
    -- Valid entry?
    valid :: Bit 1
    -- Progam counter
  , pc :: Bit xlen
    -- Branch target for this PC
  , target :: Bit xlen
  }
  deriving (Generic, Bits)

-- Create BTB-based predictor using BTB with 2^n entries
makeBTBPredictor ::
     KnownNat xlen
  => Int
  -> InstrSet xlen ilen instr lregs mreq
  -> PipelineState xlen instr
  -> Module (BranchPredictor xlen)
makeBTBPredictor n iset s =
  -- Lift table size to type level
  liftNat n \(_ :: Proxy n) -> do

    -- Branch target buffer
    btb :: RAM (Bit n) (BTBEntry xlen) <- makeDualRAM

    -- BTB entry being looked up
    lookup <- makeReg dontCare

    -- Base-2 log of program counter increment
    let iwidth = log2strict iset.incPC

    -- Function to map PC to BTB index
    let getIdx :: Bit xlen -> Bit n
        getIdx = untypedSlice (n + iwidth - 1, iwidth)

    -- Update BTB
    always do
      when (iset.canBranch s.execInstr.val) do
        btb.store (getIdx s.execPC.val)
          BTBEntry {
            valid = s.execBranch_w.active
          , pc = s.execPC.val
          , target = s.execBranch_w.val
          }

    return
      BranchPredictor {
        predict = \fetchPC -> do
          btb.load (getIdx fetchPC)
          lookup <== fetchPC
      , val = if btb.out.valid .&&. btb.out.pc .==. lookup.val
                then btb.out.target
                else lookup.val + fromIntegral iset.incPC
      }
