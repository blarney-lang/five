module Blarney.Five.BranchPredictor where

import Data.Proxy

import Blarney
import Blarney.Option
import Blarney.Five.Util
import Blarney.Five.Interface

-- Naive branch predictor
-- ======================

-- Naive predictor always predicts the next instruction
makeNaivePredictor ::
     KnownNat xlen
  => InstrSet xlen ilen instr lregs mreq
  -> Module (BranchPredictor xlen instr)
makeNaivePredictor iset = do
  predPC <- makeReg dontCare
  return
    BranchPredictor {
      predict = \fetchPC -> do
        predPC <== fetchPC + fromIntegral iset.incPC
    , val = predPC.val
    , train = \_ _ _ -> return ()
    }

-- Arbirarty predictor for verification
-- ====================================

-- Predict using a universtally-quantified variable, for verification
makeArbitraryPredictor ::
     KnownNat xlen
  => InstrSet xlen ilen instr lregs mreq
  -> Module (BranchPredictor xlen instr)
makeArbitraryPredictor iset = do
  return
    BranchPredictor {
      predict = \fetchPC -> return ()
    , val = var "predicted_pc"
    , train = \_ _ _ -> return ()
    }

-- Branch predictor using BTB
-- ==========================

-- Entry in the branch target buffer (BTB)
data BTBEntry xlen =
  BTBEntry {
    -- Branch taken?
    taken :: Bit 1
    -- Progam counter
  , pc :: Bit xlen
    -- Branch target for this PC
  , target :: Bit xlen
  }
  deriving (Generic, Bits)

-- Create BTB-based predictor using BTB with 2^n entries
makeBTBPredictor :: forall n xlen ilen instr lregs mreq.
     (KnownNat n, KnownNat xlen)
  => InstrSet xlen ilen instr lregs mreq
  -> Module (BranchPredictor xlen instr)
makeBTBPredictor iset = do
  -- Branch target buffer
  btb :: RAM (Bit n) (BTBEntry xlen) <- makeDualRAM

  -- BTB entry being looked up
  lookup <- makeReg dontCare

  -- Base-2 log of program counter increment
  let iwidth = log2strict iset.incPC

  -- Function to map PC to BTB index
  let getIdx :: Bit xlen -> Bit n
      getIdx = untypedSlice (valueOf @n + iwidth - 1, iwidth)

  return
    BranchPredictor {
      predict = \fetchPC -> do
        btb.load (getIdx fetchPC)
        lookup <== fetchPC
    , val = if btb.out.pc .==. lookup.val .&&. btb.out.taken
              then btb.out.target
              else lookup.val + fromIntegral iset.incPC
    , train = \instr pc target -> do
        when (iset.canBranch instr) do
          btb.store (getIdx pc) (BTBEntry target.valid pc target.val)
    }
