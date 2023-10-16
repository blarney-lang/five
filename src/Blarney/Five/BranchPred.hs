module Blarney.Five.BranchPred where

import Blarney
import Blarney.Five.Util
import Blarney.Five.Interface

-- Naive branch target predictor
-- =============================

-- Naive predictor always predicts the next instruction
makeNaivePredictor ::
     KnownNat xlen
  => Bit xlen
  -> PipelineState xlen mreq
  -> Module (BranchPred xlen)
makeNaivePredictor instrLen s = do
  predPC <- makeReg dontCare
  return
    BranchPred {
      predict = \fetchPC -> do
        predPC <== fetchPC + instrLen
    , out = predPC.val
    }

-- Arbirarty predictor for verification
-- ====================================

-- Predict using a universtally-quantified variable, for verification
makeArbitraryPredictor ::
     KnownNat xlen
  => Bit xlen
  -> PipelineState xlen mreq
  -> Module (BranchPred xlen)
makeArbitraryPredictor instrLen s = do
  return
    BranchPred {
      predict = \fetchPC -> return ()
    , out = var "predicted_pc"
    }

-- Branch target predictor using BTB
-- =================================

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
makeBTBPredictor :: forall n xlen mreq.
     (KnownNat n, KnownNat xlen)
  => Int
  -> PipelineState xlen mreq
  -> Module (BranchPred xlen)
makeBTBPredictor logInstrLen s = do
  -- Branch target buffer
  -- TODO: need to call preserveOut when not loading?
  btb :: RAM (Bit n) (BTBEntry xlen) <- makeDualRAM

  -- BTB entry being looked up
  lookup <- makeReg dontCare

  -- Function to map PC to RAM index
  let getIdx :: Bit xlen -> Bit n
      getIdx = unsafeSlice (valueOf @n + logInstrLen - 1, logInstrLen)

  -- Update BTB
  always do
    when s.execBranch.active do
      btb.store (getIdx s.execPC.val)
        BTBEntry {
          valid = true
        , pc = s.execPC.val
        , target = s.execBranch.val
        }

  return
    BranchPred {
      predict = \fetchPC -> do
        btb.load (getIdx fetchPC)
        lookup <== fetchPC
    , out = if btb.out.valid .&&. btb.out.pc .==. lookup.val
              then btb.out.target
              else lookup.val + fromInteger (2 ^ logInstrLen)
    }
