module Blarney.Five.BranchPred where

import Blarney
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
