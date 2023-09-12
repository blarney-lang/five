module Blarney.Five.RegFile where

import Blarney
import Blarney.Option
import Blarney.Five.Util
import Blarney.Five.Interface

-- Basic register file & scoreboard
-- ================================

-- Basic register file with scoreboard to handle data hazards
makeBasicRegFile :: (Bits instr, KnownNat xlen, KnownNat lregs)
  => InstrSet xlen ilen instr lregs mreq
  -> PipelineState xlen instr
  -> Module (RF xlen instr)
makeBasicRegFile instrSet s = do
  regs     <- replicateM instrSet.numRegs (makeReg 0)
  sboard   <- replicateM instrSet.numRegs (makeReg false)
  operands <- replicateM instrSet.numSrcs (makeReg dontCare)
  stall    <- makeWire false

  always do
    -- Monitor writeback stage and perform writes
    when s.wbActive.val do
      let rd = instrSet.getDest s.wbInstr.val
      when rd.valid do
        regs ! rd.val <== s.wbResult.val
        sboard ! rd.val <== false
    -- Handle mispredicted instructions
    when s.execActive.val do
      let rd = instrSet.getDest s.execInstr.val
      when (s.execMispredict.val .&&. rd.valid) do
        sboard ! rd.val <== false

  return
    RF {
      submit = \instr -> do
        -- Load operands
        let rd = instrSet.getDest instr
        let rss = instrSet.getSrcs instr
        sequence
          [ o <== (regs ! r.val).val
          | (o, r) <- zip operands rss ]
        -- Stall if registers locked
        stall <== orList [ r.valid .&&. (sboard ! r.val).val
                         | r <- rd:rss ]
        -- Lock destination register
        when (rd.valid .&&. inv stall.val) do sboard ! rd.val <== true
    , operands = map (.val) operands
    , stall = stall.val
    }
