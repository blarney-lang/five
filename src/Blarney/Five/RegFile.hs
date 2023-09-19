module Blarney.Five.RegFile where

import Blarney
import Blarney.Option
import Blarney.Five.Util
import Blarney.Five.Interface

-- Register memory abstraction
-- ===========================

-- Register memory abstraction, parameterised by the number of
-- registers (2^lregs) and the register size (xlen).
data RegMem lregs xlen =
  RegMem {
    -- Load each of the given operands
    load  :: [Bit lregs] -> Action ()
    -- Values loaded, valid one cycle after call to load and preserved
    -- until load is called against.
  , outs  :: [Bit xlen]
    -- Overwrite value of given register with given value.
  , store :: Bit lregs -> Bit xlen -> Action ()
  }

-- Register memory using a list of registers
makeRegMem :: forall lregs xlen. (KnownNat lregs, KnownNat xlen) =>
  Int -> Module (RegMem lregs xlen)
makeRegMem numReadPorts = do
  -- Register array and operand latches
  regs    <- replicateM (2 ^ valueOf @lregs) (makeReg 0)
  latches <- replicateM numReadPorts (makeReg 0)

  return
    RegMem {
      load = \rss ->
        sequence_
          [ latch <== (regs!r).val
          | (latch, r) <- zip latches rss ]
    , outs = map (.val) latches
    , store = \r x ->
        (regs!r) <== x
    }

-- Register memory using onchip synchronous RAMs
makeRegMemRAM :: (KnownNat lregs, KnownNat xlen) =>
  Int -> Module (RegMem lregs xlen)
makeRegMemRAM numReadPorts = do
  -- Register RAM, one per read port
  rams <- replicateM numReadPorts makeDualRAM

  -- Read enable wire
  readEnable <- makeWire false

  always do
    when (inv readEnable.val) do
      sequence_ [ ram.preserveOut | ram <- rams ]

  return
    RegMem {
      load = \rss -> do
        sequence
          [ ram.load r
          | (ram, r) <- zip rams rss ]
        readEnable <== true
    , outs = map (.out) rams
    , store = \r x ->
        sequence_ [ ram.store r x | ram <- rams ]
    }

-- Basic register file & scoreboard
-- ================================

-- Basic register file with scoreboard to handle data hazards
makeBasicRegFile :: (Bits instr, KnownNat xlen, KnownNat lregs)
  => Module (RegMem lregs xlen)
  -> InstrSet xlen ilen instr lregs mreq
  -> PipelineState xlen instr
  -> Module (RF xlen instr)
makeBasicRegFile makeRegMem instrSet s = do
  regMem   <- makeRegMem
  sboard   <- replicateM instrSet.numRegs (makeReg false)
  stall    <- makeWire false

  always do
    -- Monitor writeback stage and perform writes
    when s.wbActive.val do
      let rd = instrSet.getDest s.wbInstr.val
      when rd.valid do
        regMem.store rd.val s.wbResult.val
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
        regMem.load (map (.val) rss)
        -- Stall if registers locked
        stall <== orList [ r.valid .&&. (sboard ! r.val).val
                         | r <- rd:rss ]
        -- Lock destination register
        when (rd.valid .&&. inv stall.val) do sboard ! rd.val <== true
    , operands = regMem.outs
    , stall = stall.val
    }
