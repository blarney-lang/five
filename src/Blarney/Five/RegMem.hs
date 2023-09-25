module Blarney.Five.RegMem where

import Blarney
import Blarney.Option

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

-- Forward stores to loads on the same cycle
-- =========================================

-- Given a register memory, return a version that forwards stores to loads
-- and always reflects the latest values on its output port.
forward :: (KnownNat lregs, KnownNat xlen) =>
  Int -> RegMem lregs xlen -> Module (RegMem lregs xlen)
forward numReadPorts regMem = do
  -- Register ids being loaded
  srcRegs  <- replicateM numReadPorts (makeReg dontCare)
  srcWires <- mapM makeWire (map (.val) srcRegs)

  -- Forwarding registers
  writeReg <- makeReg 0
  writeVal <- makeReg 0

  always do
    regMem.load (map (.val) srcWires)

  return
    RegMem {
      load = \rss -> do
        zipWithM_ (<==) srcWires rss
        zipWithM_ (<==) srcRegs rss
    , outs = 
        [ if writeReg.val .==. r.val then writeVal.val else x
        | (r, x) <- zip srcRegs regMem.outs ]
    , store = \r x -> do
        regMem.store r x
        writeReg <== r
        writeVal <== x
    }

-- Register memory using a list of registers
-- =========================================

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

-- Forwarding version
makeForwardingRegMem numReadPorts =
  makeRegMem numReadPorts >>= forward numReadPorts

-- Register memory using onchip synchronous RAMs
-- =============================================

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

-- Forwarding version
makeForwardingRegMemRAM numReadPorts =
  makeRegMemRAM numReadPorts >>= forward numReadPorts
