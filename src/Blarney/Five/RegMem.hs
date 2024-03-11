module Blarney.Five.RegMem
  ( makeRegMem
  , makeRegMemRAM
  ) where

import Blarney
import Blarney.Option
import Blarney.Five.Interface

-- Forward stores to loads on the same cycle
-- =========================================

-- Given a register memory, return a version that forwards stores to loads
-- and always reflects the latest values on its output port.
forward :: (KnownNat lregs, KnownNat xlen) =>
  RegMem lregs xlen -> Module (RegMem lregs xlen)
forward regMem = do
   -- Register ids being loaded
   srcReg1  <- makeReg dontCare
   srcReg2  <- makeReg dontCare
   srcWire1 <- makeWire srcReg1.val
   srcWire2 <- makeWire srcReg2.val
 
   -- Forwarding registers
   writeReg <- makeReg 0
   writeVal <- makeReg 0
 
   always do
     regMem.load (srcWire1.val, srcWire2.val)
 
   -- Forwarding logic
   let fwd r m = if writeReg.val .==. r then writeVal.val else m
 
   return
     RegMem {
       load = \(rs1, rs2) -> do
         srcReg1 <== rs1
         srcReg2 <== rs2
         srcWire1 <== rs1
         srcWire2 <== rs2
     , outs = let (x1, x2) = regMem.outs in
                (fwd srcReg1.val x1, fwd srcReg2.val x2)
     , store = \r x -> do
         regMem.store r x
         writeReg <== r
         writeVal <== x
     }

-- Register memory using a list of registers
-- =========================================

makeRegMem :: forall lregs xlen. (KnownNat lregs, KnownNat xlen) =>
  Module (RegMem lregs xlen)
makeRegMem = do
  -- Register array and operand latches
  regs   <- replicateM (2 ^ valueOf @lregs) (makeReg 0)
  latch1 <- makeReg 0
  latch2 <- makeReg 0

  forward
    RegMem {
      load = \(rs1, rs2) -> do
        latch1 <== (regs!rs1).val
        latch2 <== (regs!rs2).val
    , outs = (latch1.val, latch2.val)
    , store = \r x ->
        (regs!r) <== x
    }

-- Register memory using onchip synchronous RAMs
-- =============================================

makeRegMemRAM :: (KnownNat lregs, KnownNat xlen) =>
  Module (RegMem lregs xlen)
makeRegMemRAM = do
  -- Register RAM, one per read port
  ram1 <- makeDualRAM
  ram2 <- makeDualRAM

  -- Read enable wire
  readEnable <- makeWire false

  forward
    RegMem {
      load = \(rs1, rs2) -> do
        ram1.load rs1
        ram2.load rs2
    , outs = (ram1.out, ram2.out)
    , store = \r x -> do
        ram1.store r x
        ram2.store r x
    }
