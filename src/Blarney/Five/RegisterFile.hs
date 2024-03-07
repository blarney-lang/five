module Blarney.Five.RegisterFile where

import Blarney
import Blarney.Option
import Blarney.Five.RegMem
import Blarney.Five.Interface

-- Regsiter file
-- =============

makeRegisterFile ::
     (KnownNat xlen, KnownNat lregs)
  => PipelineParams xlen ilen instr lregs mreq
  -> PipelineState xlen instr
  -> Module (RegisterFile xlen instr)
makeRegisterFile p s = do
  let nports = p.regFileParams.numReadPorts
  regMem <-
    if p.regFileParams.useRAM
      then if p.regFileParams.useForwarding
             then makeForwardingRegMemRAM nports
             else makeRegMemRAM nports
      else if p.regFileParams.useForwarding
             then makeForwardingRegMem nports
             else makeRegMem nports
  if p.regFileParams.useForwarding
    then makeForwardingRegFile regMem p.iset s
    else makeBasicRegFile regMem p.iset s

-- Basic register file
-- ===================

-- Basic register file with data hazard detection/stall
makeBasicRegFile ::
     RegMem lregs xlen
  -> InstrSet xlen ilen instr lregs mreq
  -> PipelineState xlen instr
  -> Module (RegisterFile xlen instr)
makeBasicRegFile regMem iset s = do
  stall <- makeWire false

  always do
    -- Monitor writeback stage and perform writes
    when s.wbActive.val do
      let rd = iset.getDest s.wbInstr.val
      when rd.valid do
        regMem.store rd.val s.wbResult.val

  return
    RegisterFile {
      submit = \instr -> do
        -- Load operands
        let (rs1, rs2) = iset.getSrcs instr
        regMem.load (rs1.val, rs2.val)
        -- Stall if register file does not hold latest value
        stall <== orList (map hazard [rs1, rs2])
    , operands = regMem.outs
    , stall = stall.val
    }

  where
    -- Is there a data hazard reading the given source register?
    hazard reg = reg.valid .&&.
         ( s.execActive.val .&&. s.execInstr `writes` reg.val
      .||. s.memActive.val  .&&. s.memInstr  `writes` reg.val
      .||. s.wbActive.val   .&&. s.wbInstr   `writes` reg.val )

    -- Does given instruction write to given reg?
    instr `writes` reg = rd.valid .&&. rd.val .==. reg
      where rd = iset.getDest instr.val

-- Forwarding register file
-- ========================

-- Forwarding register file with load hazard detection/stall
makeForwardingRegFile ::
     (KnownNat xlen)
  => RegMem lregs xlen
  -> InstrSet xlen ilen instr lregs mreq
  -> PipelineState xlen instr
  -> Module (RegisterFile xlen instr)
makeForwardingRegFile regMem iset s = do
  stall <- makeWire false

  always do
    -- Monitor writeback stage and perform writes
    when s.wbActive.val do
      let rd = iset.getDest s.wbInstr.val
      when rd.valid do
        regMem.store rd.val s.wbResult.val

  return
    RegisterFile {
      submit = \instr -> do
        -- Load operands
        let (rs1, rs2) = iset.getSrcs instr
        regMem.load (rs1.val, rs2.val)
        -- Stall if register file does not hold latest value
        stall <== orList (map hazard [rs1, rs2])
    , operands =
        let (rs1, rs2) = iset.getSrcs s.execInstr.val in
          (forward rs1.val (fst regMem.outs),
           forward rs2.val (snd regMem.outs))
    , stall = stall.val
    }

  where
    -- Is there a data hazard reading given register?
    hazard reg = reg.valid .&&.
         ( s.execActive.val .&&. s.execInstr `loads` reg.val
      .||. s.memStall_w.val   .&&. s.memInstr  `loads` reg.val )

    -- Does given instruction write to given reg?
    instr `writes` reg = rd.valid .&&. rd.val .==. reg
      where rd = iset.getDest instr.val

    -- Does given instruction load from memory into given reg?
    instr `loads` reg = iset.isMemAccess instr.val .&&.
                          instr `writes` reg

    -- Get latest value of given register
    forward reg fromRF =
      if s.memActive.val .&&. s.memInstr `writes` reg
         then s.memResult.val
         else if s.wbActive.val .&&. s.wbInstr `writes` reg
                then s.wbResult.val else fromRF
