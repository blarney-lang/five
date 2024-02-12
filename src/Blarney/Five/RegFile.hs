module Blarney.Five.RegFile where

import Blarney
import Blarney.Option
import Blarney.Five.RegMem
import Blarney.Five.Interface

-- Basic register file
-- ===================

-- Basic register file with data hazard detection/stall
makeBasicRegFile :: RegMem lregs xlen ->
  PipelineComponent xlen ilen instr lregs mreq (RF xlen instr)
makeBasicRegFile regMem p s = do
  stall <- makeWire false

  always do
    -- Monitor writeback stage and perform writes
    when s.wbActive.val do
      let rd = p.instrSet.getDest s.wbInstr.val
      when rd.valid do
        regMem.store rd.val s.wbResult.val

  return
    RF {
      submit = \instr -> do
        -- Load operands
        let rss = p.instrSet.getSrcs instr
        regMem.load (map (.val) rss)
        -- Stall if register file does not hold latest value
        stall <== orList (map hazard rss)
    , operands = regMem.outs
    , stall = stall.val
    }

  where
    -- Is there a data hazard reading given register?
    hazard reg = reg.valid .&&.
         ( s.execActive.val .&&. s.execInstr `writes` reg.val
      .||. s.memActive.val  .&&. s.memInstr  `writes` reg.val
      .||. s.wbActive.val   .&&. s.wbInstr   `writes` reg.val )

    -- Does given instruction write to given reg?
    instr `writes` reg = rd.valid .&&. rd.val .==. reg
      where rd = p.instrSet.getDest instr.val

-- Forwarding register file
-- ========================

-- Forwarding register file with load hazard detection/stall
makeForwardingRegFile :: RegMem lregs xlen ->
  PipelineComponent xlen ilen instr lregs mreq (RF xlen instr)
makeForwardingRegFile regMem p s = do
  stall <- makeWire false

  always do
    -- Monitor writeback stage and perform writes
    when s.wbActive.val do
      let rd = p.instrSet.getDest s.wbInstr.val
      when rd.valid do
        regMem.store rd.val s.wbResult.val

  return
    RF {
      submit = \instr -> do
        -- Load operands
        let rss = p.instrSet.getSrcs instr
        regMem.load (map (.val) rss)
        -- Stall if register file does not hold latest value
        stall <== orList (map hazard rss)
    , operands =
        let rss = p.instrSet.getSrcs s.execInstr.val in
          zipWith forward (map (.val) rss) regMem.outs
    , stall = stall.val
    }

  where
    -- Is there a data hazard reading given register?
    hazard reg = reg.valid .&&.
         ( s.execActive.val .&&. s.execInstr `loads` reg.val
      .||. s.memStall.val   .&&. s.memInstr  `loads` reg.val )

    -- Does given instruction write to given reg?
    instr `writes` reg = rd.valid .&&. rd.val .==. reg
      where rd = p.instrSet.getDest instr.val

    -- Does given instruction load from memory into given reg?
    instr `loads` reg = p.instrSet.isMemAccess instr.val .&&.
                          instr `writes` reg

    -- Get latest value of given register
    forward reg fromRF =
      if s.memActive.val .&&. s.memInstr `writes` reg then s.memResult.val
        else if s.wbActive.val .&&. s.wbInstr `writes` reg then s.wbResult.val
          else fromRF
