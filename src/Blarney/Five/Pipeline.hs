module Blarney.Five.Pipeline where
  
import Blarney
import Blarney.Option
import Blarney.SourceSink
import Blarney.ClientServer
import Blarney.Five.Util
import Blarney.Five.Interface
import Blarney.Five.RegisterFile
import Blarney.Five.BranchPredictor

-- Pipeline state
-- ==============

-- Create pipeline state
makePipelineState :: (KnownNat xlen, Bits instr) =>
  Module (PipelineState xlen instr)
makePipelineState = do
  let initPC = 0
  decActive      <- makeReg false
  decPC          <- makeReg dontCare
  decStall_w     <- makeWire false
  execActive     <- makeReg false
  execInstr      <- makeReg dontCare
  execPC         <- makeReg initPC
  execMispredict <- makeSetResetBypass
  execExpectedPC <- makeReg initPC
  execStall_w    <- makeWire false
  execResult_w   <- makeWire dontCare
  execBranch_w   <- makeWire dontCare
  memActive      <- makeReg false
  memInstr       <- makeReg dontCare
  memResult      <- makeReg dontCare
  memStall_w     <- makeWire false
  wbActive       <- makeReg false
  wbInstr        <- makeReg dontCare
  wbResult       <- makeReg dontCare
  return (PipelineState {..})

-- Five-stage pipeline
-- ===================

makePipeline ::
     (KnownNat xlen, KnownNat lregs, Bits instr, Bits mreq)
  => PipelineParams xlen ilen instr lregs mreq
  -> Module (PipelineState xlen instr)
makePipeline p = do
  -- Pipeline state
  s <- makePipelineState

  -- Subcomponents
  regFile <- makeRegisterFile p s
  branchPred <- makeBranchPredictor p s

  -- Pipeline stages
  always do
    fetch p s branchPred
    decode p s regFile
    execute p s regFile
    memAccess p s
    writeback p s
  return s

  where

    -- Stage 1: instruction fetch
    -- ==========================

    fetch p s branchPred = do
      -- Address of instruction to fetch
      let fetchPC = if s.execMispredict.val
            then s.execExpectedPC.val
            else branchPred.val
      -- Setup decode stage
      when (inv s.decStall_w.val) do
        s.decActive <== p.imem.reqs.canPut
        s.decPC <== fetchPC
        -- Issue imem request
        when p.imem.reqs.canPut do
          p.imem.reqs.put fetchPC
          branchPred.predict fetchPC
          s.execMispredict.reset

    -- Stage 2: instruction decode & operand fetch
    -- ===========================================

    decode p s regFile = do
      -- Can decode stage fire?
      let canFire = p.imem.resps.canPeek
               .&&. inv s.execStall_w.val
               .&&. inv regFile.stall
      -- Issue stall to earlier stage
      s.decStall_w <== s.decActive.val .&&. inv canFire
      -- Decode instruction
      let instr = p.iset.decode p.imem.resps.peek
      -- Setup execute stage and consume imem response
      when (inv s.execStall_w.val) do
        s.execActive <== s.decActive.val
                    .&&. canFire
                    .&&. inv s.execMispredict.val
        s.execInstr <== instr
        s.execPC <== s.decPC.val
        when p.imem.resps.canPeek do
          regFile.submit instr
          when (inv regFile.stall) do p.imem.resps.consume

    -- Stage 3: execute
    -- ================

    execute p s regFile = do
      -- Is memory ready for a new request?
      let isMemAccess = p.iset.isMemAccess s.execInstr.val
      let waitForMem = isMemAccess .&&. inv p.dmem.reqs.canPut
      -- Can decode stage fire?
      let canFire = inv s.memStall_w.val .&&. inv waitForMem
      -- Issue stall to earlier stages
      s.execStall_w <== s.execActive.val .&&. inv canFire
      -- Look for misprediction
      let mispredict = s.execPC.val .!=. s.execExpectedPC.val
      when (s.execActive.val .&&. mispredict) do s.execMispredict.set
      -- Issue instruction to execution unit
      let fire = s.execActive.val .&&. canFire .&&. inv mispredict
      when fire do
        p.iset.execute s.execInstr.val
          ExecState {
            pc       = ReadWrite s.execPC.val (s.execBranch_w <==)
          , operands = regFile.operands
          , result   = WriteOnly (s.execResult_w <==)
          , memReq   = WriteOnly p.dmem.reqs.put
          }
        s.execExpectedPC <== if s.execBranch_w.active then s.execBranch_w.val
          else s.execPC.val + fromIntegral p.iset.incPC
      -- Setup memory access stage
      when (inv s.memStall_w.val) do
        s.memActive <== fire
        s.memInstr <== s.execInstr.val
        s.memResult <== s.execResult_w.val

    -- Stage 4: memory access
    -- ======================

    memAccess p s = do
      -- Do we need to wait for a memory response?
      let rd = p.iset.getDest s.memInstr.val
      let isMemAccess = p.iset.isMemAccess s.memInstr.val
      let waitResp = if isMemAccess then rd.valid else false
      -- Issue stall to earlier stages
      let stall = if s.memActive.val .&&. waitResp
            then inv p.dmem.resps.canPeek else false
      s.memStall_w <== stall
      -- Setup writeback stage and consume dmem response
      s.wbActive <== s.memActive.val .&&. inv stall
      s.wbInstr <== s.memInstr.val
      if waitResp
        then when p.dmem.resps.canPeek do
               p.dmem.resps.consume
               s.wbResult <== p.dmem.resps.peek
        else s.wbResult <== s.memResult.val

    -- Stage 5: writeback
    -- ==================

    writeback p s = return ()
