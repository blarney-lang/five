-- TODO: consider suffixing reg fields with _r and/or wire fields with _w
-- TODO: consider using vectors instead of lists

module Blarney.Five.Pipeline where
  
import Blarney
import Blarney.Option
import Blarney.SourceSink
import Blarney.ClientServer
import Blarney.Five.Util
import Blarney.Five.Interface

-- Pipeline state
-- ==============

-- Create pipeline state
makeState ::
  (KnownNat xlen, Bits instr, Bits mreq) =>
       PipelineParams xlen ilen instr lregs mreq
    -> Module (PipelineState xlen instr)
makeState p = do
  decActive      <- makeReg false
  decPC          <- makeReg dontCare
  decStall       <- makeWire false
  execActive     <- makeReg false
  execInstr      <- makeReg dontCare
  execPC         <- makeReg dontCare
  execOperands   <- replicateM p.instrSet.numSrcs
                      (makeWire dontCare)
  execMispredict <- makeWire false
  execExpectedPC <- makeReg p.initPC
  execStall      <- makeWire false
  execResult     <- makeWire dontCare
  memActive      <- makeReg false
  memInstr       <- makeReg dontCare
  memResult      <- makeReg dontCare
  memStall       <- makeWire false
  wbActive       <- makeReg false
  wbInstr        <- makeReg dontCare
  wbResult       <- makeReg dontCare
  return (PipelineState {..})

-- Pipeline stages
-- ===============

-- Each pipeline stage has the following type
type PipelineStage xlen ilen instr lregs mreq =
  (KnownNat xlen, Bits instr, Bits mreq) =>
       PipelineParams xlen ilen instr lregs mreq
    -> PipelineState xlen instr
    -> Module ()

-- Stage 1: instruction fetch
-- ==========================

fetch :: PipelineStage xlen ilen instr lregs mreq
fetch p s = do
  -- Create branch target predictor
  branchPred <- p.makeBranchPred s

  -- Have we seen a misprediction since the previous fetch?
  seenMispred <- makeReg false

  always do
    let fetchPC = if s.execMispredict.val .||. seenMispred.val
          then s.execExpectedPC.val
          else branchPred.out
    -- Issue imem request
    if inv s.decStall.val .&&. p.imem.reqs.canPut
      then do
        p.imem.reqs.put fetchPC
        branchPred.predict fetchPC
        seenMispred <== false
      else do
        when s.execMispredict.val do
          seenMispred <== true
    -- Setup decode stage
    when (inv s.decStall.val) do
      s.decActive <== p.imem.reqs.canPut
      s.decPC <== fetchPC

-- Stage 2: instruction decode & operand fetch
-- ===========================================

decode :: PipelineStage xlen ilen instr lregs mreq
decode p s = do
  -- Create register file
  regFile <- p.makeRegFile s

  always do
    -- Decode instruction
    let instr = p.instrSet.decode p.imem.resps.peek
    -- Issue stall to earlier stage
    let stall = s.execStall.val .||. regFile.stall
                                .||. inv p.imem.resps.canPeek
    s.decStall <== s.decActive.val .&&. stall
    -- Setup execute stage and consume imem response
    zipWithM (<==) s.execOperands regFile.operands
    when (inv s.execStall.val) do
      s.execActive <== s.decActive.val .&&. inv stall
      s.execInstr <== instr
      s.execPC <== s.decPC.val
      when p.imem.resps.canPeek do
        regFile.submit instr
        when (inv regFile.stall) do p.imem.resps.consume

-- Stage 3: execute
-- ================

execute :: PipelineStage xlen ilen instr lregs mreq
execute p s = do
  -- Create execution unit
  execUnit <- p.instrSet.makeExecUnit

  -- Wires for communicating with execution unit
  nextPC <- makeWire (s.execPC.val + p.instrLen)
  memReq <- makeWire dontCare

  always do
    -- Is memory ready for a new request?
    let isMemAccess = p.instrSet.isMemAccess s.execInstr.val
    let memReady = if isMemAccess then p.dmem.reqs.canPut else true
    -- Issue stall to earlier stages
    s.execStall <== s.execActive.val .&&.
      (s.memStall.val .||. inv memReady)
    -- Look for misprediction
    let mispredict = s.execPC.val .!=. s.execExpectedPC.val
    s.execMispredict <== s.execActive.val .&&. mispredict
    -- Issue instruction to execution unit
    let fire = s.execActive.val .&&. inv s.memStall.val .&&.
                 inv mispredict .&&. memReady
    when fire do
      execUnit.issue s.execInstr.val
        ExecState {
          pc       = ReadWrite s.execPC.val (nextPC <==)
        , operands = map (.val) s.execOperands
        , result   = WriteOnly (s.execResult <==)
        , memReq   = WriteOnly (memReq <==)
        }
      s.execExpectedPC <== nextPC.val
      when isMemAccess do p.dmem.reqs.put memReq.val
    -- Setup memory access stage
    when (inv s.memStall.val) do
      s.memActive <== fire
      s.memInstr <== s.execInstr.val
      s.memResult <== s.execResult.val

-- Stage 4: memory access
-- ======================

memAccess :: PipelineStage xlen ilen instr lregs mreq
memAccess p s = do
  always do
    -- Do we need to wait for a memory response?
    let rd = p.instrSet.getDest s.memInstr.val
    let isMemAccess = p.instrSet.isMemAccess s.memInstr.val
    let waitResp = if isMemAccess then rd.valid else false
    -- Issue stall to earlier stages
    let stall = if s.memActive.val .&&. waitResp
          then inv p.dmem.resps.canPeek else false
    s.memStall <== stall
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

writeback :: PipelineStage xlen ilen instr lregs mreq
writeback p s = return ()

-- Classic 5-stage pipeline
-- ========================

makeClassic ::
  (KnownNat xlen, Bits instr, Bits mreq) =>
       PipelineParams xlen ilen instr lregs mreq
    -> Module (PipelineState xlen instr)
makeClassic p = do
  s <- makeState p
  fetch p s
  decode p s
  execute p s
  memAccess p s
  writeback p s
  return s
