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
makePipelineState ::
     (KnownNat xlen, Bits instr)
  => Bit xlen
  -> Module (PipelineState xlen instr)
makePipelineState initPC = do
  decActive      <- makeReg false
  decPC          <- makeReg dontCare
  decStall       <- makeWire false
  execActive     <- makeReg false
  execInstr      <- makeReg dontCare
  execPC         <- makeReg dontCare
  execMispredict <- makeSetResetBypass
  execExpectedPC <- makeReg initPC
  execStall      <- makeWire false
  execResult     <- makeWire dontCare
  execBranch     <- makeWire dontCare
  memActive      <- makeReg false
  memInstr       <- makeReg dontCare
  memResult      <- makeReg dontCare
  memStall       <- makeWire false
  wbActive       <- makeReg false
  wbInstr        <- makeReg dontCare
  wbResult       <- makeReg dontCare
  return (PipelineState {..})

-- Pipeline stage
-- ==============

type PipelineStage xlen ilen instr lregs mreq =
     (KnownNat xlen, Bits instr, Bits mreq)
  => PipelineInterface xlen ilen instr mreq
  -> InstrSet xlen ilen instr lregs mreq
  -> PipelineState xlen instr
  -> Module ()

-- Stage 1: instruction fetch
-- ==========================

fetch :: PipelineStage xlen ilen instr lregs mreq
fetch ifc iset s = always do
  -- Address of instruction to fetch
  let fetchPC = if s.execMispredict.val
        then s.execExpectedPC.val
        else ifc.branchPred.out
  -- Setup decode stage
  when (inv s.decStall.val) do
    s.decActive <== ifc.imem.reqs.canPut
    s.decPC <== fetchPC
    -- Issue imem request
    when ifc.imem.reqs.canPut do
      ifc.imem.reqs.put fetchPC
      ifc.branchPred.predict fetchPC
      s.execMispredict.reset

-- Stage 2: instruction decode & operand fetch
-- ===========================================

decode :: PipelineStage xlen ilen instr lregs mreq
decode ifc iset s = always do
  -- Can decode stage fire?
  let canFire = ifc.imem.resps.canPeek
           .&&. inv s.execStall.val
           .&&. inv ifc.regFile.stall
  -- Issue stall to earlier stage
  s.decStall <== s.decActive.val .&&. inv canFire
  -- Decode instruction
  let instr = iset.decode ifc.imem.resps.peek
  -- Setup execute stage and consume imem response
  when (inv s.execStall.val) do
    s.execActive <== s.decActive.val
                .&&. canFire
                .&&. inv s.execMispredict.val
    s.execInstr <== instr
    s.execPC <== s.decPC.val
    when ifc.imem.resps.canPeek do
      ifc.regFile.submit instr
      when (inv ifc.regFile.stall) do ifc.imem.resps.consume

-- Stage 3: execute
-- ================

execute :: PipelineStage xlen ilen instr lregs mreq
execute ifc iset s = always do
  -- Is memory ready for a new request?
  let isMemAccess = iset.isMemAccess s.execInstr.val
  let waitForMem = isMemAccess .&&. inv ifc.dmem.reqs.canPut
  -- Can decode stage fire?
  let canFire = inv s.memStall.val .&&. inv waitForMem
  -- Issue stall to earlier stages
  s.execStall <== s.execActive.val .&&. inv canFire
  -- Look for misprediction
  let mispredict = s.execPC.val .!=. s.execExpectedPC.val
  when (s.execActive.val .&&. mispredict) do s.execMispredict.set
  -- Issue instruction to execution unit
  let fire = s.execActive.val .&&. canFire .&&. inv mispredict
  when fire do
    iset.execute s.execInstr.val
      ExecState {
        pc       = ReadWrite s.execPC.val (s.execBranch <==)
      , operands = ifc.regFile.operands
      , result   = WriteOnly (s.execResult <==)
      , memReq   = WriteOnly ifc.dmem.reqs.put
      }
    s.execExpectedPC <== if s.execBranch.active then s.execBranch.val
      else s.execPC.val + fromIntegral iset.incPC
  -- Setup memory access stage
  when (inv s.memStall.val) do
    s.memActive <== fire
    s.memInstr <== s.execInstr.val
    s.memResult <== s.execResult.val

-- Stage 4: memory access
-- ======================

memAccess :: PipelineStage xlen ilen instr lregs mreq
memAccess ifc iset s = always do
  -- Do we need to wait for a memory response?
  let rd = iset.getDest s.memInstr.val
  let isMemAccess = iset.isMemAccess s.memInstr.val
  let waitResp = if isMemAccess then rd.valid else false
  -- Issue stall to earlier stages
  let stall = if s.memActive.val .&&. waitResp
        then inv ifc.dmem.resps.canPeek else false
  s.memStall <== stall
  -- Setup writeback stage and consume dmem response
  s.wbActive <== s.memActive.val .&&. inv stall
  s.wbInstr <== s.memInstr.val
  if waitResp
    then when ifc.dmem.resps.canPeek do
           ifc.dmem.resps.consume
           s.wbResult <== ifc.dmem.resps.peek
    else s.wbResult <== s.memResult.val

-- Stage 5: writeback
-- ==================

writeback :: PipelineStage xlen ilen instr lregs mreq
writeback ifc iset s = return ()

-- Classic 5-stage pipeline
-- ========================

makePipeline :: PipelineStage xlen ilen instr lregs mreq
makePipeline ifc iset s = do
  fetch ifc iset s
  decode ifc iset s
  execute ifc iset s
  memAccess ifc iset s
  writeback ifc iset s
