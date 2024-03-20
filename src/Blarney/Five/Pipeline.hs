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

-- Pipeline stage
-- ==============

type PipelineStage xlen ilen instr lregs mreq =
     (KnownNat xlen, KnownNat lregs, Bits instr, Bits mreq)
  => PipelineParams xlen ilen instr lregs mreq
  -> PipelineState xlen instr
  -> Action ()

-- Stage 1: instruction fetch
-- ==========================

fetch :: PipelineStage xlen ilen instr lregs mreq
fetch p s = do
  -- Address of instruction to fetch
  let fetchPC = if s.execMispredict.val
        then s.execExpectedPC.val
        else p.branchPred.val
  -- Setup decode stage
  when (inv s.decStall_w.val) do
    s.decActive <== p.imem.reqs.canPut
    s.decPC <== fetchPC
    -- Issue imem request
    when p.imem.reqs.canPut do
      p.imem.reqs.put fetchPC
      p.branchPred.predict fetchPC
      s.execMispredict.reset

-- Stage 2: instruction decode & operand fetch
-- ===========================================

decode :: PipelineStage xlen ilen instr lregs mreq
decode p s = do
  -- Decode instruction
  let instr = p.iset.decode p.imem.resps.peek
  let (rs1, rs2) = p.iset.getSrcs instr
  -- Are we reading from a register whose value we don't know yet?
  let dataHazard = hazard p s rs1 .||. hazard p s rs2
  -- Can decode stage fire?
  let canFire = p.imem.resps.canPeek
           .&&. inv s.execStall_w.val
           .&&. inv dataHazard
  -- Issue stall to earlier stage
  s.decStall_w <== s.decActive.val .&&. inv canFire
  -- Consume imem response
  when (s.decActive.val .&&. canFire) do p.imem.resps.consume
  -- Setup execute stage
  when (inv s.execStall_w.val) do
    s.execActive <== s.decActive.val
                .&&. canFire
                .&&. inv s.execMispredict.val
    s.execInstr <== instr
    s.execPC <== s.decPC.val
    p.regFile.load (rs1.val, rs2.val)

-- Stage 3: execute
-- ================

execute :: PipelineStage xlen ilen instr lregs mreq
execute p s = do
  -- Is memory ready for a new request?
  let isMemAccess = p.iset.isMemAccess s.execInstr.val
  let waitForMem = isMemAccess .&&. inv p.dmem.reqs.canPut
  -- Can execute stage fire?
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
      , operands = let (x1, x2) = p.regFile.outs
                       (r1, r2) = p.iset.getSrcs s.execInstr.val
                   in  (forward p s r1 x1, forward p s r2 x2)
      , result   = WriteOnly (s.execResult_w <==)
      , memReq   = WriteOnly p.dmem.reqs.put
      }
    s.execExpectedPC <== if s.execBranch_w.active then s.execBranch_w.val
      else s.execPC.val + fromIntegral p.iset.incPC
    p.branchPred.train s.execInstr.val s.execPC.val
                       (Option s.execBranch_w.active s.execBranch_w.val)
  -- Setup memory access stage
  when (inv s.memStall_w.val) do
    s.memActive <== fire
    s.memInstr <== s.execInstr.val
    s.memResult <== s.execResult_w.val

-- Stage 4: memory access
-- ======================

memAccess :: PipelineStage xlen ilen instr lregs mreq
memAccess p s = do
  -- Do we need to wait for a memory response?
  let rd = p.iset.getDest s.memInstr.val
  let isMemAccess = p.iset.isMemAccess s.memInstr.val
  let waitResp = if isMemAccess then rd.valid else false
  -- Issue stall to earlier stages
  let canFire = inv waitResp .||. p.dmem.resps.canPeek
  s.memStall_w <== s.memActive.val .&&. inv canFire
  let stall = if s.memActive.val .&&. waitResp
        then inv p.dmem.resps.canPeek else false
  s.memStall_w <== stall
  -- Setup writeback stage and consume dmem response
  s.wbActive <== s.memActive.val .&&. canFire
  s.wbInstr <== s.memInstr.val
  if waitResp
    then when p.dmem.resps.canPeek do
           p.dmem.resps.consume
           s.wbResult <== p.dmem.resps.peek
    else s.wbResult <== s.memResult.val

-- Stage 5: writeback
-- ==================

writeback :: PipelineStage xlen ilen instr lregs mreq
writeback p s = do
  when s.wbActive.val do
    let rd = p.iset.getDest s.wbInstr.val
    when rd.valid do
      p.regFile.store rd.val s.wbResult.val

-- Data hazard detection
-- =====================

-- Is there a data hazard when reading from the given source register?
hazard p s src = src.valid .&&.
     ( s.execActive.val .&&. s.execInstr `loads` src
  .||. s.memStall_w.val .&&. s.memInstr  `loads` src )
  where
    -- Does given instruction load from memory into given reg?
    instr `loads` reg = p.iset.isMemAccess instr.val .&&.
                          rd.valid .&&. rd.val .==. reg.val
      where rd = p.iset.getDest instr.val

-- Register forwarding
-- ===================

-- Override value old from the register file if there is a more recent value
forward p s src old =
  if s.memActive.val .&&. s.memInstr `writes` src
    then s.memResult.val
    else if s.wbActive.val .&&. s.wbInstr `writes` src
           then s.wbResult.val else old
  where
    -- Does given instruction write to given reg?
    instr `writes` reg = rd.valid .&&. rd.val .==. reg.val
      where rd = p.iset.getDest instr.val

-- Five-stage pipeline
-- ===================

makePipeline ::
     (KnownNat xlen, KnownNat lregs, Bits instr, Bits mreq)
  => PipelineParams xlen ilen instr lregs mreq
  -> Module (PipelineState xlen instr)
makePipeline p = do
  -- Pipeline state
  s <- makePipelineState

  -- Pipeline stages
  always do
    fetch p s
    decode p s
    execute p s
    memAccess p s
    writeback p s
  return s
