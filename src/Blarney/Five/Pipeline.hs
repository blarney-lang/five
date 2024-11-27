module Blarney.Five.Pipeline (makePipeline) where
  
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

-- By convention, we suffix wire field names with "_w".
data PipelineState xlen instr mreq =
  PipelineState {
    -- Is the decode stage active?
    decActive :: Reg (Bit 1)
    -- If so, the PC of the instruction in the decode stage.
  , decPC :: Reg (Bit xlen)
    -- Is the decode stage stalling?
  , decStall_w :: Wire (Bit 1)

    -- Is the execute stage active?
  , execActive :: Reg (Bit 1)
    -- If so, the instruction to execute with its PC and operands.
  , execInstr :: Reg instr
  , execPC :: Reg (Bit xlen)
    -- The PC of the next instruction expected in the execute stage.
  , execExpectedPC :: Reg (Bit xlen)
    -- Is the instruction in the execute stage mispredicted?
  , execMispredict :: SetReset
    -- Is the execute stage stalling?
  , execStall_w :: Wire (Bit 1)
    -- Branch target of executed instruction
  , execBranch_w :: Wire (Bit xlen)

    -- Is the memory access stage active?
  , memActive :: Reg (Bit 1)
    -- If so, the memory request, the instruction, and its result
  , memReq :: Reg mreq
  , memInstr :: Reg instr
  , memResult :: Reg (Bit xlen)
    -- Is the memory access stage stalling?
  , memStall_w :: Wire (Bit 1)

    -- Is the writeback stage active?
  , wbActive :: Reg (Bit 1)
    -- If so, the instruction and its result.
  , wbInstr :: Reg instr
  , wbResult :: Reg (Bit xlen)
    -- Is the writeback stage stalling?
  , wbStall_w :: Wire (Bit 1)
  }

-- Create pipeline state
makePipelineState :: (KnownNat xlen, Bits instr, Bits mreq) =>
  Module (PipelineState xlen instr mreq)
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
  execBranch_w   <- makeWire dontCare
  memActive      <- makeReg false
  memInstr       <- makeReg dontCare
  memReq         <- makeReg dontCare
  memResult      <- makeReg dontCare
  memStall_w     <- makeWire false
  wbActive       <- makeReg false
  wbInstr        <- makeReg dontCare
  wbResult       <- makeReg dontCare
  wbStall_w      <- makeWire false
  return (PipelineState {..})

-- Pipeline stage
-- ==============

type PipelineStage xlen ilen instr lregs mreq =
     (KnownNat xlen, KnownNat lregs, Bits instr, Bits mreq)
  => PipelineParams xlen ilen instr lregs mreq
  -> PipelineState xlen instr mreq
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
  -- Issue stall to earlier stages
  s.execStall_w <== s.execActive.val .&&. s.memStall_w.val
  -- Look for misprediction
  let mispredict = s.execPC.val .!=. s.execExpectedPC.val
  when (s.execActive.val .&&. mispredict) do s.execMispredict.set
  -- Issue instruction to execution unit
  let fire = s.execActive.val .&&. inv s.memStall_w.val .&&. inv mispredict
  when fire do
    p.iset.execute s.execInstr.val
      ExecState {
        pc       = ReadWrite s.execPC.val (s.execBranch_w <==)
      , operands = let (x1, x2) = p.regFile.outs
                       (r1, r2) = p.iset.getSrcs s.execInstr.val
                   in  (forward p s r1 x1, forward p s r2 x2)
      , result   = WriteOnly (s.memResult <==)
      , memReq   = WriteOnly (s.memReq <==)
      }
    s.execExpectedPC <== if s.execBranch_w.active then s.execBranch_w.val
      else s.execPC.val + fromIntegral p.iset.incPC
    p.branchPred.train s.execInstr.val s.execPC.val
                       (Option s.execBranch_w.active s.execBranch_w.val)
  -- Setup memory access stage
  when (inv s.memStall_w.val) do
    s.memActive <== fire
    s.memInstr <== s.execInstr.val

-- Stage 4: memory access
-- ======================

memAccess :: PipelineStage xlen ilen instr lregs mreq
memAccess p s = do
  -- Is memory ready for a new request?
  let isMemAccess = p.iset.isMemAccess s.memInstr.val
  let waitForMem = isMemAccess .&&. inv p.dmem.reqs.canPut
  -- Issue stall to earlier stages
  s.memStall_w <== s.memActive.val .&&. (waitForMem .||. s.wbStall_w.val)
  -- Setup writeback stage
  when (inv s.wbStall_w.val) do
    s.wbActive <== s.memActive.val .&&. inv waitForMem
    s.wbInstr <== s.memInstr.val
    s.wbResult <== s.memResult.val
    -- Issue memory request
    when (s.memActive.val .&&. isMemAccess .&&. p.dmem.reqs.canPut) do
      p.dmem.reqs.put s.memReq.val

-- Stage 5: writeback
-- ==================

writeback :: PipelineStage xlen ilen instr lregs mreq
writeback p s = do
  let rd = p.iset.getDest s.wbInstr.val
  let isMemAccess = p.iset.isMemAccess s.wbInstr.val
  -- Wait for memory response?
  let waitResp = isMemAccess .&&. rd.valid .&&. inv p.dmem.resps.canPeek
  -- Issue stall to earlier stages
  s.wbStall_w <== s.wbActive.val .&&. waitResp
  -- Update register file
  when (s.wbActive.val .&&. rd.valid .&&. inv waitResp) do
    when isMemAccess do p.dmem.resps.consume
    let result = if isMemAccess then p.dmem.resps.peek else s.wbResult.val
    p.regFile.store rd.val result

-- Data hazard detection
-- =====================

-- Is there a data hazard when reading from the given source register?
hazard p s src = src.valid .&&.
     ( s.execActive.val .&&. s.execInstr `loads` src
  .||. s.memActive.val .&&. s.memInstr `loads` src
  .||. s.wbStall_w.val .&&. s.wbInstr `loads` src )
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
  -> Module ()
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
