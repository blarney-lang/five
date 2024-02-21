module Blarney.Five.Verify (genSMTScripts, verify) where

import Blarney
import Blarney.Queue
import Blarney.Option
import Blarney.SourceSink
import Blarney.ClientServer
import Blarney.Five.Util
import Blarney.Five.RegMem
import Blarney.Five.RegFile
import Blarney.Five.Pipeline
import Blarney.Five.Interface
import Blarney.Five.BranchPred

-- Type parameters for verification
type V_XLen    = 3   -- Register width
type V_LogRegs = 2   -- Log_2 of number of registers

-- Number of source operands per instruction
numSrcs :: Int
numSrcs = 2

-- Enable register forwarding?
enRegFwd :: Bool
enRegFwd = True

-- Memory request type for verification
data V_MemReq =
  V_MemReq {
    -- Unique request id
    uid :: Bit V_XLen
    -- Do we expect a response from this request?
  , hasResp :: Bit 1
  }
  deriving (Generic, Bits)

-- The map-filter server consumes requests, filters out requests that
-- don't match a given predicate, applies a given function to convert a
-- request into a response, and returns back the responses.  It uses
-- universally quantified put and peek masks to capture arbitrary latency
-- and backpressure. If the masks are always true, the server is capable
-- of operating at full throughput, consuming a request and producing a
-- response on every cycle. The number of outstanding requests is
-- controlled by the size of the internal queue.
makeMapFilterServer :: (Bits req, Bits resp)
                    => Bit 1 -> Bit 1
                    -> (req -> Bit 1)
                    -> (req -> resp)
                    -> Module (Server req resp)
makeMapFilterServer putMask peekMask p f = do
  q <- makePipelineQueue 1
  let src = Source { canPeek = q.canDeq .&&. peekMask
                   , peek    = q.first
                   , consume = q.deq }
  let snk = Sink { canPut = q.notFull .&&. putMask
                 , put = \req ->
                     when (p req) do q.enq (f req) }
  return (Server snk src)

-- Instruction format for verification
data V_Instr =
  V_Instr {
    uid :: Bit V_XLen
  , rd  :: Option (Bit V_LogRegs)
  , rs1 :: Option (Bit V_LogRegs)
  , rs2 :: Option (Bit V_LogRegs)
  , isMemAccess :: Bit 1
  , canBranch :: Bit 1
  }
  deriving (Generic, Bits)

-- Instruction set interface for verification
v_instrSet execute =
  InstrSet {
    getDest       = \i -> i.rd
  , getSrcs       = \i -> [i.rs1, i.rs2]
  , isMemAccess   = \i -> i.isMemAccess
  , decode        = \uid ->
      V_Instr {
        uid = uid
      , rd  = Option (var "rd_valid")  (var "rd")
      , rs1 = Option (var "rs1_valid") (var "rs1")
      , rs2 = Option (var "rs2_valid") (var "rs2")
      , isMemAccess = var "is_mem_access"
      , canBranch = var "can_branch"
      }
  , execute     = execute
  , incPC       = 1
  }

-- Execution unit for verification
makeGoldenExecUnit :: Bit V_XLen -> Bit V_XLen -> Bool ->
  Module (V_Instr -> ExecState V_XLen V_MemReq -> Action ())
makeGoldenExecUnit initPC instrLen enAsserts = do
  goldenPC   <- makeReg initPC
  goldenRegs <- replicateM (2 ^ valueOf @V_LogRegs) (makeReg 0)
  stall <- makeWire false

  return \instr s -> do
    -- Check that correct instruction has been supplied
    when enAsserts do
      assert (instr.uid .==. s.pc.val) "Instruction correct"

    -- Issue memory request
    let req = var "req"
    when instr.isMemAccess do
      s.memReq <== V_MemReq { uid = req, hasResp = instr.rd.valid }

    -- Optionally perform a branch
    let branch = instr.canBranch .&&. var "branch_valid"
    let branchTarget = var "branch_target"
    when branch do s.pc <== branchTarget
    -- Check and maintain golden PC
    when enAsserts do
      assert (s.pc.val .==. goldenPC.val) "PC correct"
    when (inv stall.val) do
      goldenPC <== if branch then branchTarget
                     else goldenPC.val + instrLen

    -- Write result and update golden register file
    when instr.rd.valid do
      let result = var "result"
      s.result <== result
      goldenRegs ! instr.rd.val <==
        if instr.isMemAccess then req else result

    -- Check operands against golden register file
    let operandsOk =
          andList [ r.valid .==>. (goldenRegs!r.val).val .==. o
                  | (r, o) <- zip [instr.rs1, instr.rs2] s.operands ]
    when enAsserts do
      assert operandsOk "Operands correct"

-- No consecutive branch mispredictions
checkNoConsecutiveMispreds s = do
  -- The number of consecutive branch mispredictions
  mispreds :: Reg (Bit 2) <- makeReg 0

  always do
    when (s.execActive.val .&&. inv s.execStall_w.val) do
      if s.execMispredict.val
        then mispreds <== mispreds.val + 1
        else mispreds <== 0
    assert (mispreds.val .<=. 1)
           "No consecutive branch mispredictions"

-- Check that at least n instructions can be retired within given
-- time bound t.
checkForwardProgress n t s = do
  -- How many instructions have been committed?
  retired :: Reg (Bit 8) <- makeReg 0

  -- Time
  time :: Reg (Bit 8) <- makeReg 0

  always do
    time <== time.val + 1
    when s.wbActive.val do
      retired <== retired.val + 1
    assert (time.val .>=. t .==>. retired.val .>=. fromInteger n)
           ("Forward progress " ++ show n)

-- Pipeline for correctness verification
makeCorrectnessVerifier :: Module ()
makeCorrectnessVerifier = mdo
  imem <- makeMapFilterServer (var "imem_put_mask")
                              (var "imem_peek_mask")
                              (const true) id
  dmem <- makeMapFilterServer (var "dmem_put_mask")
                              (var "dmem_peek_mask")
                              (.hasResp) (.uid)
  rmem <- if enRegFwd then makeForwardingRegMem numSrcs
                      else makeRegMem numSrcs
  bpred <- makeArbitraryPredictor iset s
  exec  <- makeGoldenExecUnit 0 1 True
  let iset = v_instrSet exec
  rf    <- if enRegFwd then makeForwardingRegFile rmem iset s
                       else makeBasicRegFile rmem iset s
  let ifc = 
        PipelineInterface {
          imem           = imem
        , dmem           = dmem
        , branchPred     = bpred
        , regFile        = rf
        }
  s <- makePipelineState 0
  makePipeline ifc iset s
  checkNoConsecutiveMispreds s

-- Pipeline for forward progress verification
makeForwardProgressVerifier :: Int -> Int -> Module ()
makeForwardProgressVerifier n d = mdo
  imem  <- makeMapFilterServer true true (const true) id
  dmem  <- makeMapFilterServer true true (.hasResp) (.uid)
  bpred <- makeArbitraryPredictor iset s
  rmem  <- if enRegFwd then makeForwardingRegMem numSrcs
                       else makeRegMem numSrcs
  exec  <- makeGoldenExecUnit 0 1 False
  let iset = v_instrSet exec
  rf    <- if enRegFwd then makeForwardingRegFile rmem iset s
                       else makeBasicRegFile rmem iset s
  let ifc =
        PipelineInterface {
          imem           = imem
        , dmem           = dmem
        , branchPred     = bpred
        , regFile        = rf
        }
  s <- makePipelineState 0
  makePipeline ifc iset s
  checkForwardProgress (fromIntegral n) (fromIntegral d) s

-- Max cycles to retire 1 instruction
-- (For forward progress checking)
maxCyclesToRetire1 = 8

-- Max cycles to retire 2 instructions
-- (For forward progress checking)
maxCyclesToRetire2 = 13

-- Generate SMT scripts for verification
genSMTScripts :: IO ()
genSMTScripts = do
  let d    = 7
  let conf = dfltVerifyConf {
               verifyConfMode = Bounded (fixedDepth d)
             }
  writeSMTScript conf makeCorrectnessVerifier "Correctness" "SMT"

  let cycs = maxCyclesToRetire1
  let conf = dfltVerifyConf { verifyConfMode = Bounded (fixedDepth (cycs+1)) }
  writeSMTScript conf (makeForwardProgressVerifier 1 cycs)
                 "ForwardProgress1" "SMT"

  let cycs = maxCyclesToRetire2
  let conf = dfltVerifyConf { verifyConfMode = Bounded (fixedDepth (cycs+1)) }
  writeSMTScript conf (makeForwardProgressVerifier 2 cycs)
                 "ForwardProgress2" "SMT"

-- Lauch SMT solver and verify. This is the recommended flow (it's more
-- efficient) but not the default as it assumes an SMT solver is available.
verify :: IO ()
verify = do
  let d    = 7
  let conf = dfltVerifyConf {
               verifyConfMode = Bounded (Range 1 d)
             }
  verifyWith conf makeCorrectnessVerifier

  let cycs = maxCyclesToRetire1
  let conf = dfltVerifyConf { verifyConfMode = Bounded (fixedDepth (cycs+1)) }
  verifyWith conf (makeForwardProgressVerifier 1 cycs)

  let cycs = maxCyclesToRetire2
  let conf = dfltVerifyConf { verifyConfMode = Bounded (fixedDepth (cycs+1)) }
  verifyWith conf (makeForwardProgressVerifier 2 cycs)
