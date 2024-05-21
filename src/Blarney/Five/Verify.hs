module Blarney.Five.Verify (genSMTScripts, verify, verifyUnbounded) where

import Data.Proxy

import Blarney
import Blarney.Queue
import Blarney.Option
import Blarney.SourceSink
import Blarney.ClientServer
import Blarney.Backend.NewSMT
import Blarney.Five.Util
import Blarney.Five.Pipeline
import Blarney.Five.Interface
import Blarney.Five.RegisterFile
import Blarney.Five.BranchPredictor

-- Type parameters for verification
type XLen = 3     -- Three-bit registers
type ILen = XLen  -- Instruction width same as register width
type LogRegs = 2  -- Four registers in total

-- Decoded instructions hold the raw (encoded) instruction,
-- the operands, a memory-access bit, and a can-branch bit
data Instr = Instr {
    raw :: Bit ILen
  , rd :: Option (Bit LogRegs)
  , rs1 :: Option (Bit LogRegs)
  , rs2 :: Option (Bit LogRegs)
  , isMemAccess :: Bit 1
  , canBranch :: Bit 1
  }
  deriving (Generic, Bits)

-- Data memory requests have a value and may expect a response
data MReq = MReq { val :: Bit XLen, hasResp :: Bit 1 }
  deriving (Generic, Bits)

-- An "instruction set" that checks pipeline properties
makeChecker :: Bool -> Module (InstrSet XLen ILen Instr LogRegs MReq)
makeChecker enAsserts = do
  goldenPC <- makeReg 0
  goldenRegs <- replicateM (2 ^ valueOf @LogRegs) (makeReg 0)
  return
    InstrSet {
      decode = \raw ->
        Instr {
          raw = raw
        , rd  = Option (var "rd_valid")  (var "rd")
        , rs1 = Option (var "rs1_valid") (var "rs1")
        , rs2 = Option (var "rs2_valid") (var "rs2")
        , isMemAccess = var "is_mem_access"
        , canBranch = var "can_branch"
        }
    , getDest = \i -> i.rd
    , getSrcs = \i -> (i.rs1, i.rs2)
    , execute = \i s -> do
        -- Check raw instruction matches the PC
        when enAsserts do
          assert (i.raw .==. s.pc.val) "Instruction correct"
        -- Optionally perform a branch
        let branchValid = i.canBranch .&&. var "branch_valid"
        let branchTarget = var "branch_target"
        when branchValid do s.pc <== branchTarget
        -- Maintain golden PC and check against it
        goldenPC <== if branchValid then branchTarget
                       else goldenPC.val + 1
        when enAsserts do
          assert (s.pc.val .==. goldenPC.val) "PC correct"
        -- Issue memory request
        let mreq = MReq { val = var "mreq_val", hasResp = i.rd.valid }
        when i.isMemAccess do s.memReq <== mreq
        -- Write result and update golden register file
        when i.rd.valid do
          let result = var "result"
          s.result <== result
          goldenRegs!i.rd.val <==
            if i.isMemAccess then mreq.val else result
        -- Check operands against golden register file
        let check r x = r.valid .==>. (goldenRegs!r.val).val .==. x
        when enAsserts do
          assert (check i.rs1 (fst s.operands) .&&.
                  check i.rs2 (snd s.operands)) "Operands correct"
    , isMemAccess = \i -> i.isMemAccess
    , incPC = 1
    , canBranch = \i -> i.canBranch
    }
           
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

-- Check that at an instruction is retired every n cycles at least
checkForwardProgress n s = do
  liftNat (log2ceil n + 1) $ \(_ :: Proxy w) -> do

    -- Cycles since last retired instruction
    count :: Reg (Bit w) <- makeReg 0

    always do
      if s.wbActive.val
        then count <== 0
        else count <== count.val + 1
      assert (count.val .<=. fromIntegral n) "Forward progress"

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

-- Pipeline for correctness verification
makeCorrectnessVerifier :: Module ()
makeCorrectnessVerifier = mdo
  imem <- makeMapFilterServer (var "imem_put_mask")
                              (var "imem_peek_mask")
                              (const true) id
  dmem <- makeMapFilterServer (var "dmem_put_mask")
                              (var "dmem_peek_mask")
                              (.hasResp) (.val)
  iset <- makeChecker True
  branchPred <- makeArbitraryPredictor iset
  regFile <- makeRegisterFile
  let params = 
        PipelineParams {
          iset             = iset
        , imem             = imem
        , dmem             = dmem
        , regFile          = regFile
        , branchPred       = branchPred
        }
  s <- makePipeline params
  checkNoConsecutiveMispreds s

-- Pipeline for forward progress verification
makeForwardProgressVerifier :: Int -> Module ()
makeForwardProgressVerifier n = mdo
  let maxExtraLatency = 1 :: Bit 2
  imask <- makeLowForAtMost maxExtraLatency "imem_peek_mask"
  dmask <- makeLowForAtMost maxExtraLatency "dmem_peek_mask"
  imem <- makeMapFilterServer true imask (const true) id
  dmem <- makeMapFilterServer true dmask (.hasResp) (.val)
  iset <- makeChecker False
  branchPred <- makeArbitraryPredictor iset
  regFile <- makeRegisterFile
  let params =
        PipelineParams {
          iset             = iset
        , imem             = imem
        , dmem             = dmem
        , regFile          = regFile
        , branchPred       = branchPred
        }
  s <- makePipeline params
  checkForwardProgress n s

-- Max cycles to retire an instruction
-- (For forward progress checking)
maxCyclesToRetire = 10

-- Generate SMT scripts for verification
genSMTScripts :: IO ()
genSMTScripts = do
  let d    = 7
  let conf = dfltVerifyConf {
               verifyConfMode = Bounded (fixedDepth d)
             }
  writeSMTScript conf makeCorrectnessVerifier "Correctness" "SMT"

  let d = 2 * maxCyclesToRetire + 2
  let conf = dfltVerifyConf { verifyConfMode = Bounded (fixedDepth d) }
  writeSMTScript conf (makeForwardProgressVerifier maxCyclesToRetire)
                 "ForwardProgress" "SMT"

-- Lauch SMT solver and verify. This is the recommended flow (it's more
-- efficient) but not the default as it assumes an SMT solver is available.
verify :: IO ()
verify = do
  let d    = 7
  let conf = dfltVerifyConf {
               verifyConfMode = Bounded (Range 1 d)
             }
  verifyWith conf makeCorrectnessVerifier

  let d = 2 * maxCyclesToRetire + 2
  let conf = dfltVerifyConf { verifyConfMode = Bounded (fixedDepth d) }
  verifyWith conf (makeForwardProgressVerifier maxCyclesToRetire)

-- Lauch SMT solver and verify unbounded using Blarney's new SMT backend.
verifyUnbounded :: IO ()
verifyUnbounded = do
  checkAuto Verbose makeCorrectnessVerifier
  checkAuto Verbose (makeForwardProgressVerifier maxCyclesToRetire)
