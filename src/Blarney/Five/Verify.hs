module Blarney.Five.Verify where

import Blarney
import Blarney.Queue
import Blarney.Option
import Blarney.SourceSink
import Blarney.ClientServer
import Blarney.Five.Util
import Blarney.Five.RegFile
import Blarney.Five.Pipeline
import Blarney.Five.Interface
import Blarney.Five.BranchPred

-- Type parameters for verification
type V_XLen    = 4   -- Register width
type V_ILen    = 4   -- Instruction width
type V_LogRegs = 3   -- Log_2 of number of registers

-- The identity server simply consumes requests and sends them back again
-- as responses. It uses universally quantified put and peek masks to
-- capture arbitrary latency and backpressure. If the masks are always
-- true, the server is capable of operating at full throughput, consuming
-- a request and producing a response on every cycle. The number of
-- outstanding requests is controlled by the size of the internal queue.
makeIdentityServer :: Bits a => Bit 1 -> Bit 1 -> Module (Server a a)
makeIdentityServer putMask peekMask = do
  q <- makePipelineQueue 1
  return
    Server {
      reqs  = limitSink putMask (toSink q)
    , resps = limitSource peekMask (toSource q)
    }

limitSink :: Bit 1 -> Sink a -> Sink a
limitSink putMask s = s { canPut = s.canPut .&&. putMask }

limitSource :: Bit 1 -> Source a -> Source a
limitSource peekMask s = s { canPeek = s.canPeek .&&. peekMask }

-- Instruction format for verification
data V_Instr =
  V_Instr {
    uid :: Bit V_XLen
  , rd  :: Option (Bit V_LogRegs)
  , rs1 :: Option (Bit V_LogRegs)
  , rs2 :: Option (Bit V_LogRegs)
  , isMemAccess :: Bit 1
  }
  deriving (Generic, Bits)

-- Instruction set interface for verification
v_instrSet initPC instrLen enAsserts =
  InstrSet {
    numRegs     = 2 ^ valueOf @V_LogRegs
  , numSrcs     = 2
  , getDest     = \i -> i.rd
  , getSrcs     = \i -> [i.rs1, i.rs2]
  , isMemAccess = \i -> i.isMemAccess
  , decode      = \uid ->
      V_Instr {
        uid = uid
      , rd  = Option (var "rd_valid")  (var "rd")
      , rs1 = Option (var "rs1_valid") (var "rs1")
      , rs2 = Option (var "rs2_valid") (var "rs2")
      , isMemAccess = var "is_mem_access"
      }
  , makeExecUnit = makeGoldenExecUnit initPC instrLen enAsserts
  }

-- Execution unit for verification
makeGoldenExecUnit :: Bit V_XLen -> Bit V_XLen -> Bool ->
  Module (ExecUnit V_XLen V_Instr (Bit V_XLen))
makeGoldenExecUnit initPC instrLen enAsserts = do
  goldenPC   <- makeReg initPC
  goldenRegs <- replicateM (2 ^ valueOf @V_LogRegs) (makeReg 0)
  stall <- makeWire false

  return
    ExecUnit {
      issue = \instr s -> do
        -- Check that correct instruction has been supplied
        when enAsserts do
          assert (instr.uid .==. s.pc.val) "Instruction correct"

        -- Issue memory request
        let req = var "req"
        when instr.isMemAccess do
          s.memReq <== req

        -- Optionally perform a branch
        let branch = Option (var "branch_valid") (var "branch")
        when branch.valid do s.pc <== branch.val
        -- Check and maintain golden PC
        when enAsserts do
          assert (s.pc.val .==. goldenPC.val) "PC correct"
        when (inv stall.val) do
          goldenPC <== if branch.valid then branch.val
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
    }

-- Bounded number of consecutive branch mispredictions
checkMispredsBounded s = do
  -- The number of consecutive branch mispredictions
  mispreds :: Reg (Bit 2) <- makeReg 0

  always do
    when (s.execActive.val .&&. inv s.execStall.val) do
      if s.execMispredict.val
        then mispreds <== mispreds.val + 1
        else mispreds <== 0
    assert (mispreds.val .<=. 2)
           "Max of two consecutive branch mispredictions"

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
    assert (time.val .>=. t .==>. retired.val .>=. n) "Forward progress"

-- Pipeline for correctness verification
makeCorrectnessVerifier :: Module ()
makeCorrectnessVerifier = do
  imem <- makeIdentityServer (var "imem_put_mask")
                             (var "imem_peek_mask")
  dmem <- makeIdentityServer (var "dmem_put_mask")
                             (var "dmem_peek_mask")
  let instrSet = v_instrSet 0 1 True
  s <- makeClassic
    PipelineParams {
      initPC         = 0
    , instrLen       = 1
    , imem           = imem
    , dmem           = dmem
    , instrSet       = instrSet
    , makeBranchPred = makeArbitraryPredictor 1
    , makeRegFile    = makeBasicRegFile instrSet
    }
  checkMispredsBounded s

-- Pipeline for forward progress verification
makeForwardProgressVerifier :: Int -> Module ()
makeForwardProgressVerifier d = do
    imem <- makeIdentityServer true true
    dmem <- makeIdentityServer true true
    let instrSet = v_instrSet 0 1 False
    s <- makeClassic
      PipelineParams {
        initPC         = 0
      , instrLen       = 1
      , imem           = imem
      , dmem           = dmem
      , instrSet       = instrSet
      , makeBranchPred = makeArbitraryPredictor 1
      , makeRegFile    = makeBasicRegFile instrSet
      }
    checkForwardProgress 1 (fromIntegral d) s

-- Generate SMT scripts for verification
verify :: IO ()
verify = do
  let d    = 5
  let conf = dfltVerifyConf { verifyConfMode = Bounded (fixedDepth d) }
  writeSMTScript conf makeCorrectnessVerifier "Correctness" "SMT"

  let d    = 10
  let conf = dfltVerifyConf { verifyConfMode = Bounded (fixedDepth d) }
  writeSMTScript conf (makeForwardProgressVerifier (d-1))
                 "ForwardProgress" "SMT"
