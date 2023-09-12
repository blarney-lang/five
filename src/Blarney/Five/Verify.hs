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
makeIdentityServer :: Bits a => String -> Module (Server a a)
makeIdentityServer name = do
  q <- makePipelineQueue 1
  return
    Server {
      reqs  = limitSink putMask (toSink q)
    , resps = limitSource peekMask (toSource q)
    }
  where
    putMask  = var (name ++ "_put_mask")
    peekMask = var (name ++ "_peek_mask")

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
v_instrSet initPC instrLen =
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
  , makeExecUnit = makeGoldenExecUnit initPC instrLen
  }

-- Execution unit for verification
makeGoldenExecUnit :: Bit V_XLen -> Bit V_XLen ->
  Module (ExecUnit V_XLen V_Instr (Bit V_XLen))
makeGoldenExecUnit initPC instrLen = do
  goldenPC   <- makeReg initPC
  goldenRegs <- replicateM (2 ^ valueOf @V_LogRegs) (makeReg 0)
  stall <- makeWire false

  return
    ExecUnit {
      issue = \instr s -> do
        -- Check that correct instruction has been supplied
        assert (instr.uid .==. s.pc.val) "Instruction correct"

        -- Issue memory request
        let req = var "req"
        when instr.isMemAccess do
          s.memReq <== req

        -- Optionally perform a branch
        let branch = Option (var "branch_valid") (var "branch")
        when branch.valid do s.pc <== branch.val
        -- Check and maintain golden PC
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
        assert operandsOk "Operands correct"
    }

-- Construct pipeline for verification
makePipelineVerifier :: Module ()
makePipelineVerifier = do
  imem <- makeIdentityServer "imem"
  dmem <- makeIdentityServer "dmem"
  let instrSet = v_instrSet 0 1
  makeClassic
    PipelineParams {
      initPC         = 0
    , instrLen       = 1
    , imem           = imem
    , dmem           = dmem
    , instrSet       = instrSet
    , makeBranchPred = makeNaivePredictor 1
    , makeRegFile    = makeBasicRegFile instrSet
    }
  return ()

verify :: IO ()
verify = do
    writeSMTScript conf makePipelineVerifier
                   "Verifier" "Verifier-SMT"
  where
    conf = dfltVerifyConf { verifyConfMode = Bounded (fixedDepth 5) }
