module Blarney.Five.Interface where

-- Blarney imports
import Blarney
import Blarney.Option
import Blarney.SourceSink
import Blarney.ClientServer

-- Local imports
import Blarney.Five.Util

-- Interface between instruction set and pipeline
-- ==============================================

-- Information about the instruction set provided to the pipeline,
-- parameterised by the register size in bits (xlen), instruction size in
-- bits (ilen), the decoded instruction format (instr), number of
-- registers (2^lregs), and the data memory request format (mreq).
data InstrSet xlen ilen instr lregs mreq =
  InstrSet {
    -- Instruction decoder
    decode :: Bit ilen -> instr
    -- Extract source/destination registers from instruction
  , getDest :: instr -> Option (Bit lregs)
  , getSrcs :: instr -> [Option (Bit lregs)]
    -- Max number of source operands per instruction
  , numSrcs :: Int
    -- Will instruction issue a data memory request?
  , isMemAccess :: instr -> Bit 1
    -- Can instruction branch?
  , canBranch :: instr -> Bit 1
    -- Function to execute a given instruction
  , execute :: instr -> ExecState xlen mreq -> Action ()
  }

-- State that can be read/written by an instruction
data ExecState xlen mreq =
  ExecState {
    -- Program counter (can be read and written)
    pc :: ReadWrite (Bit xlen)
    -- Instruction operands from the register file (read-only)
  , operands :: [Bit xlen]
    -- Instruction result (write-only)
  , result :: WriteOnly (Bit xlen)
    -- Memory request (write-only)
  , memReq :: WriteOnly mreq
  }

-- Pipeline parameters and I/O
-- ===========================

-- Pipeline parameters and memory interfaces
data PipelineParams xlen ilen instr lregs mreq =
  PipelineParams {
    -- Initial PC
    initPC :: Bit xlen
    -- Instruction size in bytes
  , instrLen :: Bit xlen
    -- Interfaces to instruction and data memories
  , imem :: Server (Bit xlen) (Bit ilen)
  , dmem :: Server mreq (Bit xlen)
    -- Instruction set definition
  , instrSet :: InstrSet xlen ilen instr lregs mreq
    -- Interface to branch predictor
  , branchPred :: BranchPred xlen
    -- Interface to register file
  , regFile :: RF xlen instr
  }

-- Branch target predictor
-- =======================

-- Interface to the branch target predictor
data BranchPred xlen =
  BranchPred {
    -- Given the PC of the instruction currently being fetched,
    -- predict the PC of the next instruction to fetch.
    predict :: Bit xlen -> Action ()
    -- The prediction is available one cycle after calling predict and
    -- will remain stable until predict is called again.
  , out :: Bit xlen
  }

-- Register file
-- =============

-- Interface to the register file
data RF xlen instr =
  RF {
    -- Submit instruction to the register file.
    submit :: instr -> Action ()
    -- The instruction operands are available one cycle after
    -- submission and will remain stable until submit is called again.
  , operands :: [Bit xlen]
    -- The register file can request a pipeline stall (on the same
    -- cycle as an instruction submission) if the submitted
    -- instruction's operands are not yet available.
  , stall :: Bit 1
  }

-- Pipeline state
-- ==============

-- Pipeline state parameterised by register size in bits (xlen)
-- and decoded instruction format (instr).
data PipelineState xlen instr =
  PipelineState {
    -- Is the decode stage active?
    decActive :: Reg (Bit 1)
    -- If so, the PC of the instruction in the decode stage.
  , decPC :: Reg (Bit xlen)
    -- Is the decode stage stalling?
  , decStall :: Wire (Bit 1)
    -- Is the execute stage active?
  , execActive :: Reg (Bit 1)
    -- If so, the instruction to execute with its PC and operands.
  , execInstr :: Reg instr
  , execPC :: Reg (Bit xlen)
  , execOperands :: [Wire (Bit xlen)]
    -- The PC of the next instruction expected in the execute stage.
  , execExpectedPC :: Reg (Bit xlen)
    -- Is the instruction in the execute stage mispredicted?
  , execMispredict :: SetReset
    -- Is the execute stage stalling?
  , execStall :: Wire (Bit 1)
    -- Instruction result
  , execResult :: Wire (Bit xlen)
    -- Branch target of executed instruction
  , execBranch :: Wire (Bit xlen)
    -- Is the memory access stage active?
  , memActive :: Reg (Bit 1)
    -- If so, the instruction and its result
  , memInstr :: Reg instr
  , memResult :: Reg (Bit xlen)
    -- Is the memory access stage stalling?
  , memStall :: Wire (Bit 1)
    -- Is the writeback stage active?
  , wbActive :: Reg (Bit 1)
    -- If so, the instruction and its result.
  , wbInstr :: Reg instr
  , wbResult :: Reg (Bit xlen)
  }
