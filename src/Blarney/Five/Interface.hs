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
  , getSrcs :: instr -> (Option (Bit lregs), Option (Bit lregs))
    -- Will instruction issue a data memory request?
  , isMemAccess :: instr -> Bit 1
    -- Function to execute a given instruction
  , execute :: instr -> ExecState xlen mreq -> Action ()
    -- Program counter increment
  , incPC :: Int
    -- For branch predictor: can given instruction branch?
  , canBranch :: instr -> Bit 1
  }

-- State that can be read/written by an instruction
data ExecState xlen mreq =
  ExecState {
    -- Program counter (can be read and written)
    pc :: ReadWrite (Bit xlen)
    -- Instruction operands from the register file (read-only)
  , operands :: (Bit xlen, Bit xlen)
    -- Instruction result (write-only)
  , result :: WriteOnly (Bit xlen)
    -- Memory request (write-only)
  , memReq :: WriteOnly mreq
  }

-- Pipeline parameters
-- ===================

-- Pipeline parameters
data PipelineParams xlen ilen instr lregs mreq =
  PipelineParams {
    -- Information about the instruction set
    iset :: InstrSet xlen ilen instr lregs mreq
    -- Interfaces to instruction and data memories
  , imem :: Server (Bit xlen) (Bit ilen)
  , dmem :: Server mreq (Bit xlen)
    -- Interface to register file and branch predictor
  , regFile :: RegisterFile lregs xlen
  , branchPred :: BranchPredictor xlen instr
  }

-- Register file abstraction, parameterised by the number of
-- registers (2^lregs) and the register size (xlen).
data RegisterFile lregs xlen =
  RegisterFile {
    -- Load each of the given operands
    load  :: (Bit lregs, Bit lregs) -> Action ()
    -- Values loaded, valid one cycle after call to load and preserved
    -- until load is called against.
  , outs  :: (Bit xlen, Bit xlen)
    -- Overwrite value of given register with given value.
  , store :: Bit lregs -> Bit xlen -> Action ()
  }

-- Branch predictor interface
data BranchPredictor xlen instr =
  BranchPredictor {
    -- Given the PC of the instruction currently being fetched,
    -- predict the PC of the next instruction to fetch.
    predict :: Bit xlen -> Action ()
    -- The prediction is available one cycle after calling predict and
    -- will remain stable until predict is called again.
  , val :: Bit xlen
    -- Inform the predictor about control-flow behaviour, including an
    -- instruction, its address, and the branch target address (if
    -- indeed a branch was taken)
  , train :: instr -> Bit xlen -> Option (Bit xlen) -> Action ()
  }
