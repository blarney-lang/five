import Blarney
import Blarney.Five

main :: IO ()
main = do
  -- Verilog
  writeVerilogModule makeVerifier "Correctness" "gen"
  writeVerilogModule makeForwardProgressVerifier "ForwardProgress" "gen"
  -- SMT
  let conf1 = dfltVerifyConf { verifyConfMode = Bounded (fixedDepth 7) }
  writeSMTScript conf1 makeVerifier "Correctness" "gen"
  let conf2 = dfltVerifyConf { verifyConfMode = Bounded (fixedDepth 50) }
  writeSMTScript conf2 makeForwardProgressVerifier "ForwardProgress" "gen"
