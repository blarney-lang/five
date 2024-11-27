import Blarney
import Blarney.Five

main :: IO ()
main = writeVerilogModule makeVerifier "Correctness" "gen"
