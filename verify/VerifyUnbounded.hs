import Blarney
import Blarney.Five
import Blarney.Backend.NewSMT

main :: IO ()
main = do
  checkAuto Verbose makeVerifier
  checkAuto Verbose makeForwardProgressVerifier
