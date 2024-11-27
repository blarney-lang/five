import Blarney
import Blarney.Five
import Blarney.Backend.NewSMT

main :: IO ()
main = checkAuto Verbose makeVerifier
