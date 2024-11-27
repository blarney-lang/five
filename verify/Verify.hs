import Blarney
import Blarney.Five

main :: IO ()
main = do
  let conf1 = dfltVerifyConf { verifyConfMode = Bounded (Range 1 10) }
  verifyWith conf1 makeVerifier
  let conf2 = dfltVerifyConf { verifyConfMode = Bounded (Range 1 50) }
  verifyWith conf2 makeForwardProgressVerifier
