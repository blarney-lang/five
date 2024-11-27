import Blarney
import Blarney.Five

main :: IO ()
main = do
  let conf = dfltVerifyConf { verifyConfMode = Bounded (Range 1 12) }
  verifyWith conf makeVerifier
