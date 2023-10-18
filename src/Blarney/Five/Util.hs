module Blarney.Five.Util where

import Blarney

-- Obtain universally quantified variable for verification
var :: KnownNat n => String -> Bit n
var = inputPin

-- Set/reset latch interface
data SetReset =
  SetReset {
    set   :: Action ()
  , reset :: Action ()
  , val   :: Bit 1
  }

-- Set-dominant set/reset latch with 0-cycle set
makeSetResetBypass = do
  setWire   <- makeWire false
  resetWire <- makeWire false
  latch     <- makeReg false

  always do
    if setWire.val
      then latch <== true
      else when resetWire.val do latch <== false

  return
    SetReset {
      set   = setWire <== true
    , reset = resetWire <== true
    , val   = setWire.val .||. latch.val
    }
