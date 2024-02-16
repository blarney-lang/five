module Blarney.Five.Util where

import Blarney

-- Obtain universally quantified variable for verification
var :: KnownNat n => String -> Bit n
var = inputPin

-- Base-2 logarithm with error
log2strict :: Int -> Int
log2strict x
  | 2^y == x = y
  | otherwise = error "log2: argument is not a power of 2"
  where y = log2ceil x

-- Set/reset latch interface
data SetReset =
  SetReset {
    set   :: Action ()
  , reset :: Action ()
  , val   :: Bit 1
  }

-- Set/reset latch with 0-cycle set
makeSetResetBypass = do
  setWire   <- makeWire false
  resetWire <- makeWire false
  latch     <- makeReg false

  always do
    if resetWire.val
      then latch <== false
      else when setWire.val do latch <== true

  return
    SetReset {
      set   = setWire <== true
    , reset = resetWire <== true
    , val   = setWire.val .||. latch.val
    }
