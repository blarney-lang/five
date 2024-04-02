module Blarney.Five.Util where

import Blarney

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

-- Returns a universally quantified 1-bit vector that is low for at
-- most n consecutive clock cycles
makeLowForAtMost :: KnownNat n => Bit n -> String -> Module (Bit 1)
makeLowForAtMost n name = do
  let v = var name
  count <- makeReg 0
  output <- makeWire v
  always do
    if v
      then count <== 0
      else if count.val .==. n
             then do
               count <== 0
               output <== true
             else do
               count <== count.val + 1
  return output.val
