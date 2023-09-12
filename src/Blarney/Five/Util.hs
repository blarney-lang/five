module Blarney.Five.Util where

import Blarney

var :: KnownNat n => String -> Bit n
var = inputPin
