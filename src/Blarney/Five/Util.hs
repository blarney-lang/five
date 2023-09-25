module Blarney.Five.Util where

import Blarney

-- Obtain universally quantified variable for verification
var :: KnownNat n => String -> Bit n
var = inputPin
