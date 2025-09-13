{-
SBV: SMT Based Verification in Haskell

Copyright (c) 2010-2025, Levent Erkok (erkokl@gmail.com)
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the developer (Levent Erkok) nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL LEVENT ERKOK BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

module ShowTDiff where

import Data.List (intercalate)
import Data.Ratio
import Data.Time.Clock
import GHC.Real (Ratio((:%)))
import Numeric (showFFloat)

-- | Show 'NominalDiffTime' in human readable form. 'NominalDiffTime' is
-- essentially picoseconds (10^-12 seconds). We show it so that
-- it's represented at the day:hour:minute:second.XXX granularity.
showTDiff :: NominalDiffTime -> String
showTDiff diff
   | denom /= 1    -- Should never happen! But just in case.
   = show diff
   | True
   = intercalate ":" fields
   where total, denom :: Integer
         total :% denom = (picoFactor % 1) * toRational diff

         -- there are 10^12 pico-seconds in a second
         picoFactor :: Integer
         picoFactor = (10 :: Integer) ^ (12 :: Integer)

         (s2p, m2s, h2m, d2h) = case drop 1 $ scanl (*) 1 [picoFactor, 60, 60, 24] of
                                  (s2pv : m2sv : h2mv : d2hv : _) -> (s2pv, m2sv, h2mv, d2hv)
                                  _                               -> (0, 0, 0, 0)  -- won't ever happen

         (days,    days')    = total    `divMod` d2h
         (hours,   hours')   = days'    `divMod` h2m
         (minutes, seconds') = hours'   `divMod` m2s
         (seconds, picos)    = seconds' `divMod` s2p
         secondsPicos        =  show seconds
                             ++ dropWhile (/= '.') (showFFloat (Just 3) (fromIntegral picos * (10**(-12) :: Double)) "s")

         aboveSeconds = map (\(t, v) -> show v ++ [t]) $ dropWhile (\p -> snd p == 0) [('d', days), ('h', hours), ('m', minutes)]
         fields       = aboveSeconds ++ [secondsPicos]
