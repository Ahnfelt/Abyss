module Floating ((/), acos, sqrt, infinite, minusInfinite) where

import Prelude hiding ((/), acos, sqrt)
import qualified Prelude as P

-- exception safe operations

unsafe :: Double -> Bool
unsafe r = isInfinite r || isNaN r

(/) :: Double -> Double -> Double
a / b = let r = a P./ b in if unsafe r then error (show a ++ " / " ++ show b) else r

acos :: Double -> Double
acos a = let r = P.acos a in if unsafe r then error ("acos " ++ show a) else r

sqrt :: Double -> Double
sqrt a = let r = P.sqrt a in if unsafe r then error ("sqrt " ++ show a) else r

infinite :: Double
infinite =     
    let highestExponent = snd $ floatRange (undefined :: Double)
    in 2^highestExponent

minusInfinite :: Double
minusInfinite =     
    let highestExponent = snd $ floatRange (undefined :: Double)
    in -2^highestExponent


