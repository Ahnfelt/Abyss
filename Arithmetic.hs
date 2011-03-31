module Arithmetic where

import Floating
import Prelude hiding ((/), acos, sqrt)

data Vector = Vector Double Double deriving (Eq, Show)

infixl 4 ~~
infixl 4 .~~.
infixl 6 .+.
infixl 6 .-.
infixl 7 .*

Vector x1 y1 .+. Vector x2 y2 = Vector (x1 + x2) (y1 + y2)
Vector x1 y1 .-. Vector x2 y2 = Vector (x1 - x2) (y1 - y2)
Vector x y .* scale = Vector (x * scale) (y * scale)
Vector x1 y1 .~~. Vector x2 y2 = x1 ~~ x2 && y1 ~~ y2
x ~~ y = abs (x - y) < 0.01

distance :: Vector -> Vector -> Double
distance a b = let Vector x y = b .-. a in sqrt (x*x + y*y)

-- | Second degree vector polynomial. The first parameter specifies the time for the 
-- initial position and velocity. The second paremeter is the acceleration, the third
-- the initial velocity and the fourth the initial position.
data Path = Path Time Vector Vector Vector deriving Show
type Time = Double

getPosition :: Path -> Time -> Vector
getPosition (Path t0 a0 v0 p0) t = a0 .* (t - t0) ^ 2 .+. v0 .* (t - t0) .+. p0

getVelocity :: Path -> Time -> Vector
getVelocity (Path t0 a0 v0 _) t = a0 .* (2 * (t - t0)) .+. v0

getAcceleration :: Path -> Time -> Vector
getAcceleration (Path _ a0 _ _) _ = a0

getInitialTime :: Path -> Time
getInitialTime (Path t0 _ _ _) = t0

setAcceleration :: Vector -> Time -> Path -> Path
setAcceleration a t path = Path t a (getVelocity path t) (getPosition path t)

setVelocity :: Vector -> Time -> Path -> Path
setVelocity v t path = Path t (getAcceleration path t) v (getPosition path t)

setPosition :: Vector -> Time -> Path -> Path
setPosition p t path = Path t (getAcceleration path t) (getVelocity path t) p

setInitialTime :: Time -> Path -> Path
setInitialTime t path = Path t (getAcceleration path t) (getVelocity path t) (getPosition path t) 

normalizePath :: Path -> Path
normalizePath = setInitialTime 0

staticPath :: Vector -> Path
staticPath p = Path 0 (Vector 0 0) (Vector 0 0) p

substractPaths :: Path -> Path -> Path
substractPaths path1 path2 = 
    let (Path _ a1 v1 p1) = normalizePath path1 in
    let (Path _ a2 v2 p2) = normalizePath path2 in
    Path 0 (a1 .-. a2) (v1 .-. v2) (p1 .-. p2)
    

data Polynomial = Polynomial Time Double Double Double deriving Show

polynomialX :: Path -> Polynomial
polynomialX (Path t (Vector ax _) (Vector vx _) (Vector px _)) = 
    Polynomial t ax vx px

polynomialY :: Path -> Polynomial
polynomialY (Path t (Vector _ ay) (Vector _ vy) (Vector _ py)) = 
    Polynomial t ay vy py

-- | Remove paths that ends before @time@
pathsFrom :: Time -> [Path] -> [Path]
pathsFrom _ [] = []
pathsFrom _ [path] = [path]
pathsFrom time (path1 : path2 : paths) = 
    if (time < getInitialTime path2) then (path1 : path2 : paths)
    else pathsFrom time (path2 : paths)

expirations :: [Path] -> [Time]
expirations (_ : paths) = map getInitialTime (paths ++ [Path infinit undefined undefined undefined])







