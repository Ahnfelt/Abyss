module Arithmetic where

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

data Path = Path Time Vector Vector Vector deriving Show
type Time = Double

getPosition :: Path -> Time -> Vector
getPosition (Path t0 a0 v0 p0) t = a0 .* (t - t0) ^ 2 .+. v0 .* (t - t0) .+. p0

getVelocity :: Path -> Time -> Vector
getVelocity (Path t0 a0 v0 _) t = a0 .* (2 * (t - t0)) .+. v0

getAcceleration :: Path -> Time -> Vector
getAcceleration (Path t0 a0 _ _) _ = a0


setAcceleration :: Vector -> Time -> Path -> Path
setAcceleration a t path = Path t a (getVelocity path t) (getPosition path t)

setVelocity :: Vector -> Time -> Path -> Path
setVelocity v t path = Path t (getAcceleration path t) v (getPosition path t)

setPosition :: Vector -> Time -> Path -> Path
setPosition p t path = Path t (getAcceleration path t) (getVelocity path t) p


staticPath :: Vector -> Path
staticPath p = Path 0 (Vector 0 0) (Vector 0 0) p


