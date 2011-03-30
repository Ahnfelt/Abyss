module Arithmetic where

import Floating
import Prelude hiding ((/), acos, sqrt)
import Control.Monad

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
getAcceleration (Path t0 a0 _ _) _ = a0

setAcceleration :: Vector -> Time -> Path -> Path
setAcceleration a t path = Path t a (getVelocity path t) (getPosition path t)

setVelocity :: Vector -> Time -> Path -> Path
setVelocity v t path = Path t (getAcceleration path t) v (getPosition path t)

setPosition :: Vector -> Time -> Path -> Path
setPosition p t path = Path t (getAcceleration path t) (getVelocity path t) p

setInitialTime :: Time -> Path -> Path
setInitialTime t p@(Path t0 a0 v0 p0) = Path t (getAcceleration p t) (getVelocity p t) (getPosition p t) 

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


-- Collisions
type Interval = (Double, Double)

collision :: Path -> Interval -> Path -> Interval -> Maybe Time 
collision path1 (height1, width1) path2 (height2, width2) = do
    let path = substractPaths path1 path2
    (collisionX1, collisionX2M) <- solveWithMargin' ((width1 + width2) / 2) (polynomialX path)
    (collisionY1, collisionY2M) <- solveWithMargin' ((height1 + height2) / 2) (polynomialY path)
    liftM fst $ intersection collisionX1 collisionY1 `mplus` do
    	collisionX2 <- collisionX2M
    	intersection collisionX2 collisionY1 `mplus` do
	    	collisionY2 <- collisionY2M
	    	intersection collisionX1 collisionY2 `mplus` do
		    	intersection collisionX2 collisionY2
	where
		solveWithMargin' :: Double -> Polynomial -> Maybe (Interval, Maybe Interval)
		solveWithMargin' margin (Polynomial _ a v p) = solveWithMargin margin a v p

intersection :: Interval -> Interval -> Maybe Interval
intersection (a1, a2) (b1, b2) = do
	let c1 = max a1 b1
	let c2 = min a2 b2
	guard (c1 <= c2)
	return (c1, c2)
		
solveWithMargin :: Double -> Double -> Double -> Double -> Maybe (Interval, Maybe Interval)
solveWithMargin margin a b c = do
    margin <- return $ signum a * margin
    (upper1, Just upper2) <- solveQuadratic a b (c - margin)
    case solveQuadratic a b (c + margin) of 
        Just (lower1, Just lower2) -> return ((upper1, lower1), Just (lower2, upper2))
        _ -> return ((upper1, upper2), Nothing)

solveQuadratic :: Double -> Double -> Double -> Maybe (Double, Maybe Double)
solveQuadratic a b c = do
    let d = b^2 - 4*a*c
    guard (d >= 0)
    return ((-b - signum a * sqrt d) / (2 * a), do
        guard (d > 0)
        return ((-b + signum a * sqrt d) / (2 * a)))

solveTest = do
	print "Smily curve - positive a"
	print $ solveWithMargin 1 (1) (-2) (0)
	print $ solveWithMargin 0.5 (1) (-2) (0)
	print $ solveQuadratic (1) (-2) (-1)
	print $ solveQuadratic (1) (-2) (1)

	print "Sad curve - negative a"
	print $ solveWithMargin 1 (-1) (2) (0)
	print $ solveWithMargin 0.5 (-1) (2) (0)
	print $ solveQuadratic (-1) (2) (-1)
	print $ solveQuadratic (-1) (2) (1)


