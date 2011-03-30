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

getInitialTime :: Path -> Time
getInitialTime (Path t0 _ _ _) = t0

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

-- | Remove paths that ends before @time@
pathsFrom :: Time -> [Path] -> [Path]
pathsFrom time [] = []
pathsFrom time (path : paths) = 
    if (time >= getInitialTime path) then (path:paths)
    else pathsFrom time paths


-- Collisions
type Interval = (Double, Double)

pathBoxCollision :: Time -> Path -> Interval -> Path -> Interval -> Maybe Time 
pathBoxCollision time path1 (height1, width1) path2 (height2, width2) = do
    let path = substractPaths path1 path2
    (collisionX1, collisionX2M) <- solveWithMargin' ((width1 + width2) / 2) (polynomialX path)
    (collisionY1, collisionY2M) <- solveWithMargin' ((height1 + height2) / 2) (polynomialY path)
    liftM fst $ intersectionAfter time collisionX1 collisionY1 `mplus` do
        collisionX2 <- collisionX2M
        intersectionAfter time collisionX2 collisionY1 `mplus` do
            collisionY2 <- collisionY2M
            intersectionAfter time collisionX1 collisionY2 `mplus` do
                intersectionAfter time collisionX2 collisionY2
    where
        solveWithMargin' :: Double -> Polynomial -> Maybe (Interval, Maybe Interval)
        solveWithMargin' margin (Polynomial _ a v p) = solveWithMargin margin a v p

intersectionAfter :: Time -> Interval -> Interval -> Maybe Interval
intersectionAfter time (a1, a2) (b1, b2) = do
    guard (a2 > time && b2 > time)
    let c1 = max time (max a1 b1)
    let c2 = min a2 b2
    guard (c1 <= c2)
    return (c1, c2)
    
-- | Solve two quadratic inequalities @margin > a*t^2 + b*t + c > -margin@. 
-- @a@ may be zero, thereby yielding linear inequalities.
solveWithMargin :: Double -> Double -> Double -> Double -> Maybe (Interval, Maybe Interval)
solveWithMargin margin a b c =
    if a == 0 then do
        solution <- if b == 0 
                    then solveConstantWithMargin margin c
                    else return (solveLinearWithMargin margin b c)
        return (solution, Nothing)
    else do
        solveQuadraticWithMargin margin a b c

-- | Solve two quadratic inequalities @margin > a*t^2 + b*t + c > -margin@. 
-- @a@ are expected to be non-zero.
solveQuadraticWithMargin :: Double -> Double -> Double -> Double -> Maybe (Interval, Maybe Interval)
solveQuadraticWithMargin margin a b c = do
    margin <- return $ signum a * margin
    (upper1, Just upper2) <- solveQuadratic a b (c - margin) -- Discard single solution results
    case solveQuadratic a b (c + margin) of 
        Just (lower1, Just lower2) -> return ((upper1, lower1), Just (lower2, upper2))
        _ -> return ((upper1, upper2), Nothing)

-- | Solve a quadratic equation @a*t^2 + b*t + c == 0@. 
-- @a@ are expected to be non-zero.
solveQuadratic :: Double -> Double -> Double -> Maybe (Double, Maybe Double)
solveQuadratic a b c = do
    let d = b^2 - 4*a*c
    guard (d >= 0)
    return ((-b - signum a * sqrt d) / (2 * a), do
        guard (d > 0)
        return ((-b + signum a * sqrt d) / (2 * a)))

-- | Solve two liner inequalities @margin > a*t + b > -margin@. 
-- @a@ are expected to be non-zero.
solveLinearWithMargin :: Double -> Double -> Double -> Interval
solveLinearWithMargin margin a b =
    let margin' = margin * signum a in
    let upper = solveLinear a (b - margin') in
    let lower = solveLinear a (b + margin') in
    (lower, upper)

-- | Solve a liner equation @a*t + b == 0@. 
-- @a@ are expected to be non-zero.
solveLinear :: Double -> Double -> Double
solveLinear a b = negate b / a

-- | Solve two constant inequalities @margin > a > -margin@. 
solveConstantWithMargin :: Double -> Double -> Maybe Interval
solveConstantWithMargin margin a = do
    guard (margin > a && a > negate margin)
    let highestExponent = snd $ floatRange (undefined :: Double)
    return (-2^highestExponent, 2^highestExponent)

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

