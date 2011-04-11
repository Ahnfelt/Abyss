module Collision where

import Arithmetic
import Floating
import Prelude hiding ((/), acos, sqrt)
import Control.Monad
import Data.Maybe

import Debug.Trace

type Interval = (Double, Double)
type BoxShape = (Double, Double)

pathsBoxCollision :: Time -> [Path] -> BoxShape -> [Path] -> BoxShape -> Maybe (Time, Vector)
pathsBoxCollision time paths1 shape1 paths2 shape2 = do
    paths1 <- return $ pathsFrom time paths1
    paths2 <- return $ pathsFrom time paths2
    path1 <- listToMaybe paths1
    path2 <- listToMaybe paths2
    let expireTime = min (head (expirations paths1)) (head (expirations paths2))
    (do 
        (collision, normal) <- pathBoxCollision time path1 shape1 path2 shape2 
        guard (collision <= expireTime)
        return (collision, normal)
        ) `mplus` (do
            guard (not (isInfinite expireTime))
            paths1 <- return $ pathsFrom expireTime paths1
            paths2 <- return $ pathsFrom expireTime paths2
            pathsBoxCollision time paths1 shape1 paths2 shape2
        )

-- | Assume that two axis aligned boxes with the given shapes are moving their 
-- centers as defined by the given paths, this function finds the first time, if any,
-- after the specified @time@, where these boxes collide. 
pathBoxCollision :: Time -> Path -> BoxShape -> Path -> BoxShape -> Maybe (Time, Vector)
pathBoxCollision time path1 (width1, height1) path2 (width2, height2) = do
    let path = substractPaths path1 path2
    (collisionX1, collisionX2M) <- solveWithMargin' ((width1 + width2) / 2) (polynomialX path)
    (collisionY1, collisionY2M) <- solveWithMargin' ((height1 + height2) / 2) (polynomialY path)
    (collisionTime, Vector x y) <- liftM (first fst) $ 
        (do 
            intersectionAfter time collisionX1 collisionY1
        ) `mplus` (do
            collisionX2 <- collisionX2M
            intersectionAfter time collisionX2 collisionY1
        ) `mplus` (do
            collisionY2 <- collisionY2M
            intersectionAfter time collisionX1 collisionY2
        ) `mplus` (do
            collisionX2 <- collisionX2M
            collisionY2 <- collisionY2M
            intersectionAfter time collisionX2 collisionY2
        )
    let (Vector x1 y1) = getPosition path1 collisionTime
    let (Vector x2 y2) = getPosition path2 collisionTime
    let (x', y') = (signum (x1 - x2), signum (y1 - y2))
    return (collisionTime, Vector (x * x') (y * y'))
    where
        solveWithMargin' :: Double -> Polynomial -> Maybe (Interval, Maybe Interval)
        solveWithMargin' margin (Polynomial _ a v p) = solveWithMargin margin a v p

intersectionAfter :: Time -> Interval -> Interval -> Maybe (Interval, Vector)
intersectionAfter time (a1, a2) (b1, b2) = do
    guard (a2 > time && b2 > time)
    guard (a1 >= time || b1 >= time) -- Ignore initial collisions
    let c1 = max time (max a1 b1)
    let c2 = min a2 b2
    guard (c1 <= c2)
    return ((c1, c2), if a1 < b1 then Vector 0 1 else Vector 1 0)
    
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
    (upper1, upper2) <- solveQuadratic a b (c - margin) -- Discard single solution results
    case solveQuadratic a b (c + margin) of 
        Just (lower1, lower2) -> return ((upper1, lower1), Just (lower2, upper2))
        _ -> return ((upper1, upper2), Nothing)

-- | Solve a quadratic equation @a*t^2 + b*t + c == 0@. 
-- @a@ are expected to be non-zero.
solveQuadratic :: Double -> Double -> Double -> Maybe (Double, Double)
solveQuadratic a b c = do
    let d = b^2 - 4*a*c
    guard (d >= 0)
    return ((-b - signum a * sqrt d) / (2 * a), (-b + signum a * sqrt d) / (2 * a))

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
    return (minusInfinite, infinite)

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


bounce :: Vector -> Vector -> Double -> Double -> Vector -> Double -> (Vector, Vector)
bounce v1 v2 m1 m2 normal elasticity = 
    let impulse = normal .* ((((1 - elasticity) * ((v1 .-. v2)) `dot` normal)) / (1 / m1 + 1 / m2)) in
    (v1 .-. impulse .* (1 / m1), v2 .+. impulse .* (1 / m2))


bounce2 :: Vector -> Vector -> Double -> Double -> Vector -> Double -> (Vector, Vector)
bounce2 v1@(Vector x1 y1) v2@(Vector x2 y2) m1 m2 normal elasticity = 
    let invertedNormal = normal .* (-1) in
    (v1 .-. (v1 `projectedOn` normal) .+. v2 `projectedOn` normal, 
    v2 .-. (v2 `projectedOn` normal) .+. v1 `projectedOn` normal)



