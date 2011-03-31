module Collision where

import Arithmetic
import Floating
import Prelude hiding ((/), acos, sqrt)
import Control.Monad
import Data.Maybe

type Interval = (Double, Double)

pathsBoxCollision :: Time -> [Path] -> Interval -> [Path] -> Interval -> Maybe Time 
pathsBoxCollision time paths1 shape1 paths2 shape2 = do
    paths1 <- return $ pathsFrom time paths1
    paths2 <- return $ pathsFrom time paths2
    path1 <- listToMaybe paths1
    path2 <- listToMaybe paths2
    pathBoxCollision time path1 shape1 path2 shape2 `mplus` do
        (paths1, paths2) <- return $ removeFirstExpiring (paths1, paths2)
        pathsBoxCollision time paths1 shape1 paths2 shape2

removeFirstExpiring :: ([Path], [Path]) -> ([Path], [Path])
removeFirstExpiring (p1 : p2 : ps, q1 : q2 : qs) = 
    if getInitialTime p2 < getInitialTime q2
    then (     p2 : ps, q1 : q2 : qs)
    else (p1 : p2 : ps,      q2 : qs)
removeFirstExpiring (p1 : p2 : ps, [q1]) = (p2 : ps, [q1])
removeFirstExpiring ([p1], q1 : q2 : qs) = ([p1], q2 : qs)
removeFirstExpiring ([p], [q]) = ([], [])


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


