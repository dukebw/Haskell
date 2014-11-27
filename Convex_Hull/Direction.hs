-- file: Ch3/Direction.hs
-- Functions relating to computing the convex hull of a set of 2D points.
-- http://en.wikipedia.org/wiki/Convex_hull 
-- If we consider three 2-D points a, b and c, and look at the angle formed
-- by the line segment from a to b and the line segment from b to c, it
-- either turns left, turns right or forms a straight line.
-- A Direction data type that represents the three possibilities: turn left,
-- turn right and straight line.
import AlgebraicVector
import Data.List

data Direction = LeftTurn
               | RightTurn
               | StraightLine
   deriving (Show, Eq)

-- A function that takes a list of 2D points and computes the direction of each
-- successive triple. E.g., given a list of points [a,b,c,d,e], it begins by
-- computing the turn made by [a,b,c], then the turn made by [b,c,d], then
-- [c,d,e].
findTurns :: [Cartesian2D] -> [Direction]
findTurns x | length x < 3 = []
findTurns (x:y:z:zs) = (findTurnDirection x y z):(findTurns (y:z:zs))

-- A function that calculates the turn made by three 2D points
-- and returns a Direction.
findTurnDirection :: Cartesian2D -> Cartesian2D -> Cartesian2D -> Direction
findTurnDirection a b c
   | ccw a b c == 0 = StraightLine
   | ccw a b c > 0 = LeftTurn
   | otherwise = RightTurn

-- Calculates the z-component of a crossproduct of points in the x-y plane.
-- Three points are a counter-clockwise turn if ccw > 0 because ccw is a
-- determinant that gives twice the signed area of the triangle formed by
-- p1, p2 and p3.
ccw :: Cartesian2D -> Cartesian2D -> Cartesian2D -> Double
ccw (Cartesian2D x1 y1) (Cartesian2D x2 y2) (Cartesian2D x3 y3) 
   = (x2-x1) * (y3-y1) - (x3-x1) * (y2-y1)

-- Converts a list of doubles to a list of 2D points.
makePoints :: [Double] -> [Cartesian2D]
makePoints x | length x < 2 = []
makePoints (x:y:ys) = (Cartesian2D x y):(makePoints ys)

-- Read a String to Double
readToDbl :: String -> Double
readToDbl x = (read x)::Double

-- Computes the convex hull of a list of 2D points.
convexHull :: [Cartesian2D] -> [Cartesian2D]
convexHull x = (tail l) ++ (tail u)
   where u = hullHalf [] (sortPoints x)
         l = hullHalf [] (reverse (sortPoints x))

-- Sorts a list of 2D points based first on their x-coordinate, then y.
sortPoints :: [Cartesian2D] -> [Cartesian2D]
sortPoints xs = sortBy compareX xs
   where compareX (Cartesian2D x1 y1) (Cartesian2D x2 y2)
            | compare x1 x2 == EQ = compare y1 y2
            | otherwise = compare x1 x2

-- hullHalf computes an upper hull based on Andrew's monotone chain convex
-- hull algorithm.
hullHalf :: [Cartesian2D] -> [Cartesian2D] -> [Cartesian2D]
hullHalf xs [] = xs
hullHalf xs (a:as) | length xs < 2 = hullHalf (a:xs) as
hullHalf (x:y:ys) (a:as)
   | findTurnDirection y x a == LeftTurn = hullHalf (a:x:y:ys) as
   | otherwise = hullHalf (y:ys) (a:as)

-- Prints a list of turns from an input set of test points.
main = do
       inpDbls <- readFile "TEST_POINTS"
       writeFile "CONVEX_HULL" (show  . convexHull . makePoints . 
                              map readToDbl . words $ inpDbls)
