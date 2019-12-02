{-
Module: Day23
Description: Nanobots are required for some emergency teleportation. Each nanobot has a position in three-dimensional space and a range for its signal. Part1 asks for the number of nanobots within range of the strongest (signal-wise) nanobots signal range. Part2 asks for the coordinate which is in range of the most nanobots. Due to the number of nanobots and size of 3D space, a brute-forge approach is prohibitively expensive. The solution below leverages SMT solvers using the SBV library. 

Maintainer: Nate Launchbury
 -}

module Main where

import Library 
import Data.List
import Data.Map(Map)
import qualified Data.Map as Map
import Data.SBV hiding (inRange)
import Data.SBV.Tuple 

main = do 
  part1
  part2 

part1 = do 
  bs <- loadBots
  let g = greatestRange bs
  let rs = allInRange g bs
  putStrLn $ show (length rs) 


part2 = part2z3

-- | Returns parsed input, useful for testing 
loadBots = do 
  input <- readFile "input23.txt"
  return $ parseBots (lines input)

-- | A nanobot has a 3D position and a signal range 
data Nanobot = Nanobot {position :: (Int,Int,Int), range :: Int}
  deriving (Eq,Show)

-- | Bots are given as a 4-tuple
parseBot = do 
  x <- nextInt
  y <- nextInt
  z <- nextInt
  r <- nextInt 
  return (Nanobot {position = (x,y,z), range = r})

parseBots bs = map (run parseBot) bs 


-- | Sort the nanobots on range and return the greatest 
greatestRange :: [Nanobot] -> Nanobot
greatestRange ns = last $ sortOn range ns   

-- | Range-check predicate based on the first argument's signal range  
inRange :: Nanobot -> Nanobot -> Bool
inRange source target = let (a,b,c) = position source
                            r       = range source 
                            (x,y,z) = position target
                        in (manhattanDistance (a,b,c) (x,y,z) <= r)

-- | A Manhattan distance metric 
manhattanDistance :: (Int,Int,Int) -> (Int,Int,Int) -> Int
manhattanDistance (a,b,c) (x,y,z) = abs (a-x) + abs (b-y) + abs (c-z) 

-- | Returns the sublist of nanobots within range of a given nanobot 
allInRange :: Nanobot -> [Nanobot] -> [Nanobot]
allInRange n ns = filter (inRange n) ns 


-- | Construct a bot for testing
makeBot :: Int -> Int -> Int -> Int -> Nanobot
makeBot a b c r = Nanobot {position = (a,b,c), range = r}

ex1 = "pos=<0,0,0>, r=4\n pos=<1,0,0>, r=1\n pos=<4,0,0>, r=3\n pos=<0,2,0>, r=1\n pos=<0,5,0>, r=3\n pos=<0,0,3>, r=1\n pos=<1,1,1>, r=1\n pos=<1,1,2>, r=1\n pos=<1,3,1>, r=1\n pos=<0,-1,-3>, r=3\n"

ex2 = "pos=<10,12,12>, r=2\n pos=<12,14,12>, r=2\n pos=<16,12,12>, r=4\n pos=<14,14,14>, r=6\n pos=<50,50,50>, r=200\n pos=<10,10,10>, r=5\n" 


-- -- -- -- -- -- -- -- -- -- -- -- -- --
-- SBV and Z3

part2z3 = do 
  bots <- loadBots 
  let spheres = map makeSphere bots 
  ans <- optimize Lexicographic (problem spheres)
  print ans 

-- | Test to check SBV and Z3 are working/installed properly (uses `problem` defined below)
-- "testz3 exSpheres"
testz3 spheres = optimize Lexicographic (problem spheres)

exSpheres :: [Sphere]
exSpheres = [(0,1,2,3),(1,1,1,2),(4,4,4,4)]

-- I know they're not actually spheres
-- | A Sphere is represented just like a Nanobot: a 3D position for the center and a radius 
type Sphere = (SInteger, SInteger, SInteger, SInteger)
type Point = (SInteger, SInteger, SInteger)

-- | Explicit translation from Nanobots to Spheres by destructing and reconstructing fields
makeSphere :: Nanobot -> Sphere
makeSphere n = let (x,y,z) = position n 
               in (fromIntegral x, fromIntegral y, fromIntegral z, fromIntegral (range n))

-- | Problem for SBV to solve. Introduces an existential point and maximizes it to be in range of the most spheres. Ties should be broken based on shortest distance to the origin
problem :: [Sphere] -> Symbolic ()
problem spheres = do 
  x <- exists "x" 
  y <- exists "y" 
  z <- exists "z" 
  let contained = contains (x,y,z) spheres
  let distance = sManhattanDistance (0,0,0) (x,y,z)
  maximize "goal" (tuple (contained, (-distance)))

-- | Symbolic count of encompassing spheres for a given point 
contains :: Point -> [Sphere] -> SInteger
contains point spheres = sum (map (isWithin point) spheres)

-- | Symbolic range checking predicate to see if a point is in a sphere 
isWithin :: Point -> Sphere -> SInteger
isWithin point (a,b,c,r) = ite (sManhattanDistance point (a,b,c) .<= r) 1 0 

-- | Symbolic manhattan distance metric 
sManhattanDistance :: Point -> Point -> SInteger
sManhattanDistance (x,y,z) (a,b,c) = abs (x-a) + abs (y-b) + abs (z-c) 




