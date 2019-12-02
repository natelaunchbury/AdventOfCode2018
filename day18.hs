{-
Module: Day18
Description: The elves are harvesting trees in a 'Forest' where each 'Acre' is either 'Open', 'Wooded' or a 'Lumberyard'. The 'Forest' evolves each minute based on the neighboring 'Acre's of each 'Acre'. A 'Forest' has resource value equal to the number of wooded acres multiplied by the number of lumberyards. Part1 asks for the resource value of the 'Forest' after 10 minutes. Part2 asks for the resource value after 1,000,000,000 minutes. There is a loop that occurs every 700 minutes (resources at 900 == resources at 1600, for example). So since 1000000000 `mod` 700 = 300, you can just use the resources at a sufficiently evolved member of the equivalence class (i.e. 1000) 
Maintainer: Nate Launchbury
-}

module Day18 where

import Library 
import qualified Data.Map as Map 
import Data.Map(Map)
import Data.List
import Codec.Picture

main = do 
  part1
  part2

part1 = do 
  f <- loadFile "input18.txt"
  let f' = simulateN 10 f
  let (o,w,l) = resources f'
  putStrLn $ "Resources at 10 min: open = " ++ show o ++ 
                                ", wooded = " ++ show w ++ 
                                ", lumberyards = " ++ show l
  putStrLn $ "Value = " ++ show (w*l) 


part2 = do 
  f <- loadFile "input18.txt"
  let f' = simulateN 1000 f 
  let (o,w,l) = resources f'
  putStrLn $ "Resources at 1000 min: open = " ++ show o ++ 
                                  ", wooded = " ++ show w ++ 
                                  ", lumberyards = " ++ show l
  putStrLn $ "Value = " ++ show (w*l) 

-- -- -- -- -- -- -- -- -- -- --
-- types

-- |A 'Forest' is a coordinate map of 'Acre's 
type Forest = Map Coord Acre

-- |An 'Acre' can be 'Open', 'Wooded', or a 'Lumberyard' (others are for parsing) 
data Acre = Open | Wooded | Lumberyard | Newline | Empty 
  deriving (Show,Eq)


-- -- -- -- -- -- -- -- -- -- --
-- "parsing"

-- |Decodes a char into an 'Acre' 
decode :: Char -> Acre 
decode '.'  = Open 
decode '|'  = Wooded 
decode '#'  = Lumberyard 
decode '\n' = Newline 
decode _    = Empty

-- |Encodees an 'Acre' as a char for printing
encode :: Acre -> Char 
encode Open       = '.'
encode Wooded     = '|'
encode Lumberyard = '#'
encode Newline    = '\n'
encode Empty      = ' '

-- |Makes a 50x50 'Forest' from the given input string 
makeForest :: String -> Forest
makeForest s = Map.fromList $ zip (coords 50 50) (map decode s) 

-- |Turns a 'Forest' into a string suitable for printing
outputForest :: Forest -> String 
outputForest f = outputForestH (sortBy compareCoord (Map.toList f))
  where outputForestH :: [(Coord,Acre)] -> String
        outputForestH [] = [] 
        outputForestH (((x,y),a):as)
--          | x==0 = '\n':encode a : outputForestH as
          | True = encode a : outputForestH as 

-- |Uses 'outputForest' to print a 'Forest'
printForest :: Forest -> IO ()
printForest f = printLines (lines (outputForest f))

-- -- -- -- -- -- -- -- -- -- --
-- forest functions

-- |Returns the neighboring 'Acre's to a given 'Coord'
surrounding :: Coord -> Forest -> [Maybe Acre]
surrounding c f = map (`Map.lookup` f) (neighbors c) 

-- |Checks to see if a given 'Coord' is surrounded by at least n specific 'Acres'
surroundedBy :: Int -> Acre -> Coord -> Forest -> Bool 
surroundedBy n a c f = length (filter (\x -> x == Just a) (surrounding c f)) >= n

-- |The evolution rules for each 'Acre' in a 'Forest'
evolve :: (Coord,Acre) -> Forest -> Acre
evolve (c,Open) f 
  | surroundedBy 3 Wooded c f     = Wooded 
  | otherwise                     = Open
evolve (c,Wooded) f
  | surroundedBy 3 Lumberyard c f = Lumberyard
  | otherwise                     = Wooded 
evolve (c,Lumberyard) f 
  | surroundedBy 1 Lumberyard c f 
    && surroundedBy 1 Wooded c f  = Lumberyard
  | otherwise                     = Open 
evolve (c,Newline) f = Newline
evolve (c,Empty) f = Empty

-- |Returns the number of each 'Acre' type in a 'Forest'
resources :: Forest -> (Int,Int,Int)
resources f = let w = length (wooded f) 
                  l = length (lumberyard f) 
              in (50*50-(w+l),w,l)

value :: Forest -> Int 
value f = length (wooded f) * length (lumberyard f) 

unJust (Just f) = f
unJust Nothing = error "unjust Nothing" 

open :: Forest -> [(Coord,Acre)]
open f = let fs = Map.toList f in filter (\a -> snd a == Open) fs

wooded :: Forest -> [(Coord,Acre)]
wooded f = let fs = Map.toList f in filter (\a -> snd a == Wooded) fs

lumberyard :: Forest -> [(Coord,Acre)]
lumberyard f = let fs = Map.toList f in filter (\a -> snd a == Lumberyard) fs

-- -- -- -- -- -- -- -- -- -- --
-- simulations

-- |Returns the 'Forest' after simulating n evolutions on it
simulateN :: Int -> Forest -> Forest
simulateN 0 f = f
simulateN n f = let f' = Map.mapWithKey (\c a -> evolve (c,a) f) f
                in simulateN (n-1) f' 

-- |Simulates the 'Forest' at a given granularity, printing the 'resources' at each step 
simulateBy :: Int -> Forest -> IO ()
simulateBy n f = simulateByH n 0 f 
  where simulateByH :: Int -> Int -> Forest -> IO ()
        simulateByH n m f = do 
          putStrLn $ "Resources at " ++ show m ++ " = " ++ show (resources f) 
          let f' = simulateN n f
          simulateByH n (m+n) f'

-- |Simulates the 'Forest' interactively, prompting the user at each step 
simulateI :: Forest -> IO ()
simulateI f = simulateIH 0 f
  where simulateIH :: Int -> Forest -> IO ()
        simulateIH n f = do
          printForest f
          putStrLn $ "At " ++ show n ++ " minutes" 
          putStrLn $ "Press Enter to continue, Crtl-C to exit" 
          _ <- getLine
          simulateIH (n+1) (simulateN 1 f)


-- -- -- -- -- -- -- -- -- -- --
-- animations

-- |Animates the evolution of a 'Forest' for n minutes (output: "gif18.gif")
animateForest :: Int -> Forest -> IO ()
animateForest n f = do 
  let (imgs,f') = simulateDraw n [drawForest f] f
  writeAnimation "gif18.gif" 25 imgs 
  putStrLn $ "Done! gif written to \"gif18.gif\""
  putStrLn $ "Open with ':!open -a \"Google Chrome\" \"gif18.gif\"'" 

-- |Simulates the Forest to n minutes, drawing a picture at each stage 
simulateDraw :: Int -> [Image PixelRGB8] -> Forest -> ([Image PixelRGB8],Forest)
simulateDraw 0 imgs f = (reverse imgs,f)
simulateDraw n imgs f = let f' = simulateN 1 f
                            img = drawForest f'
                        in simulateDraw (n-1) (img:imgs) f'

-- |Draws a 'Forest' where 'Open' are light yellow, 'Wooded' are green, 'Lumberyard' are brown
drawForest :: Forest -> Image PixelRGB8
drawForest f = drawCoords bnds $ \p -> 
  case Map.lookup p f of 
     Just Open       -> PixelRGB8 250 250 200 
     Just Wooded     -> PixelRGB8 100 255 100 
     Just Lumberyard -> PixelRGB8 255 100 50 
     _               -> PixelRGB8 0   0   0 
  where bnds = boundingBox (map fst (Map.toList f))
     
-- -- -- -- -- -- -- -- -- -- --
-- IO helpers

-- |Returns the parsed 'Forest' from target file 
loadFile file = do 
  r <- readFile file
  return (makeForest r) 

-- |Makes a 10x10 'Forest' 
makeExample s = Map.fromList $ zip (coords 10 10) (map decode s)

-- |Returns the parsed 10x10 'Forest' from target file 
loadExample = do 
  r <- readFile "ex18.txt"
  return (makeExample r) 

