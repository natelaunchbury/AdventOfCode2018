{-
Module: Day17
Description: You need to locate water for some elves. You get a cross section of soil underneath a 'Spring' of water, containing 'Sand' and 'Clay'. The water will move vertically downward through 'Sand' turning it into 'Wetsand' and pooling as 'Water' in any basins formed by 'Clay'. Part1 ask how many total 'Veins' of 'Wetsand' and 'Water' will form from an unlimited supply of water soaking into the 'Soil' from the 'Spring'. Part2 asks for the total amount of pooled 'Water'. 

The function 'animateSoil' takes in a filepath and creates the file "gif17.gif" which animates the water soaking into the 'Soil' from the 'Spring'. 
Maintainer: Nate Launchbury

-}


module Day17 where

import Data.Map(Map)
import qualified Data.Map as Map 
import Data.List
import Library 
import Codec.Picture



-- -- -- -- -- -- -- -- -- 
-- execution

main = do 
  part1 "input17.txt"
  part2 "input17.txt"

part1 file = do 
  input <- readFile file
  let s = makeSoil input
  s' <- simulate ([spring],[]) (deepest s) s
  writeFile "output17.txt" (outputSoil s')
  putStrLn $ "The number of wet veins is: " ++ show (length (allWet s'))
  putStrLn $ "Wet soil written to 'output17.txt'"

part2 file = do 
  input <- readFile file
  let s = makeSoil input
  s' <- simulate ([spring],[]) (deepest s) s 
  putStrLn $ "The number of water veins is: " ++ show (length (allWater s'))


-- |Uses the accumulated images from 'simulateDraw' to make a gif 
-- |Two simulations occur, the first simply to find the greatest bounding box 
animateSoil file = do 
  s <- loadFile file
  s' <- simulate initial (deepest s) s 
  let bnds = boundingBox (Map.keys s') 
  (imgs,s') <- simulateDraw initial (deepest s) bnds [drawSoil bnds s] s 
  writeAnimation "gif17.gif" 15 imgs
  putStrLn $ "Done!, gif output as \"gif17.gif\""
  putStrLn $ "Open with ':!open -a \"Google Chrome\" \"gif17.gif\"'" 

-- -- -- -- -- -- -- -- -- 
-- types

-- |A coordinate map showing veins. If a Coord does not appear, it is assumed to be 'Sand'
type Soil = Map Coord Vein

-- |Everything is imagined as 'Sand' by default, 'Clay' is added during parsing, 'Spring' at (500,0) is special, 'Water' comes from the spring and either pools above 'Clay' or creates 'Wetsand' 
data Vein = Sand | Clay | Spring | Water | Wetsand 
  deriving (Show,Eq)

-- -- -- -- -- -- -- -- -- 
-- parsing

-- |Creates a list of Coords for a vertical segment of clay
verticalClay :: Int -> Int -> Int -> [(Coord,Vein)]
verticalClay x loy hiy = zip [(x,y) | y <- [loy..hiy]] (repeat Clay) 

-- |Creates a list of Coords for a horizontal segment of clay
horizontalClay :: Int -> Int -> Int -> [(Coord,Vein)]
horizontalClay y lox hix = zip [(x,y) | x <- [lox..hix]] (repeat Clay) 

-- |Encodes a 'Vein' as a 'Char' for printing
encode :: Vein -> Char
encode Sand    = ' '
encode Clay    = '#'
encode Spring  = '+'
encode Water   = '~'
encode Wetsand = '|'

-- |Parses a single line, creating a [(Coord,Vein)] of 'Clay' veins
parseLine = do 
  a  <- one 
  b  <- nextInt
  lo <- nextInt
  hi <- nextInt  
  return ((if a=='x' then verticalClay else horizontalClay) b lo hi)

-- |Creates a soil map from a input string
makeSoil :: String -> Soil 
makeSoil s = let ls = lines s
                 clays = concat (map (run parseLine) ls)
                 spring = ((500,0),Spring)
             in Map.fromList (spring:(clays)) 
  

-- |Encodes the Soil as a string for output 
outputSoil :: Soil -> String
outputSoil s = let (lows,highs) = boundingBox (map fst (Map.toList s))
                   sand = Map.fromList $ zip (coordsWithin lows highs) (repeat Sand)
                   s' = s `Map.union` sand  -- union is left-biased 
               in outputSoilH (sortBy compareCoord (Map.toList s')) lows highs
   where outputSoilH [] _ highs = []
         outputSoilH (((x,y),v):vs) (lox,loy) highs 
           | x==lox = '\n':encode v: outputSoilH vs (lox,loy) highs
           | True   = encode v : outputSoilH vs (lox,loy) highs



-- -- -- -- -- -- -- -- -- 
-- soil functions

-- |The 'Coord' of the 'Spring' of water
spring :: Coord
spring = (500,0) 

-- |The initial Queue for a simulation
initial :: Queue Coord 
initial = ([spring],[])

-- |Returns the corresponding extreme coordinate value from the 'Soil'
deepest,leftmost,rightmost :: Soil -> Int
deepest    s = maximum $ map (snd . fst) (Map.toList s)
leftmost   s = minimum $ map (fst . fst) (Map.toList s)  
rightmost  s = maximum $ map (fst . fst) (Map.toList s)  

-- |Shallowest is special since we only want to count at the level of the highest 'Clay'
shallowest :: Soil -> Int 
shallowest s = let clays = filter (\v -> snd v == Clay) (Map.toList s) 
               in minimum $ map (snd . fst) clays

-- |Returns a list of all 'Water' and 'Wetsand' veins in the 'Soil' 
allWet :: Soil -> [(Coord,Vein)]
allWet s = let ss = Map.toList s
               wets = filter (\v -> snd v==Water || snd v==Wetsand) ss
               shallow = shallowest s
           in filter (\v -> (snd . fst) v >= shallow) wets 
            

-- |Returns a list of all 'Water' veins in the 'Soil' 
allWater :: Soil -> [(Coord,Vein)]
allWater s = let ss = Map.toList s
                 wets = filter (\v -> snd v==Water) ss
                 shallow = shallowest s
             in filter (\v -> (snd . fst) v >= shallow) wets 


-- -- -- -- -- -- -- -- -- 
-- water functions
-- always keeps track of the "splashes": coords where flowing water is hitting still water 

-- |Simualtes the flow of water, returning the final 'Soil' 
simulate :: Queue Coord -> Int -> Soil -> IO (Soil)
simulate q deep s 
  | isEmpty q = return s
  | otherwise = do 
     let (c,q') = front q 
     let (cs,s') = pour c deep s
     simulate (enqueue q' cs) deep s'

-- |Simulates the flow of water interactive, prompting the user at each step 
-- |A heuristic is used to print only the pertinent portion of the 'Soil' if too large 
simulateI :: Queue Coord -> Int -> Soil -> IO ()
simulateI q deep s
  | isEmpty q = putStrLn $ "Done! There are " ++ show (length (allWet s)) ++ " wet veins"
  | otherwise = do 
     let (c,q') = front q
     let (cs,s') = pour c deep s
     let ps = take 50 (drop (snd c - 10) (lines (outputSoil s')))
     printLines ps 
     putStrLn $ "Press Enter to continue, Ctrl-C to exit"
     _ <- getLine 
     simulateI (enqueue q' cs) deep s'

-- |Simulates the flow of water, accumulating an image at each step 
simulateDraw :: Queue Coord ->                       -- ^Work queue ("splashes") 
                Int ->                               -- ^'deepest' part of 'Soil'  
                (Coord,Coord) ->                     -- ^Greatest bounding box 
                [Image PixelRGB8] ->                 -- ^Accumulating list of images
                Soil ->                              -- ^Current 'Soil'
                IO ([Image PixelRGB8],Soil)          -- ^list of images and final 'Soil'
simulateDraw q deep bnds imgs s 
  | isEmpty q = return (reverse imgs,s)
  | otherwise = do 
     let (c,q') = front q
     let (cs,s') = pour c deep s 
     let img = drawSoil bnds s'
     simulateDraw (enqueue q' cs) deep bnds (img:imgs) s'

-- |Pours water from the splash into the soil, returning a new splash and soil at each step
-- |Can: 1) fill a 'Clay' basin with 'Water'
-- |     2) moisten a horizontal patch of 'Sand', making it 'Wetsand'
-- |     3) spill over the edge of a 'Clay' basin, perhaps at multiple places 
pour :: Coord -> Int -> Soil -> ([Coord],Soil)
pour c deep s 
  | snd c >= deep = ([],s)
  | otherwise =  
     case Map.lookup (down c) s of -- search vertically, replacing 'Sand' with 'Wetsand'
        Just Wetsand -> ([],s) -- pour (down c) s
        Nothing      -> pour (down c) deep (Map.insert (down c) Wetsand s)
        Just _       ->            -- we hit 'Water' or 'Clay', need to spread horizontally
           case edge c s of
   {-1-}     ((l,Clay),(r,Clay)) -> fillBasin c s
   {-2-}     ((l,Clay),(r,Sand)) -> ([r], fill c (coordSpan (l,r)) Wetsand s)
   {-2-}     ((l,Sand),(r,Clay)) -> ([l], fill c (coordSpan (l,r)) Wetsand s) 
   {-3-}     ((l,Sand),(r,Sand)) -> ([l,r], fill c (coordSpan (l,r)) Wetsand s) 
             ((l,Water),(r,_))   -> ([],s)
             ((l,_),(r,Water))   -> ([],s)


-- |Fills the given splash point with given veins, returning a new splash and 'Soil'
fill :: Coord -> [Coord] -> Vein -> Soil -> Soil
fill splash [] v s = s
fill splash (c:cs) v s = fill splash cs v (Map.insert c v s) 

-- |Completely fills a basin of clay, returning new splash list and 'Soil' 
fillBasin :: Coord -> Soil -> ([Coord],Soil)
fillBasin c s = 
   case edge c s of 
     ((l,Clay),(r,Clay))    -> fillBasin (up c) (fill c (coordSpan (l,r)) Water s)
     ((l,Clay),(r,Sand))    -> ([r], fill c (coordSpan (l,r)) Wetsand s)
     ((l,Sand),(r,Clay))    -> ([l], fill c (coordSpan (l,r)) Wetsand s)
     ((l,Sand),(r,Sand))    -> ([l,r], fill c (coordSpan (l,r)) Wetsand s)
     ((l,Wetsand),(r,Sand)) -> ([r], fill c (coordSpan (l,r)) Wetsand s) 
     ((l,Wetsand),(r,Clay)) -> ([], fill c (coordSpan (l,r)) Wetsand s) 
     ((l,Sand),(r,Wetsand)) -> ([l], fill c (coordSpan (l,r)) Wetsand s) 
     ((l,Clay),(r,Wetsand)) -> ([], fill c (coordSpan (l,r)) Wetsand s) 
     ((l,Water),(r,_))      -> ([],s)
     ((l,_),(r,Water))      -> ([],s)



-- |Returns the left and right edges and the veins that caused them to stop 
-- | Clay means a wall, Sand means a dropoff, Water means submerged, Wetsand is special 
edge :: Coord -> Soil -> ((Coord,Vein),(Coord,Vein))
edge splash s = (leftEdge splash s, rightEdge splash s)

-- |Finds the left edge, used in 'edge'
leftEdge c s = 
  case Map.lookup (left c) s of 
     Just Clay -> (c,Clay)
     Nothing -> 
       case Map.lookup (down (left c)) s of 
           Nothing -> (left c,Sand)
           Just _ -> leftEdge (left c) s 
     Just Water -> (c,Water) 
     Just Wetsand -> 
       case Map.lookup (down (left c)) s of 
           Just Wetsand -> (c,Wetsand)
           _ -> leftEdge (left c) s 
     
      
-- |Finds the right edge, used in 'edge'
rightEdge c s = 
   case Map.lookup (right c) s of 
     Just Clay -> (c,Clay)
     Nothing -> 
        case Map.lookup (down (right c)) s of 
            Nothing -> (right c,Sand)
            Just _ -> rightEdge (right c) s 
     Just Water -> (c,Water)
     Just Wetsand -> 
       case Map.lookup (down (right c)) s of 
           Just Wetsand -> (c,Wetsand)
           _ -> rightEdge (right c) s 



-- -- -- -- -- -- -- -- -- 
-- loading 

loadFile file = do 
  r <- readFile file
  let s = makeSoil r
  return s

runSoil file = do 
  s <- loadFile file
  simulateI initial (deepest s) s


-- -- -- -- -- -- -- -- -- 
-- animations

-- can use pretty much this same function for any coordinate map 
-- need to supply the bounds of the last frame since subsequent may get larger
drawSoil :: (Coord,Coord) -> Soil -> Image PixelRGB8
drawSoil bnds s = drawCoords bnds $ \p -> 
    case Map.lookup p s of 
      Just Spring  -> PixelRGB8 15 255 15         -- green 
      Just Clay    -> PixelRGB8 255 100 50        -- brown 
      Just Water   -> PixelRGB8 0 100 255         -- blue 
      Just Wetsand -> PixelRGB8 100 200 255       -- light blue 
      _ -> PixelRGB8 255 225 200                  -- light yellow 







