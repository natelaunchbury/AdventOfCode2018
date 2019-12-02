{- 
  Module: Day16
  Description: The day 16 puzzles from adventOfCode 2018. A primitive CPU-like system is discovered on the time-traveling wristwatch. 4 registers and 16 Opcodes are avaiable but it is unknown which Opcode is refered to by the numbers 0-15. In Part I, we figure out how many of the input-output behavior in the puzzle input matches at least 3 different Opcodes. In Part II, we discover which Opcode matches which 0-15 number and then use that information to run the commands in the second half of the puzzle-input.
  Maintainer: Nate Launchbury

-} 
module Day16 where

import Data.Bits
import Library
import Control.Applicative
import Data.List

main = do 
  part1 "input16.txt"
  part2 "input16.txt"

part1 file = do 
  input <- readFile file 
  let samples = parseSamples input 
  let cs = map matchingCodes samples 
  putStrLn $ show (length (filter (\x -> length x >= 3) cs))

part2 file = do 
  input <- readFile file 
  let (samples,commands) = parseFile input
  let cs = map matchingCodes samples
  let codes = discoverCodes cs
  let res = runCommands [0,0,0,0] commands codes 
  putStrLn $ show res

-- | A 'Register' is an Int value between 0 and 3
type Register = Int

type RegFunc = [Register] -> Int -> Int -> Register -> [Register]

-- | An 'Opcode' is a function which trasforms the registers using the inputs provided
data Opcode = Opcode String RegFunc 

-- | Samples are puzzle inputs containing input and output registers along with an opcode 
data Sample = Sample [Register] [Register] Int Int Int Register
  deriving Show 

-- | Commands are puzzle inputs containing opcode numbers along with input
data Command = Command Int Int Int Register
  deriving Show 

instance Show Opcode where
  show (Opcode s f) = s

-- | a lens into a [Register] which updates the register number using a supplied function
update :: [Register] -> Register -> (Int -> Int) -> [Register]
update [] _ _ = [] 
update (r:rs) n f 
 | n==0 = (f r):rs
 | True = r: update rs (n-1) f 

-- | uses 'update' to apply a constant function to a [Register]
updateC rs n v = update rs n (const v)


-- | There are 16 opcodes in consideration, stored in this list and detailed later. The ending "r" indicates that both arguments refer to registers while the ending "i" indicates the first argument is a register but the second is a value. 
allOpcodes :: [Opcode]
allOpcodes = [Opcode "addr" addr, Opcode "addi" addi,                     -- addition 
              Opcode "mulr" mulr, Opcode "muli" muli,                     -- multiplication  
              Opcode "banr" banr, Opcode "bani" bani,                     -- boolean and 
              Opcode "borr" borr, Opcode "bori" bori,                     -- boolean or 
              Opcode "setr" setr, Opcode "seti" seti,                     -- register update 
              Opcode "gtir" gtir, Opcode "gtri" gtri, Opcode "gtrr" gtrr, -- greater-than testing
              Opcode "eqir" eqir, Opcode "eqri" eqri, Opcode "eqrr" eqrr  -- equality testing
             ]              


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- parsing 

-- | Parses a 'Sample' by checking for "Before:" and "After:" keywords
parseSample = do 
  _  <- str "Before:"
  i0 <- nextInt 
  i1 <- nextInt 
  i2 <- nextInt 
  i3 <- nextInt 
 
  op <- nextInt 
  a  <- nextInt
  b  <- nextInt
  c  <- nextInt

  _  <- one  
  _  <- str "After:"
  o0 <- nextInt 
  o1 <- nextInt 
  o2 <- nextInt 
  o3 <- nextInt  
  _  <- takeUntil (=='B')

  return (Sample [i0,i1,i2,i3] [o0,o1,o2,o3] op a b c)

ex = "Before: [3, 2, 1, 1] 9 2 1 2 After:  [3, 2, 2, 1]"

-- | Parses many samples using 'parseSample' 
parseSamples = do 
  samples <- run (many parseSample)
  return samples 
 
-- | Parses a 'Command' by grabbing 4 Ints 
parseCommand = do 
  n <- nextInt
  a <- nextInt
  b <- nextInt
  c <- nextInt 
  return (Command n a b c) 

-- | Parses as many samples as possible and then parses commands, returning a pair of lists  
parseFile = do 
  samples <- run $ many parseSample 
  commands <- run $ many parseCommand 
  return (samples,commands) 

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- computation  

-- | Gets the register function out of an Opcode 
getOp :: Opcode -> RegFunc
getOp (Opcode s f) = f

-- | Gets the function out of an opcode table by looking up the function name 
getFunc :: String -> [Opcode] -> RegFunc 
getFunc _ [] = error "name not found" 
getFunc name (Opcode s f : os) 
  | s==name = f
  | True    = getFunc name os 

-- | Runs the register function of an opcode on a sample 
runOp :: Opcode -> Sample -> [Register] 
runOp o (Sample inp out n a b c) = f inp a b c 
  where f = getOp o 

-- | Returns a list of all opcodes which match the input-output of a sample 
matchingCodes :: Sample -> [(Int,String)]
matchingCodes (Sample inp out n a b c) = 
   [(n,show o) | o <- allOpcodes, (getOp o) inp a b c == out]

testFunc (Sample inp out n a b c) = [(getOp o) inp a b c | o <- allOpcodes]

-- | Runs a command on register list using a table of opcodes to lookup the correct function 
runCommand :: [Register] -> Command -> [(Int,String)] -> [Register]
runCommand rs (Command n a b c) tab = f rs a b c 
  where f = getFunc name allOpcodes
        name = fromJust $ lookup n tab 
        fromJust (Just s) = s
        fromJust Nothing = error "fromJust Nothing" 

-- | Runs a list of commands sequentially, keeping track of the registers 
runCommands :: [Register] -> [Command] -> [(Int,String)] -> [Register]
runCommands rs [] _ = rs
runCommands rs (c:cs) tab = runCommands (runCommand rs c tab) cs tab 

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- discovery 

-- | Discovers the number for each opcode by removing singletons from future candidates 
discoverCodes :: [[(Int,String)]] -> [(Int,String)]
discoverCodes s = discoverCodesH s []
  where discoverCodesH :: [[(Int,String)]] -> [(Int,String)] -> [(Int,String)]
        discoverCodesH [] cs = sortOn fst cs
        discoverCodesH (x:xs) cs  
          | length x==0 = discoverCodesH xs cs
          | length x==1 = discoverCodesH (removeCandidate (head x) xs) ((head x):cs)
          | otherwise   = discoverCodesH (sortOn length (x:xs)) cs 

-- | Removes an (Int,String) pair from the rest of the pairs
removeCandidate :: (Int,String) -> [[(Int,String)]] -> [[(Int,String)]]
removeCandidate (n,s) [] = []
removeCandidate (n,s) (x:xs) = filter (\x -> snd x/=s) x : removeCandidate (n,s) xs


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- opcodes (RegFunc to be put into Opcodes) 

-- | Addition on registers A and B 
addr :: RegFunc
addr rs a b c = updateC rs c (rs!!a+rs!!b) 

-- | Addition on value A and register B 
addi :: RegFunc
addi rs a b c = updateC rs c (rs!!a + b) 

-- | Multiplication on registers A and B
mulr :: RegFunc
mulr rs a b c = updateC rs c (rs!!a*rs!!b) 

-- | Multiplication on register A and value B 
muli :: RegFunc
muli rs a b c = updateC rs c (rs!!a * b) 

-- | Bitwise AND on registers. '(.&.)' is defined in Data.Bits
banr :: RegFunc
banr rs a b c = updateC rs c (rs!!a .&. rs!!b) 

-- | Bitwise AND on register A and value B 
bani :: RegFunc
bani rs a b c = updateC rs c (rs!!a .&. b) 

-- | Bitwise OR on registers. '(.|.)' is defined in Data.Bits
borr :: RegFunc
borr rs a b c = updateC rs c (rs!!a .|. rs!!b) 

-- | Bitwise OR on register A and value B 
bori :: RegFunc
bori rs a b c = updateC rs c (rs!!a .|. b) 

-- | Copies contents of register A into register C (B is ignored) 
setr :: RegFunc
setr rs a b c = updateC rs c (rs!!a)

-- | Stores value A in register C (B is ignored)
seti :: RegFunc
seti rs a b c = updateC rs c (a)

-- | Compares value A with register B using '>', storing 1 or 0 in C 
gtir :: RegFunc
gtir rs a b c = updateC rs c (if a>rs!!b then 1 else 0) 

-- | Compares register A with value B using '>', storing 1 or 0 in C 
gtri :: RegFunc 
gtri rs a b c = updateC rs c (if rs!!a>b then 1 else 0) 

-- | Compares register A with register B using '>', storing 1 or 0 in C 
gtrr :: RegFunc
gtrr rs a b c = updateC rs c (if rs!!a>rs!!b then 1 else 0) 

-- | Compares value A with register B using '=', storing 1 or 0 in C 
eqir :: RegFunc
eqir rs a b c = updateC rs c (if a==rs!!b then 1 else 0) 

-- | Compares register A with value B using '=', storing 1 or 0 in C 
eqri :: RegFunc 
eqri rs a b c = updateC rs c (if rs!!a==b then 1 else 0) 

-- | Compares register A with register B using '=', storing 1 or 0 in C 
eqrr :: RegFunc
eqrr rs a b c = updateC rs c (if rs!!a==rs!!b then 1 else 0) 

