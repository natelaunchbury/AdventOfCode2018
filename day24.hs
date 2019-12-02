{-
Module: Day24
Description: A D&D-style fight simulation between Immune System and Infection armies. Each army takes turns attacking groups in the opposing army until only one army remains. Groups have damage types and may also have weaknesses or immunities to certain types of damage. Each group can attack and be attacked by at most one opposing group per round of combat. They choose to attack the group to which they will do the most damage, of the remaining available targets. Part1 asks for the results of the battle given in the input. Part2 asks for the results of the battle where the immune system is given the lowest possible attack boost to achieve victory.

Maintainer: Nate Launchbury
 -}
module Day24 where

import Data.List
import Library
import Control.Applicative

main = do 
  part1
  part2 

part1 :: IO (Army)
part1 = do
  (immsys,infect) <- parseArmies "input24.txt"
  let result = fight immsys infect 
--  result <- fightI immsys infect
  return result


part2 :: IO ((Army,Int)) 
part2 = do 
  (immsys,infect) <- parseArmies "input24.txt"
  let fights = [(fight (boost n immsys) infect,n) | n <- scanl (*) 1 (repeat 2)]
  let upper  = head $ dropWhile (\(a,n) -> winner a==True) fights 
  let num = snd upper 
  let fights' = [(fight (boost n immsys) infect,n) | n <- [num `div` 2 .. num]]
  let lowest = head $ dropWhile (\(a,n) -> winner a==True) fights'
  return lowest
    where winner a = infectious (head a) 




-- -- -- -- -- -- -- -- --
-- types

type Army = [Group]

-- | Each group contains a number of starting units, each with a number of hit points. The group as a whole has an attack with a certain damage type and possible weaknesses and immunities to damage types. They also have an initiative number (lower acts before higher) and are either infectious or part of the immune system army. 
data Group = Group { units :: Int 
                   , hp :: Int
                   , weak :: [Damage]
                   , immune :: [Damage]
                   , attack :: (Int, Damage)
                   , initiative :: Int
                   , infectious :: Bool 
                   }
  deriving (Show, Eq) 

-- | The five damage types 
data Damage = Cold | Fire | Slashing | Bludgeoning | Radiation 
  deriving (Show, Eq)

-- |An Ordering on Groups based on effectivePower and then initiative 
compareGroups :: Group -> Group -> Ordering 
compareGroups a b
  | effectivePower a > effectivePower b = GT
  | effectivePower a < effectivePower b = LT
  | initiative a > initiative b = GT
  | initiative a < initiative b = LT
  | otherwise = EQ  -- this shouldn't occur on proper input 

-- -- -- -- -- -- -- -- --
-- parsing

-- | Parses all the immune system grorups and then infection groups 
parseArmies file = do 
  input <- readFile file 
  let ls = lines input 
  let immune = tail $ takeWhile (/="") ls 
  let infect = tail $ dropWhile (/="Infection:") ls 
  return (map (run (parseGroup False)) immune, map (run (parseGroup True)) infect)

-- | Parses a single group 
parseGroup allegiance = do 
  units      <- num
  hitpoints  <- nextInt
  str " hit points "
  a <- one 
  (weak,imm) <- opt ([],[]) parseVulnerabilities
  attack     <- nextInt
  space 
  damageType <- word  
  init       <- nextInt 
  return (Group {units=units, hp=hitpoints, weak=map decode weak, immune=map decode imm, 
                 attack=(attack,decode damageType), initiative=init, infectious=allegiance})

-- | Parsing weaknessess and immunities is tricky since they can be nonexistent and in either order 
parseVulnerabilities = do 
  lit '('
  (ws,is) <- pure (,) <*> parseWeaknesses <*> (opt [] (lit ';' >> parseImmunities))
             <|> swap <$> 
             (pure (,) <*> parseImmunities <*> (opt [] (lit ';' >> parseWeaknesses)))
  lit ')'
  return (ws,is) 
    where swap (a,b) = (b,a) 

parseWeaknesses = (str "weak to " *> commaList word)

parseImmunities = (str "immune to " *> commaList word)
   
-- |A simple decoder for damage types from strings  
decode :: String -> Damage
decode "cold"        = Cold
decode "fire"        = Fire
decode "slashing"    = Slashing
decode "bludgeoning" = Bludgeoning
decode "radiation"   = Radiation


-- -- -- -- -- -- -- -- -- 
-- combat

-- |The fight which occurs between the immune system and infection, until one army remains
fight :: Army -> Army -> Army
fight immsys infect
  | totalUnits immsys == 0 = infect
  | totalUnits infect == 0 = immsys
  | survivors == (immsys,infect) = infect 
  | otherwise = uncurry fight survivors 
  where order   = reverse $ sortBy compareGroups (immsys ++ infect)
        targets = targetSelection order immsys infect 
        survivors = attacking (initiativeOrder targets) (immsys,infect)


-- |Interactive fight with printing
fightI :: Army -> Army -> IO (Army)
fightI immsys infect
  | totalUnits immsys == 0 = return infect
  | totalUnits infect == 0 = return immsys
  | otherwise = do 
       let order = reverse $ sortBy compareGroups (immsys ++ infect)
       let targets = targetSelection order immsys infect
       putStrLn "Targets : "
       printLines targets
       _ <- getLine
       let survivors = attacking (initiativeOrder targets) (immsys,infect)
       putStrLn "Immune System : " 
       printLines (fst survivors)
       _ <- getLine
       putStrLn "Infection : "
       printLines (snd survivors)
       _ <- getLine
       putStrLn "Next round" 
       uncurry fightI survivors 


-- |Phase 1 of combat: each Group finds at most 1 target in the opposing Army 
targetSelection :: [Group]                 -- ^Groups selecting a target 
                -> Army                    -- ^Immune system army
                -> Army                    -- ^Infection army 
                -> [(Group,Maybe Group)]   -- ^Targets of each group 
targetSelection [] _ _ = []
targetSelection (g:gs) immune infect
  | infectious g = let t = findTarget g immune
                   in (g,t) : targetSelection gs (remove t immune) infect 
  | otherwise    = let t = findTarget g infect
                   in (g,t) : targetSelection gs immune (remove t infect) 
  where remove (Just x) ys = filter (\y -> y/=x) ys 
        remove Nothing  ys = ys


-- |Phase 2 of combat: each Group deals damage to its target in order 
attacking :: [(Group,Maybe Group)] -> (Army,Army) -> (Army,Army)
attacking [] survivors = removeDead survivors 
attacking ((g,Nothing):gs) survivors = attacking gs survivors
attacking ((g,Just t) :gs) (immsys,infect)
  | infectious g = attacking gs' (removeUnits killed t immsys, infect)
  | otherwise    = attacking gs' (immsys, removeUnits killed t infect)
  where killed = damage g t `div` hp t
        gs' = zip (removeUnits killed t (map fst gs)) (map snd gs) 

-- |Removes n units from a Group, leaving them with at least 0 
removeUnits :: Int -> Group -> Army -> Army 
removeUnits _ _ [] = [] 
removeUnits killed g (a:as)
  | g==a = g {units = max 0 (units g - killed)} : as 
  | True = a : removeUnits killed g as 

removeDead :: (Army,Army) -> (Army,Army) 
removeDead (imm,inf) = (filter (\g -> units g /= 0) imm, filter (\g -> units g /= 0) inf)

-- |Finds the target in the opposing Army that the Group will do the most damage to 
findTarget :: Group -> Army -> Maybe Group 
findTarget g army = case [(t,damage g t) | t <- army] of 
                        [] -> Nothing 
                        ds -> let max = maximum (map snd ds)
                                  prospects = map fst $ filter (\(t,d) -> d==max) ds
                              in if max==0 then Nothing else breakTies' prospects

-- |The order in which a fight will unfold 
initiativeOrder :: [(Group,Maybe Group)] -> [(Group,Maybe Group)]
initiativeOrder xs = reverse $ sortOn (initiative . fst) xs

breakTies' gs
  | length gs == 0 = Nothing
  | otherwise = Just $ last (sortBy compareGroups gs) 

-- |If multiple Groups are given, returns the Group with highest initiative 
breakTies :: [Group] -> Maybe Group
breakTies gs
  | length gs == 0 = Nothing 
  | length gs == 1 = Just $ head gs
  | otherwise      = let max = maximum (map effectivePower gs)
                         prospects = filter (\g -> effectivePower g == max) gs
                     in breakTiesInitiative prospects
 
breakTiesInitiative :: [Group] -> Maybe Group
breakTiesInitiative gs
  | length gs == 0 = Nothing 
  | length gs == 1 = Just $ head gs
  | otherwise      = Just $ head (sortOn initiative gs) 
                 
-- |Calculates the damage an attacking group will do to defenders (not considering overkill)
damage :: Group -> Group -> Int
damage attackers defenders 
  | damageType `elem` immune defenders = 0
  | damageType `elem` weak   defenders = 2 * effectivePower attackers
  | otherwise                          = effectivePower attackers
  where damageType  = snd (attack attackers) 


-- |The total attack power of a group 
effectivePower :: Group -> Int
effectivePower g = units g * fst (attack g) 

-- |Total number of units in an Army
totalUnits :: Army -> Int
totalUnits a = sum (map units a) 

-- |Gives a boost to the attack power of an entire army 
boost :: Int -> Army -> Army
boost n a = map (\g -> g {attack = (fst (attack g) + n, snd (attack g))}) a 


-- -- -- -- -- -- -- -- -- 
-- examples 

exGroup = "18 units each with 729 hit points (weak to fire; immune to cold, slashing) with an attack that does 8 radiation damage at initiative 10"

exGroup' = "2023 units each with 5427 hit points (weak to slashing) with an attack that does 22 slashing damage at initiative 3"


exImmune = [ Group {units=17,hp=5390,immune=[],weak=[Radiation,Bludgeoning],
                   attack=(4507,Fire),initiative=2,infectious=False}
           , Group {units=989,hp=1274,immune=[Fire],weak=[Bludgeoning,Slashing],
                   attack=(25,Slashing),initiative=3,infectious=False}
           ]

exInfect = [ Group {units=801,hp=4706,immune=[],weak=[Radiation],
                   attack=(116,Bludgeoning),initiative=1,infectious=True}
           , Group {units=4485,hp=2961,immune=[Radiation],weak=[Fire,Cold],
                   attack=(12,Slashing),initiative=4,infectious=True}
           ]

armies@[immune1,immune2,infect1,infect2] = (exImmune++exInfect)

