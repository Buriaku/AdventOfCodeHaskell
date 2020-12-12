{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}


import Data.List
import Data.Array
import Data.Foldable
import Data.Char
import Data.Maybe
import Data.Ord
import Data.Function
import Data.Bits
import Control.Concurrent
import Data.Hashable

import GHC.Generics (Generic)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.HashPSQ as HPSQ
import qualified Data.IntMap as IntMap


import AdventOfCodeData

data Base =
 Room Int | Door
 deriving (Show, Eq)

data Wood =
 Ground | Trees | Lumber
 deriving (Eq, Ord, Show)
 
data Flowstate =
 Stagnant | Flowing
 deriving (Eq, Show)

data Operation =
 ADDR | ADDI | MULR | MULI |
 BANR | BANI | BORR | BORI |
 SETR | SETI |
 GTIR | GTRI | GTRR |
 EQIR | EQRI | EQRR
 deriving (Eq, Ord, Bounded, Enum, Show, Read)
 
data Cave =
 Wall | Floor
 deriving (Show, Eq)

-- creature position health elf/goblin
data Creature =
 Creature Point Int Kind
 deriving (Show, Eq)
 
data Kind =
 Elf | Goblin
 deriving (Show, Eq, Ord)
 
data Track =
 Horizontal | Vertical |
 NorthWest | SouthWest | NorthEast | SouthEast |
 Crossing
 deriving (Show, Eq)

data Turning =
 Counterclockwise | Straight | Clockwise
 deriving (Show, Eq)

data Direction =
 North | East | South | West
 deriving (Show, Eq)

data Cart =
 Cart Point Direction Turning
 deriving (Show, Eq)
 
data IntTree =
 IntBranch [IntTree] [Int]
 deriving (Show, Eq)

data Point =
 Point Int Int
 deriving (Show, Eq, Ord, Ix, Generic)

instance Hashable Point

data Rectangle =
 Rectangle Point Int Int
 deriving (Show, Eq, Ord)
 
-- Day 22a

day22a =
 foldl day22a_foldl 0 $ Map.elems day22a_eroMap

day22a_foldl acc x =
 acc + (mod x 3)
 

day22a_eroMap =
 day22a_makeMap Map.empty day22a_points

day22a_makeMap eroMap [] = eroMap
day22a_makeMap eroMap (curr:rest) =
 day22a_makeMap newEroMap rest
 where
  newEroMap = day22a_erosion eroMap curr

day22a_erosion eroMap p@(Point 0 0) =
 Map.insert p (day22a_geoToEro 0) eroMap
day22a_erosion eroMap p@(Point x 0) =
 Map.insert p (day22a_geoToEro $ x * 16807) eroMap
day22a_erosion eroMap p@(Point 0 y) =
 Map.insert p (day22a_geoToEro $ y * 48271) eroMap
day22a_erosion eroMap p@(Point x y)
 | p == day22a_target =
  Map.insert p (day22a_geoToEro 0) eroMap
 | Map.member p eroMap = eroMap
 | otherwise =
  Map.insert p erosion yMinusMap 
 where
  erosion = day22a_geoToEro $ xMinus * yMinus
  xMinusMap =
   day22a_erosion eroMap (Point (x - 1) y)
  xMinus = xMinusMap Map.! (Point (x - 1) y)
  yMinusMap =
   day22a_erosion xMinusMap (Point x (y - 1))
  yMinus = yMinusMap Map.! (Point x (y - 1))

day22a_geoToEro geoInd =
 mod (geoInd + day22a_depth) 20183
 
day22a_points =
 [Point x y | x <- [0..xMax], y <- [0..yMax]]
 where
  (Point xMax yMax) = day22a_target

day22a_depth = 3339
day22a_target = Point 10 715
day22a_mouth = Point 0 0

-- Day 21b

day21b = day21b_calc Set.empty 0 day21a_calc

day21b_calc intSet prev (curr:rest)
 | Set.member curr intSet =
  prev
 | otherwise =
  day21b_calc (Set.insert curr intSet) curr rest
  
-- Day 21a

day21a = head day21a_calc

day21a_calc =
 map (\[_,_,_,x,_,_] -> x) $
  day21a_test [0,0,0,0,0,0]

-- x = 3
-- pass 1 drop 1800
-- [3,256,65536,3809384,0,25]
-- pass 3
-- [3,827,212115,13496537,0,25]

day21a_test list =
 map toList $
  day21a_timeCPU2 $
   listToArray list

day21a_timeCPU2 state
 | newPointer == 28 =
  newState:(day21a_timeCPU2 newState)
 | newPointer > day21a_maxPointer
    || newPointer < 0 = [newState]
 | otherwise =
  --newState:
  (day21a_timeCPU2 newState)
 where
  pointer = state ! day21a_ip
  
  magic1 = (div (state ! 2) 256) - 1
  
  modState = 
   if pointer == 25 && (state ! 1) < magic1
    then
     state // [(1,magic1)]
    else
     state
  
  (op,values) = day21a_instructions ! pointer
  opState = timeCPU_apply op values modState
  
  newPointer = (opState ! day21a_ip) + 1
  newState = opState // [(day21a_ip,newPointer)]
  
day21a_maxPointer = snd $ bounds day21a_instructions

day21a_instructions =
 listToArray day21a_instructionList

day21a_instructionList =
 map getInstruction $ tail day21a_split
 where
  getInstruction string =
   (op,values)
   where
    split = splitOn ' ' string
    op =
     read $ map toUpper $ head split :: Operation
    values = map read $ tail split :: [Int]

day21a_ip =
 read $ filter isNumber $ head day21a_split :: Int

day21a_split = splitOn ';' data21
 
-- Day 20b

day20b = length day20b_calc

day20b_calc = filter (\(Room a) -> a >= 1000) rooms
 where
  rooms = filter filterRooms $ Map.elems day20a_map
  filterRooms (Room _) = True
  filterRooms _ = False
 
-- Day 20a

day20a = (\(Room a) -> a) day20a_calc

day20a_calc =
 maximumBy (\(Room a) (Room b) -> compare a b) rooms
 where
  rooms = filter filterRooms $ Map.elems day20a_map
  filterRooms (Room _) = True
  filterRooms _ = False

day20a_map =
 day20a_spool (Map.singleton day20a_start (Room 0))  0 day20a_start $ init $ tail data20

day20a_spool baseMap _ _ [] = baseMap
day20a_spool baseMap doors p string@(curr:rest)
 | elem curr "NESW" =
  day20a_spool nextBaseMap nextDoors nextRoom rest
 | otherwise =
  day20a_fork baseMap doors p string
 where
  direction = charToDirection curr
  nextDoors = doors + 1
  nextDoor = movePoint p direction
  nextRoom = movePoint nextDoor direction
  nextBaseMap =
   Map.insertWith day20a_insert nextRoom (Room nextDoors) $
    Map.insert nextDoor Door baseMap
   
  charToDirection 'N' = North
  charToDirection 'E' = East
  charToDirection 'S' = South
  charToDirection 'W' = West

day20a_insert Door Door = Door
day20a_insert (Room a) (Room b) = Room $ min a b

day20a_fork baseMap doors p string =
 day20a_spool newBaseMap doors p rest
 where
  parsed = day20a_parseFork 0 "" [] string
  rest = head parsed
  forkList = tail parsed
  baseMaps = map (day20a_spool Map.empty doors p) forkList
  newBaseMap = Map.unionsWith day20a_insert $ baseMap:baseMaps

day20a_parseFork level reverseFork reverseList (curr:rest)
 | curr == '(' && level == 0 =
  day20a_parseFork (level + 1) reverseFork reverseList rest
 | curr == '(' =
  day20a_parseFork (level + 1) (curr:reverseFork) reverseList rest
 | curr == '|' && level == 1 =
  day20a_parseFork level "" ((reverse reverseFork):reverseList) rest
 | curr == ')' && level == 1 =
  rest:(reverse $ (reverse reverseFork):reverseList)
 | curr == ')' =
  day20a_parseFork (level - 1) (curr:reverseFork) reverseList rest
 | otherwise =
  day20a_parseFork level (curr:reverseFork) reverseList rest
  

day20a_start = Point 0 0
 
-- Day 19b

day19b = day19b_output ! 0

-- shortcut
day19b_output =
 timeCPU_process $
  listToArray
   --[0,8,10551261,0,10551261,10551261]
   [16078144,9,10551261,0,10551261,10551261]

day19b_output2 =
 timeCPU_process2 $
  listToArray
   --[1,0,0,0,0,0]
   --[0,9,10551261,0,10551261,1]
   --[1,9,(div 10551261 3),0,10551261,3]
   --[4,9,(div 10551261 7),0,10551261,7]
   --[11,9,(div 10551261 502441),0,10551261,502441]
   --[11,9,(div 10551261 502441),0,10551261,502441]
   --[502452,9,10551261,0,10551261,10551261]
   --[5526883,9,1,0,10551261,10551261]
   [16078144,9,10551261,0,10551261,10551261]

   --[1,9,10551261,0,10551261,10551261]
   
   --1+3+7+21+(502441)*(1+3+7+21) = 16078144
  
timeCPU_process2 state
 | newPointer > day19a_maxPointer
    || newPointer < 0 = [newState]
 | otherwise =
  newState:(timeCPU_process2 newState)
 where
  pointer = (state ! day19a_ip)
  
  (op,values) = day19a_instructions ! pointer
  opState = timeCPU_apply op values state
  
  newPointer = (opState ! day19a_ip) + 1
  newState = opState // [(day19a_ip,newPointer)]

-- Day 19a

day19a = day19a_output ! 0

day19a_output =
 timeCPU_process $
  listToArray [0,0,0,0,0,0]

timeCPU_process state
 | newPointer > day19a_maxPointer
    || newPointer < 0 = newState
 | otherwise =
  timeCPU_process newState
 where
  pointer = (state ! day19a_ip)
  
  (op,values) = day19a_instructions ! pointer
  opState = timeCPU_apply op values state
  
  newPointer = (opState ! day19a_ip) + 1
  newState = opState // [(day19a_ip,newPointer)]

timeCPU_apply operation [valA,valB,valC] state =
 case operation of
  ADDR -> state // [(valC,regA + regB)]
  ADDI -> state // [(valC,regA + valB)]
  MULR -> state // [(valC,regA * regB)]
  MULI -> state // [(valC,regA * valB)]
  BANR -> state // [(valC,regA .&. regB)]
  BANI -> state // [(valC,regA .&. valB)]
  BORR -> state // [(valC,regA .|. regB)]
  BORI -> state // [(valC,regA .|. valB)]
  SETR -> state // [(valC,regA)]
  SETI -> state // [(valC,valA)]
  GTIR -> state // [(valC,boolToInt $ valA > regB)]
  GTRI -> state // [(valC,boolToInt $ regA > valB)]
  GTRR -> state // [(valC,boolToInt $ regA > regB)]
  EQIR -> state // [(valC,boolToInt $ valA == regB)]
  EQRI -> state // [(valC,boolToInt $ regA == valB)]
  EQRR -> state // [(valC,boolToInt $ regA == regB)]
 where
  regA = state ! valA
  regB = state ! valB
  boolToInt False = 0
  boolToInt True  = 1

day19a_maxPointer = snd $ bounds day19a_instructions

day19a_instructions =
 listToArray day19a_instructionList

day19a_instructionList =
 map getInstruction $ tail day19a_split
 where
  getInstruction string =
   (op,values)
   where
    split = splitOn ' ' string
    op =
     read $ map toUpper $ head split :: Operation
    values = map read $ tail split :: [Int]

day19a_ip =
 read $ filter isNumber $ head day19a_split :: Int

day19a_split = splitOn ';' data19

-- Day 18b

day18b = treeCount * lumberCount
 where
  output = day18b_output
  elements = elems output
  treeCount = length $ filter (== Trees) elements
  lumberCount = length $ filter (== Lumber) elements

 
day18b_output =
 day18a_iterate
  ((mod (day18b_time - day18b_init) day18b_period) + day18b_init)
  day18a_woodArray

day18b_period = (snd day18b_cycles) - day18b_init
day18b_init = fst day18b_cycles

day18b_cycles =
 day18b_getPeriod 0 day18a_woodArray Map.empty

day18b_getPeriod n woodArray prevArrayMap
 | Map.member woodArray prevArrayMap =
  (prevArrayMap Map.! woodArray,n)
 | otherwise =
  day18b_getPeriod (n + 1) nextWoodArray $
     Map.insert woodArray n prevArrayMap
 where
  range = bounds woodArray
  assocList = assocs woodArray
  nextWoodArray =
   array range $ map (day18a_update woodArray) assocList
   
day18b_time = 1000000000 :: Int

-- Day 18a
 
day18a = treeCount * lumberCount
 where
  output = day18a_output
  elements = elems output
  treeCount = length $ filter (== Trees) elements
  lumberCount = length $ filter (== Lumber) elements

 
day18a_output = day18a_iterate 10 day18a_woodArray

day18a_iterate 0 woodArray = woodArray
day18a_iterate n woodArray =
 day18a_iterate (n - 1) nextWoodArray
 where
  range = bounds woodArray
  assocList = assocs woodArray
  nextWoodArray =
   array range $ map (day18a_update woodArray) assocList

day18a_update woodArray (p,wood)
 | wood == Ground && treeCount > 2 =
  (p,Trees)
 | wood == Ground =
  (p,Ground)
 | wood == Trees && lumberCount > 2 =
  (p,Lumber)
 | wood == Trees =
  (p,Trees)
 | wood == Lumber && lumberCount > 0 && treeCount > 0 =
  (p,Lumber)
 | wood == Lumber =
  (p,Ground)
 where
  candidates = day18a_sanitize $ surroundingPoints p
  surrounding = map (woodArray !) candidates
  treeCount = length $ filter (== Trees) surrounding
  lumberCount = length $ filter (== Lumber) surrounding

day18a_sanitize [] = []
day18a_sanitize (p@(Point x y):rest)
 | x >= 0 && y >= 0 && x <= day18a_xMax && y <= day18a_yMax =
  p:(day18a_sanitize rest)
 | otherwise =
  day18a_sanitize rest
 
day18a_woodArray =
 array (Point 0 0,Point day18a_xMax day18a_yMax) day18a_list
 
    
-- day18a_woodToChar Floor = '.'
-- day18a_woodToChar Wall  = '#'
-- day18a_woodToChar Wall  = '#'
 
day18a_list = assocList
 where
  coords =
   [Point x y | y <- [0..day18a_yMax], x <- [0..day18a_xMax]]
  assocList = zip coords $ map charToWood $ concat day18a_split
  charToWood '.' = Ground
  charToWood '|' = Trees
  charToWood '#' = Lumber

day18a_xMax = day18a_xLength - 1
day18a_yMax = day18a_yLength - 1
day18a_xLength = length $ head day18a_split
day18a_yLength = length day18a_split

day18a_split = splitOn ';' data18

-- Day 17b

day17b =
 length $ filter (== Stagnant) $
  Map.elems day17a_waterMap

-- Day 17a

day17a = (Map.size day17a_waterMap) - 1

day17a_toFile =
 do
  writeFile "day17a.txt" $
   concat $ map (++ "\n") day17a_getLines
  return ()

day17a_print =
  do
  mapM putStrLn day17a_getLines
  return ()

day17a_getLines =
 [[Map.findWithDefault ' ' (Point x y) day17a_printMap |
    x <- [day17a_xMin - 2..day17a_xMax + 2]] |
     y <- [day17a_yMin - 2..day17a_yMax + 2]]

day17a_printMap = Map.union water clay -- water
 where
  clay =
   Map.fromList $ zip
    day17a_pointsList $
    repeat '#'
  water =
   fmap flowStateToChar day17a_waterMap
  flowStateToChar Flowing  = '|' 
  flowStateToChar Stagnant = '~'

day17a_waterMap =
 snd' $ day17a_flowDown Map.empty day17a_start

day17a_cutoff = day17a_yMax
 
day17a_start = Point 500 $ day17a_yMin - 1

day17a_flowDown waterMap p@(Point x y)
 | y > day17a_cutoff =
  (Flowing,waterMap,[])
 | Map.member p waterMap =
  (checkedStatus,waterMap,[])
 | isClay =
  (Stagnant,waterMap,[])
 | downStagnant && leftRightStagnant =
  (Stagnant,newWaterMap,layer)
 | downStagnant = 
  (Flowing,newWaterMap,[])
 | otherwise =
  (Flowing,Map.insert p Flowing downWaterMap,[])
 where
  isClay = Set.member p day17a_claySet
 
  down = movePoint p South
  flowDown = day17a_flowDown waterMap down
  downStagnant = fst' flowDown == Stagnant
  downWaterMap = snd' flowDown
  downLayer = trd' flowDown
  
  left = movePoint p West
  flowLeft =
   day17a_flowHorizontal downWaterMap West downLayer left
  leftStagnant = fst' flowLeft == Stagnant
  leftWaterMap = snd' flowLeft
  leftLayer = trd' flowLeft
  
  right = movePoint p East
  flowRight =
   day17a_flowHorizontal leftWaterMap East downLayer right
  rightStagnant = fst' flowRight == Stagnant 
  rightWaterMap = snd' flowRight
  rightLayer = trd' flowRight
  
  leftRightStagnant = leftStagnant && rightStagnant
  leftRightStatus
   | leftRightStagnant = Stagnant
   | otherwise         = Flowing
  
  layer = p:leftLayer ++ rightLayer
  newWaterMap =
   foldl
    (\acc x -> Map.insert x leftRightStatus acc)
    rightWaterMap layer
  
  
  checkedStatus = waterMap Map.! p

day17a_flowHorizontal waterMap direction downLayer p
 | isClay =
  (Stagnant,waterMap,[])
 | downStagnant =
  (horiStatus,horiWaterMap,horiLayerNew)
  -- (horiStatus,newWaterMap,horiLayerNew)
 | otherwise =
  (Flowing,downWaterMap,[p])
  -- (Flowing,Map.insert p Flowing downWaterMap,[])
 where
  isClay = Set.member p day17a_claySet
 
  down = movePoint p South
  flowDown
   | elem down downLayer =
    (Stagnant,waterMap,[])
   | otherwise =
    day17a_flowDown waterMap down
  downStagnant = fst' flowDown == Stagnant
  downWaterMap = snd' flowDown
  downLayerAdd = trd' flowDown
  downLayerNew = downLayerAdd ++ downLayer
  
  hori = movePoint p direction
  flowHori =
   day17a_flowHorizontal
    downWaterMap direction downLayerNew hori
  horiStatus = fst' flowHori
  horiWaterMap = snd' flowHori
  horiLayer = trd' flowHori
  horiLayerNew = p:horiLayer
  
  -- newWaterMap = Map.insert p horiStatus horiWaterMap

day17a_xMin = minimum day17a_xs
day17a_xMax = maximum day17a_xs

day17a_yMin = minimum day17a_ys
day17a_yMax = maximum day17a_ys

day17a_xs = map (\(Point x y) -> x) day17a_pointsList
day17a_ys = map (\(Point x y) -> y) day17a_pointsList

day17a_claySet =
 Set.fromList day17a_pointsList
 
day17a_pointsList = concat $ map getPoints day17a_data
 where
  getPoints ('x',n,m1,m2) = [Point n m | m <- [m1..m2]]
  getPoints ('y',n,m1,m2) = [Point m n | m <- [m1..m2]]

day17a_data = map getData day17a_split
 where
  getData entry =
   (static,n,m1,m2)
   where
   static = head entry
   split = splitOnList ",." entry
   n = readInt $ filter isNumber $ split !! 0
   m1 = readInt $ filter isNumber $ split !! 1
   m2 = readInt $ filter isNumber $ split !! 3

day17a_split = splitOn ';' data17

-- Day 16b

day16b = toList day16b_output

day16b_output = day16b_calc day16b_input $ listToArray [0,0,0,0]

day16b_calc [] state = state
day16b_calc ((opCode:values):rest) state =
 day16b_calc rest newState
 where
  newState =
   day16a_apply (day16b_opMap Map.! opCode) values state

day16b_opMap = Map.fromList $ day16b_seive day16b_reduced

day16b_seive assocs
 | length resolved == length day16a_operations =
  map (\(a,[b]) -> (a,b) ) assocs
 | otherwise = day16b_seive nextAssocs
  where
   resolved =
    concat $ filter (\x -> length x == 1) $ snd $ unzip assocs
   nextAssocs = map seive assocs
   seive (a,[b]) = (a,[b])
   seive (a,b) = (a,b \\ resolved)
   

day16b_reduced = map reduce day16b_grouped
 where
  reduce list = (opCode,reduced)
   where
    unzipped = unzip list
    opCode = head $ fst unzipped
    codes = snd unzipped
    reduced = foldl1 (\acc x -> intersect x acc) codes
    
day16b_input = spoolList 4 data16b

day16b_grouped = groupBy (equaling fst) day16b_nubSort

day16b_nubSort = nub $ sort day16a_calc

-- Day 16a
 
day16a = length day16a_output
 
day16a_output = filter (\(x,y) -> length y >= 3) day16a_calc

day16a_calc = map day16a_check day16a_input

day16a_check example@(before,instruction,after) = (opCode,possible)
 where
  list = tail instruction
  opCode = head instruction
  appliedList =
   map (\op -> day16a_apply op list before) day16a_operations
  boolList = map (== after) appliedList
  filtered =
   filter (\(a,b) -> b) $ zip day16a_operations boolList
  possible = fst $ unzip filtered
  

day16a_apply operation [valA,valB,valC] state =
 case operation of
  ADDR -> state // [(valC,regA + regB)]
  ADDI -> state // [(valC,regA + valB)]
  MULR -> state // [(valC,regA * regB)]
  MULI -> state // [(valC,regA * valB)]
  BANR -> state // [(valC,regA .&. regB)]
  BANI -> state // [(valC,regA .&. valB)]
  BORR -> state // [(valC,regA .|. regB)]
  BORI -> state // [(valC,regA .|. valB)]
  SETR -> state // [(valC,regA)]
  SETI -> state // [(valC,valA)]
  GTIR -> state // [(valC,boolToInt $ valA > regB)]
  GTRI -> state // [(valC,boolToInt $ regA > valB)]
  GTRR -> state // [(valC,boolToInt $ regA > regB)]
  EQIR -> state // [(valC,boolToInt $ valA == regB)]
  EQRI -> state // [(valC,boolToInt $ regA == valB)]
  EQRR -> state // [(valC,boolToInt $ regA == regB)]
 where
  regA = state ! valA
  regB = state ! valB
  boolToInt False = 0
  boolToInt True  = 1

day16a_input = map transform entries
 where
  split = splitOn ';' data16a
  entries = spoolList 4 split
  transform [a,b,c,_] = (d,e,f)
   where
    d =
     listToArray
      (read $ filter (`elem` "[,]0123456789") a :: [Int])
    e = map read $ splitOn ' ' b :: [Int]
    f =
     listToArray
      (read $ filter (`elem` "[,]0123456789") c :: [Int])

day16a_operations = enumFrom minBound :: [Operation]

-- Day 15b

day15b = fullRounds * hpSum
 where
  (weapon,rounds,elves,creatures) = last day15b_getOutput
  fullRounds = rounds - 1
  hpSum = sum $ map (\(Creature _ hp _) -> hp) creatures
  
day15b_getOutput = day15b_output 4
  
day15b_output weapon
 | finalElves == day15b_elves =
  output
 | otherwise =
  output ++ (day15b_output (weapon + 1))
 where
  output = day15b_iterate weapon 1 [] day15a_creatures
  finalElves = (\(_,_,e,_) -> e) $ last output

-- delay of 200000 to 500000 microseconds is reasonable
day15b_print delay =
 do
  sequence $ concat $
   map (\x -> (threadDelay delay):(map putStrLn x)) $
    map day15b_string day15b_getOutput
  return ()

day15b_string (weapon,n,elves,creatures) =
 "":("Weapon: " ++ (show weapon) ++ ", Round: " ++ (show n) ++ ", Elves: " ++ (show elves)):strings
 --(show creatureHP):strings
 where
  creaturePos = map (\(Creature p _ _) -> p) creatures
  creatureHP = map (\(Creature _ h _) -> h) creatures
  creatureChar =
   map (\(Creature _ _ k) -> day15a_kindToChar k) creatures
  creatureMap = Map.fromList $ zip creaturePos creatureChar 
  
  strings =
   [[Map.findWithDefault
     (day15a_caveToChar $ day15a_caveMap Map.! (Point x y))
     (Point x y) creatureMap |
      x <- [0..day15a_xMax]] | y <- [0..day15a_yMax]]
 
-- day15b_iterate ::
-- Int -> Int -> [Creature] -> [(Int,Int,Int,[Creature])]
day15b_iterate weapon n prevPos creatures
 | ended || newElves < day15b_elves =
  [(weapon,n,newElves,nextCreatures)]
 | otherwise =
  (weapon,n,newElves,nextCreatures):
   (day15b_iterate weapon (n + 1) currPos nextCreatures)
 where
  (ended,nextCreatures) =
   day15a_doStep weapon prevPos creatures
  newElves = day15b_getElves nextCreatures
  currPos = day15a_getPos creatures
   
day15b_elves = day15b_getElves day15a_creatures
 
day15b_getElves creatures =
 length $ filter (\(Creature _ _ k) -> k == Elf) creatures

-- Day 15a
 
day15a = fullRounds * hpSum
 where
  (rounds,n,creatures) = last day15a_output
  fullRounds = rounds - 1
  hpSum = sum $ map (\(Creature _ hp _) -> hp) creatures
  
day15a_output = day15a_iterate 1 [] day15a_creatures

day15a_print =
 do
  mapM putStrLn $ concat $
   map day15a_string
    ((0,length day15a_creatures,day15a_creatures):day15a_output)
  return ()

day15a_toFiles =
 do
  mapM day15a_toFile $
   map day15a_string
    ((0,length day15a_creatures,day15a_creatures):day15a_output)
  return ()

day15a_toFile strings =
 do
  writeFile ((head $ tail strings) ++ ".txt") $
   concat $ map (++ "\n") $ (head $ tail strings):(tail $ tail $ tail strings)
  return ()

day15a_string (n,_,creatures) = "":(show n):(show creatureHP):strings
 where
  creaturePos = map (\(Creature p _ _) -> p) creatures
  creatureHP = map (\(Creature _ h _) -> h) creatures
  creatureChar =
   map (\(Creature _ _ k) -> day15a_kindToChar k) creatures
  creatureMap = Map.fromList $ zip creaturePos creatureChar 
  
  strings =
   [[Map.findWithDefault
     (day15a_caveToChar $ day15a_caveMap Map.! (Point x y))
     (Point x y) creatureMap |
      x <- [0..day15a_xMax]] | y <- [0..day15a_yMax]]
 
day15a_iterate n prevPos creatures
 | ended =
  [(n,length nextCreatures,nextCreatures)]
 | otherwise =
  (n,length nextCreatures,nextCreatures):(day15a_iterate (n + 1) currPos nextCreatures)
 where
  (ended,nextCreatures) = day15a_doStep 3 prevPos creatures
  currPos = day15a_getPos creatures
    
day15a_doStep weapon prevPos creatures =
 day15a_step weapon False prevPos creatures creatures

day15a_step weapon ended _ [] creatures = (ended,day15a_sortCreatures creatures)
day15a_step weapon ended prevPos
 (current@(Creature pos hp kind):restQueue) creatures =
 day15a_step weapon newEnded prevPos nextQueue nextCreatures
  where
   -- common
   others = delete current creatures
   currPos = day15a_getPos creatures
   blocked = delete pos currPos
   combatLocked = currPos == prevPos
   
   enemies = filter (\(Creature _ _ k) -> k /= kind) others
   enemyPos = day15a_getPos enemies
   
   newEnded
    | ended     = ended
    | otherwise = enemies == []
   
   targets = concat $ map (day15a_options blocked) enemyPos
   
   -- move
   posMoved
    | combatLocked =
     pos
    | otherwise =
     day15a_move blocked targets pos
   
   currentMoved = Creature posMoved hp kind
   
   -- attack
   
   inPosition = elem posMoved targets
   
   (target,harmed)
    | inPosition =
     day15a_attack weapon kind enemies posMoved
    | otherwise =
     (current,current) -- dummy value
   
   -- cleanup   
   nextCreatures
    | inPosition && day15a_isDead harmed =
     currentMoved:(delete target others)
    | inPosition =
     currentMoved:harmed:(delete target others)
    | otherwise =
     currentMoved:others
   
   nextQueue
    | inPosition && day15a_isDead harmed =
     delete target restQueue
    | inPosition =
     map
      (\c -> if c == target then harmed else c)
      restQueue
    | otherwise =
     restQueue

day15a_move blocked targets pos = nextPos
 where
  path = day15a_getPath blocked targets pos
  
  nextPos
   | length path < 2 = pos
   | otherwise       = last $ init path

day15a_attack weapon kind enemies pos = (target,harmed)
 where
  inReach =
   filter
    (\(Creature p _ _) -> (manhattanDistance p pos) == 1)
    enemies
  
  minimumHP =
   minimum $ map
    (\(Creature _ hp _) -> hp)
    inReach
    
  filteredByHP =
   filter
    (\(Creature _ h _) -> h == minimumHP)
    inReach
  
  target = head $ day15a_sortCreatures filteredByHP
  harmed
   | kind == Elf = day15a_harmCreature target weapon
   | otherwise   = day15a_harmCreature target 3

day15a_harmCreature (Creature pos hp kind) damage =
 Creature pos (hp - damage) kind

day15a_isDead (Creature _ hp _) = hp < 1

day15a_getPos creatures =
 map (\(Creature pos _ _) -> pos) creatures

day15a_getPath blocked targets pos@(Point x y)
 | elem pos targets = [pos]
 | options == [] = []
 | otherwise =
  day15a_queuingAlgorithm blocked targets [pos] hashPSQ
 where
  options = day15a_options blocked pos
  hashPSQ =
   foldl 
    (\acc p@(Point x y) -> HPSQ.insert (p,y,x) (1,y,x,y,x) [p,pos] acc)
    HPSQ.empty
    options    

day15a_queuingAlgorithm blocked targets checked hashPSQ
 | elem pos targets =
  path
 | HPSQ.null nextHashPSQ = -- no options left and no targets reached
  []
 | otherwise =
  day15a_queuingAlgorithm blocked targets nextChecked nextHashPSQ
 where
  (key@(pos,y1,x1),(distance,_,_,_,_),path) = fromJust $ HPSQ.findMin hashPSQ
  
  alreadyChecked = elem pos checked
  
  options
   | alreadyChecked = []
   | otherwise =
    (day15a_options blocked pos)
  
  
  nextChecked
   | alreadyChecked = checked
   | otherwise =
    pos:checked
  
  -- nextPaths :: [[Point]]
  nextPaths = map (:path) options
  
  nextKPVAssocs =
   map (\path@(head@(Point x y):_) -> ((head,y1,x1),(distance + 1,y,x,y1,x1),path)) nextPaths
  
  hashPSQCleared =
   HPSQ.delete key hashPSQ
  
  nextHashPSQ =
   foldl (\acc (k,p,v) -> HPSQ.insert k p v acc)
    hashPSQCleared nextKPVAssocs

day15a_options blocked pos =
 (filter day15a_isFloor $ adjacentPoints pos) \\ blocked      

day15a_sortCreatures creatures =
 sortBy
  (\(Creature (Point a b) _ _) (Creature (Point c d) _ _) ->
   compare (b,a) (d,c))
   creatures
   
day15a_creatures = creatures
 where
  filteredList = filter ((`elem` "EG") . snd) day15a_list
  creatures =
   map
    (\(p,char) -> Creature p 200 $ day15a_charToKind char)
    filteredList

day15a_charToKind 'E' = Elf
day15a_charToKind 'G' = Goblin

day15a_kindToChar Elf = 'E'
day15a_kindToChar Goblin = 'G'
    
day15a_isFloor p
 | cave == Floor = True
 | otherwise = False
 where
  cave = day15a_caveMap Map.! p

day15a_caveMap = fmap charToCave $ Map.fromList day15a_list
 where
  charToCave char
   | elem char ".EG" =
    Floor
   | otherwise =
    Wall
    
day15a_caveToChar Floor = '.'
day15a_caveToChar Wall  = '#'
 
day15a_list = assocList
 where
  coords = [Point x y | y <- [0..day15a_yMax], x <- [0..day15a_xMax]]
  assocList = zip coords $ concat day15a_split

day15a_xMax = (length $ head day15a_split) - 1
day15a_yMax = (length day15a_split) - 1
day15a_xLength = length $ head day15a_split
day15a_yLength = length day15a_split

day15a_split = splitOn ';' data15

-- Day 14b

day14b = (last day14b_output) - day14b_len

day14b_output = day14b_iterate 0 1 day14a_start day14a_len

day14b_iterate elfIndex1 elfIndex2 sequence len
 -- | failed =
 -- error $ "lengths don't match: " ++ (show day14b_len) ++ " " ++ (show nextLen) ++ " " ++ (show $ length sequenceTail1) 
 | sequenceTail1 == day14b_seq =
  [nextLen]
 | sequenceTail2 == day14b_seq =
  [nextLen-1]
 | mod len 100000 == 0 =
  len:(day14b_iterate nextElfIndex1 nextElfIndex2 nextSequence nextLen)
 | otherwise =
  day14b_iterate nextElfIndex1 nextElfIndex2 nextSequence nextLen
 where
  recipeElf1 = Seq.index sequence elfIndex1
  recipeElf2 = Seq.index sequence elfIndex2
  recipeSum = recipeElf1 + recipeElf2
  
  newRecipes
   | recipeSum > 9 =
    (\(a,b) -> [a,b]) $ divMod recipeSum 10
   | otherwise =
    [recipeSum]
  
  nextSequence = sequence Seq.>< (Seq.fromList newRecipes)
  nextLen = len + (length newRecipes) 
 
  nextElfIndex1 = mod (elfIndex1 + 1 + recipeElf1) nextLen
  nextElfIndex2 = mod (elfIndex2 + 1 + recipeElf2) nextLen
  
  sequenceTail1 = Seq.drop (nextLen - day14b_len) nextSequence
  sequenceTail2 =
   Seq.take day14b_len $
    Seq.drop (nextLen - day14b_len - 1) nextSequence
  
  -- failed = day14b_len /= length sequenceTail && nextLen > day14b_len

day14b_len = length day14b_seq

day14b_seq :: Seq.Seq Int
day14b_seq = Seq.fromList $ map (read . (:[])) $ show data14 

-- Day 14a

day14a = Seq.take 10 $ Seq.drop data14 day14a_output 

day14a_output = day14a_iterate 0 1 day14a_start day14a_len

day14a_iterate elfIndex1 elfIndex2 sequence len
 | len >= data14 + 10 =
  sequence
 | otherwise =
  day14a_iterate nextElfIndex1 nextElfIndex2 nextSequence nextLen
 where
  recipeElf1 = Seq.index sequence elfIndex1
  recipeElf2 = Seq.index sequence elfIndex2
  recipeSum = recipeElf1 + recipeElf2
  
  newRecipes
   | recipeSum > 9 =
    (\(a,b) -> [a,b]) $ divMod recipeSum 10
   | otherwise =
    [recipeSum]
  
  nextSequence = sequence Seq.>< (Seq.fromList newRecipes)
  nextLen = len + (length newRecipes) 
 
  nextElfIndex1 = mod (elfIndex1 + 1 + recipeElf1) nextLen
  nextElfIndex2 = mod (elfIndex2 + 1 + recipeElf2) nextLen
  
day14a_len = length day14a_start

day14a_start = Seq.fromList [3,7]

-- sequence Seq.|> element || sequence Seq.>< sequence

-- Day 13b

day13b = point
 where
  output = day13b_iterate 0 day13a_carts
  point = (\(Cart p _ _) -> p) $ snd output 
 
day13b_iterate n carts
 | length nextCarts == 1 =
  (n,head nextCarts)
 | otherwise =
  day13b_iterate (n + 1) nextCarts
 where
  nextCarts = day13b_step (day13a_sort carts) []

day13b_step [] prevCarts = prevCarts
day13b_step (current:nextCarts) prevCarts =
 day13b_step newNextCarts newPrevCarts
 where
  updated = day13a_move current
  crash = day13b_check updated (nextCarts ++ prevCarts)
  newNextCarts = nextCarts \\ crash
  newPrevCarts = (updated:prevCarts) \\ crash

day13b_check a [] = []
day13b_check cart@(Cart p1 _ _) (current@(Cart p2 _ _):next)
 | p1 == p2 =
  [cart,current]
 | otherwise =
  day13b_check cart next

-- Day 13a

day13a = point
 where
  output = day13a_iterate 0 day13a_carts
  point = (\(Cart p _ _) -> p) $ head $ snd output
 
day13a_iterate n carts
 | crashed =
  (n,nextCarts)
 | otherwise =
  day13a_iterate (n + 1) nextCarts
 where
  (crashed,nextCarts) = day13a_step (day13a_sort carts) []

day13a_step [] prevCarts = (False,prevCarts)
day13a_step (current:nextCarts) prevCarts
 | crashed =
  (True,updated:prevCarts)
 | otherwise =
  day13a_step nextCarts $ updated:prevCarts
 where
  updated = day13a_move current
  crashed =
   day13a_check updated prevCarts || 
   day13a_check updated nextCarts
   
day13a_check _ [] = False
day13a_check cart@(Cart p1 _ _) ((Cart p2 _ _):next)
 | p1 == p2 =
  True
 | otherwise =
  day13a_check cart next
 
  
day13a_move cart@(Cart point@(Point x y) direction turning)
 | track == Vertical || track == Horizontal =
  Cart nextPoint direction turning
 | track == NorthWest && direction == South =
  Cart nextPoint West turning
 | track == NorthWest =
  Cart nextPoint North turning
 | track == NorthEast && direction == South =
  Cart nextPoint East turning
 | track == NorthEast =
  Cart nextPoint North turning
 | track == SouthWest && direction == North =
  Cart nextPoint West turning
 | track == SouthWest =
  Cart nextPoint South turning
 | track == SouthEast && direction == North =
  Cart nextPoint East turning
 | track == SouthEast =
  Cart nextPoint South turning
 | track == Crossing && turning == Straight =
  Cart nextPoint direction nextTurning
 | track == Crossing =
  Cart nextPoint nextDirection nextTurning
 where
  nextPoint = movePoint point direction
  track = day13a_trackMap Map.! nextPoint
  nextDirection = turnDirection direction turning
  nextTurning = day13a_nextTurning turning

day13a_nextTurning Counterclockwise = Straight
day13a_nextTurning Straight = Clockwise
day13a_nextTurning Clockwise = Counterclockwise

day13a_sort carts =
 sortBy
  (\(Cart (Point a b) _ _) (Cart (Point c d) _ _) ->
   compare (b,a) (d,c))
  carts


day13a_carts =
 map day13a_getCart $
  filter (\(a,b) -> elem b "^v<>") day13a_list

day13a_getCart (p,char)
 | char == '<' =
  Cart p West Counterclockwise
 | char == '>' =
  Cart p East Counterclockwise
 | char == '^' =
  Cart p North Counterclockwise
 | char == 'v' =
  Cart p South Counterclockwise

day13a_trackMap =
 Map.fromList $ map day13a_toTrack day13a_list
 
day13a_toTrack (p,char) = (p,track)
 where
  north = Map.findWithDefault ' ' (movePoint p North) day13a_map
  east = Map.findWithDefault ' ' (movePoint p East) day13a_map
  west = Map.findWithDefault ' ' (movePoint p West) day13a_map 
  track
   | elem char "|^v" =
    Horizontal
   | elem char "-<>" =
    Vertical
   | char == '+' =
    Crossing
   | char == '/' && elem north "+|" && elem west "+-" =
    NorthWest
   | char == '/' =
    SouthEast
   | char == '\\' && elem north "+|" && elem east "+-" =
    NorthEast
   | char == '\\' =
    SouthWest
    
    
day13a_map = Map.fromList day13a_list
 
day13a_list = filter (\(a,b) -> b /= ' ') assocList
 where
  coords = [Point x y | y <- [0..day13a_yMax], x <- [0..day13a_xMax]]
  assocList = zip coords $ concat day13a_split

day13a_xMax = (length $ head day13a_split) - 1
day13a_yMax = (length day13a_split) - 1
day13a_xLength = length $ head day13a_split
day13a_yLength = length day13a_split

day13a_split = splitOn ';' data13

-- Day 12b

day12b = day12b_getSum index list
 where
  (referenceIndex,list) = day12b_output 1000
  offset = 1000 - referenceIndex
  index = 50000000000 - offset

day12b_getSum index list = sum $ fst $ unzip filtered
 where
  zipped = zip [index..] list
  filtered = filter (\(a,b) -> b == 1) zipped

day12b_output n = (index,trimmed)
 where
  output = day12a_iterate n 0 day12a_initial
  index = fst output
  list = snd output

  zipped = zip [index..] list
  filtered = filter (\(a,b) -> b == 1) zipped
  
  trimmed =
   reverse $ snd $
    break (/= 0) $ reverse list
  
  string = filter isNumber $ show trimmed

-- Day 12a

day12a = sum $ fst $ unzip filtered
 where
  index = fst day12a_output
  list = snd day12a_output
  zipped = zip [index..] list
  filtered = filter (\(a,b) -> b == 1) zipped

day12a_output = day12a_iterate 20 0 day12a_initial

day12a_iterate 0 index list = (index,list)
day12a_iterate n index list =
 day12a_iterate (n - 1) nextIndex nextList
 where
  nextListRaw = day12a_fold $ 0:0:0:0:0:list
  nextListSplit = break (/= 0) nextListRaw
  nextList = snd nextListSplit
  nextIndex = index - 3 + (length $ fst nextListSplit)
  
day12a_fold list         
 | list == [] || list == [0,0,0,0,0] =
  [output]
 | otherwise =
  output:(day12a_fold $ tail list)
 where
  binary = takeWithDefault 0 5 list
  number = fromBinaryList binary
  output =
   if elem number day12a_rules
    then 1
    else 0

day12a_initial =
 map (\x -> if x == '#' then 1 else 0) day12a_initialString
day12a_initialString = filter (`elem` "#.") $ head day12a_split

day12a_rules :: [Int]
day12a_rules =
 map fromBinaryList $
  map (map (\x -> if x == '#' then 1 else 0)) $
   fst $ unzip day12a_rulesString

day12a_rulesString =
 filter (\(a,b) -> b == "#") $
  map (splitAt 5) $
   map (filter (`elem` "#.")) $
    drop 2 day12a_split

day12a_split = splitOn ';' data12

-- Day 11b

day11b = maximumBy (comparing snd) $ day11b_output
 -- $ Map.toList day11b_output

day11b_output =
 takeWhileAscendingBy (comparing snd) $
  day11b_iterate 2 Map.empty

day11b_iterate n (!squareMap)
 | n > 300 =
  [] -- squareMap
 | otherwise =
  maximum:(day11b_iterate (n + 1) newSquareMap)
 where
  assocList = day11b_sums n squareMap
  maximum = maximumBy (comparing snd) assocList
  newSquareMap =
   foldl (\acc (k,v) -> Map.insert k v acc) squareMap assocList
  

day11b_sums n squareMap =
 assocList
 where
  coords =
   [(Point x y,n) | x <- [1..301 - n], y <- [1..301 - n]]
  assocList =
   map (day11b_getSum squareMap) coords
  
day11b_getSum squareMap (p@(Point x y),n)
 | n == 2 =
  ((p,n),sumTwo)
 | even n =
  ((p,n),sumModTwo)
 | otherwise =
  ((p,n),sumElse)
  where
   -- n == 2
   listTwo =
    [Point px py | px <- [x,x + 1], py <- [y,y + 1]]
   sumTwo = day11b_sumList listTwo
   
   -- n even
   nDiv = div n 2
   sumModTwo =
    sum $
     map (squareMap Map.!)
      [(p,nDiv) | p <-
       [Point px py |
        px <- [x,x + nDiv], py <- [y,y + nDiv]]]
   
   -- otherwise
   sumMinus = squareMap Map.! (p,n-1)
   elseList =
    [Point px py |
     px <- [x..x + n - 1], py <- [y..y + n - 1],
     px == x + n - 1 || py == y + n - 1]
   elseSum = day11b_sumList elseList
   sumElse = sumMinus + elseSum

day11b_sumList list =
 sum $ map (day11a_matrix Map.!) list

-- Day 11a

day11a = maximumBy (comparing snd) day11a_sums

day11a_sums = zip day11a_squares $ map day11a_getSums day11a_squares

day11a_getSums (Point x y) = summed
 where
  point00 = day11a_matrix Map.! (Point x y)
  point01 = day11a_matrix Map.! (Point x (y + 1))
  point02 = day11a_matrix Map.! (Point x (y + 2))
  point10 = day11a_matrix Map.! (Point (x + 1) y)
  point11 = day11a_matrix Map.! (Point (x + 1) (y + 1))
  point12 = day11a_matrix Map.! (Point (x + 1) (y + 2))
  point20 = day11a_matrix Map.! (Point (x + 2) y)
  point21 = day11a_matrix Map.! (Point (x + 2) (y + 1))
  point22 = day11a_matrix Map.! (Point (x + 2) (y + 2))
  
  summed =
   point00 + point01 + point02 +
   point10 + point11 + point12 +
   point20 + point21 + point22

day11a_matrix =
 Map.fromList $
  zip
   day11a_coords $
   map day11a_powerLevel day11a_coords

day11a_powerLevel (Point x y) = output
 where
  rackID = x + 10
  step1 = rackID * y
  step2 = step1 + day11a_serialNumber
  step3 = step2 * rackID
  step4 = mod (div step3 100) 10
  output = step4 - 5

day11a_squares = [Point x y | x <- [1..298], y <- [1..298]]
day11a_coords = [Point x y | x <- [1..300], y <- [1..300]]

day11a_serialNumber = data11

-- Day 10b

day10b = fst day10a_calc

-- Day 10a

day10a = day10a_print

day10a_print =
  do
  mapM putStrLn day10a_string
  return ()

day10a_string =
 [[Map.findWithDefault '.' (Point x y) day10a_map |
   x <- [xMin..xMax]] | y <- [yMin..yMax]]

 where
  (xMin,yMin,xMax,yMax) = pointListMinMax day10a_output

day10a_map = Map.fromList $ zip day10a_output $ repeat '#'

day10a_output = snd day10a_calc

day10a_calc = day10a_step 0 day10a_points day10a_velocities 0

day10a_step i points velocities currArea
 | nextArea > currArea && currArea /= 0 =
  (i,points)
 | otherwise =
  day10a_step (i + 1) nextPoints velocities nextArea
 where
  nextPoints =
   map
    (\(a,b) -> addPoints a b) $
    zip points velocities
  minMax = pointListMinMax nextPoints
  
  nextArea =
   (\(a,b,c,d) -> (c - a) * (d - b)) minMax

day10a_points = head day10a_trans
day10a_velocities = last day10a_trans

day10a_trans = transpose day10a_pairs

day10a_pairs =
 spoolList 2 $
  map (\[a,b] -> Point a b) $ spoolList 2 day10a_read

day10a_read :: [Int]
day10a_read =
 map read $
  map (filter (\x -> isNumber x || x == '-')) day10a_split
  
day10a_split = tail $ splitOnList "<," data10

-- Day 09b

day09b = maximum $ Map.elems $ fst day09b_output

day09b_output =
 day09a_step
  (day09a_highestMarble * 100) 1 0
  (Seq.singleton 0) Map.empty

-- Day 09a

day09a = maximum $ Map.elems $ fst day09a_output

day09a_output =
 day09a_step day09a_highestMarble 1 0 (Seq.singleton 0) Map.empty

-- force sequence to be evaluated
day09a_step end marble curr (!sequence) scoreMap
 | mod marble 23 == 0 && marble /= 0 = -- cache in
  day09a_step end
   (marble + 1) (mod (mod (curr - 7) l) $ l - 1)
   (Seq.deleteAt (mod (curr - 7) l) sequence) $
   Map.insertWith (+)
    (mod marble day09a_players)
    (marble + (Seq.index sequence $ mod (curr - 7) l))
    scoreMap
 | marble > end = -- done
  (scoreMap,sequence)
 | otherwise = -- place next marble
  day09a_step end
   (marble + 1) (mod (curr + 2) l)
   (Seq.insertAt (mod (curr + 2) l) marble sequence)
   scoreMap
  where
   l = Seq.length sequence

day09a_players = read $ head day09a_input :: Int
day09a_highestMarble = read $ last day09a_input :: Int

day09a_input =
 map (filter isNumber) $ splitOn ';' data09

-- Day 08b

day08b = day08b_calcValue day08a_tree

day08b_calcValue :: IntTree -> Int
day08b_calcValue (IntBranch children metadata)
 | children == [] =
  sum metadata
 | otherwise =
  sum $ map (getValues children) metadata
 where
  getValues :: [IntTree] -> Int -> Int
  getValues children reference
   | element == Nothing =
    0
   | otherwise =
    day08b_calcValue $ fromJust element
   where
    element = findInList (reference - 1) children

-- Day 08a
 
day08a = day08a_sumMetadata day08a_tree
 
day08a_sumMetadata (IntBranch children metadata) =
 childrenSum + metadataSum
 where
  childrenSum = sum $ map day08a_sumMetadata children
  metadataSum = sum metadata
 
day08a_tree = snd $ day08a_getBranch $ Seq.fromList data08

-- (length,tree)
day08a_getBranch :: Seq.Seq Int -> (Int,IntTree)
day08a_getBranch sequence =
 (length,IntBranch childList metadataList)
 where
 children = Seq.index sequence 0
 metadata = Seq.index sequence 1
 output =
  unzip $ day08a_spoolChildren children $ Seq.drop 2 sequence
 childrenLength = sum $ fst output
 length = 2 + childrenLength + metadata
 childList = snd output
 metadataList =
  toList $ Seq.take metadata $
   Seq.drop (2 + childrenLength) sequence
 
day08a_spoolChildren :: Int -> Seq.Seq Int -> [(Int,IntTree)]
day08a_spoolChildren 0 _ = []
day08a_spoolChildren n sequence =
 output:(day08a_spoolChildren (n - 1) $ Seq.drop length sequence)
 where
  output = day08a_getBranch sequence
  length = fst output 
  element = snd output


-- Day 07b

day07b = fst day07b_output

day07b_output = day07b_calc 0 "" []

day07b_calc t string workers
 | length workers < 5 && possible /= [] =
  day07b_calc t string $
   (day07b_timeMap Map.! next,next):workers
 | done /= [] =
  day07b_calc t nextString rest
 | workers == [] =
  (t,string)
 | otherwise =
  day07b_calc (t + 1) string nextWorkers
 where
  added = string ++ (map snd workers)
  possible =
   filter
    (\(a,b) -> all (`elem` string) b && notElem a added)
    day07a_fullList
  next = fst $ head possible
  done = filter (\(a,b) -> a == 0) workers
  rest = workers \\ done
  nextString = string ++ (map snd done)
  nextWorkers = map (\(a,b) -> (a - 1,b)) workers

day07b_timeMap = Map.fromList $ zip ['A'..'Z'] [61..86]

-- Day 07a

day07a = reverse day07a_output

day07a_output = day07a_calc ""

day07a_calc string
 | sort string == day07a_steps =
  string
 | otherwise =
  day07a_calc $ (fst $ head possible):string
 where
  possible =
   filter
    (\(a,b) -> all (`elem` string) b && notElem a string)
    day07a_fullList


day07a_fullList = assocList
 where
  assocList =
   zip day07a_steps $
    map (sort . nub . getList) day07a_steps
  
  getList step = direct ++ getRecursive direct
   where
    direct = Map.findWithDefault "" step day07a_directMap
    
  getRecursive step = concat $ map getList step

day07a_directMap = Map.fromListWith (++) assocList
 where
  assocList = map (\[a,b] -> (b,[a])) day07a_pairs

day07a_ends = ends \\ starts
 where
  starts = nub $ head day07a_transpose
  ends = nub $ last day07a_transpose

day07a_starts = starts \\ ends
 where
  starts = nub $ head day07a_transpose
  ends = nub $ last day07a_transpose

day07a_steps = sort $ nub $ concat day07a_transpose
day07a_transpose = transpose day07a_pairs
day07a_pairs = map (filter (isUpper) . tail) day07a_split

day07a_split = splitOn ';' data07

-- Day 06b

day06b = length day06b_output

day06b_output = filter (\(a,b) -> b < 10000) day06b_distanceAssoc

day06b_distanceAssoc = zip field $ map getDistanceSum field
 where
  field = day06a_field
  getDistanceSum p = sum distances
   where
    distances = map (manhattanDistance p) day06a_points

-- Day 06a

day06a = snd $ last day06a_output

day06a_output = sortBy (comparing snd) assocList
 where
  eligablePoints = day06a_points \\ day06a_infinitePoints
  assocList =
   zip eligablePoints $
    map (day06a_areas Map.!) eligablePoints
  

day06a_infinitePoints =
 map fromJust $
  nub $ filter (/= Nothing) $
   map (day06a_closestMap Map.!) day06a_edgeCoords
  

day06a_edgeCoords =
 [p |
  p@(Point x y) <- field,
  x == xMin || x == xMax || y == yMin || y == yMax]
 where
  field = day06a_field
  xMin = day06a_xMin
  xMax = day06a_xMax
  yMin = day06a_yMin
  yMax = day06a_yMax

day06a_areas =
 Map.fromListWith (+) $ map (\(a,b) -> (fromJust b,1)) $
  filter (\(a,b) -> b /= Nothing) day06a_closestAssoc

day06a_closestMap = Map.fromList day06a_closestAssoc

day06a_closestAssoc = zip field $ map getClosest field
 where
  field = day06a_field
  getClosest p
   | length (head grouped) == 1 =
    Just $ snd $ head $ head grouped
   | otherwise =
    Nothing
   where
    distances = map (manhattanDistance p) day06a_points
    assocList = sort $ zip distances day06a_points
    grouped = groupBy (equaling fst) assocList

day06a_field =
   [Point x y |
    x <- [day06a_xMin..day06a_xMax],
    y <- [day06a_yMin..day06a_yMax]]

day06a_points = map (\[a,b] -> Point a b) day06a_spool

day06a_xMin = minimum $ head day06a_xAndY
day06a_xMax = maximum $ head day06a_xAndY
day06a_yMin = minimum $ last day06a_xAndY
day06a_yMax = maximum $ last day06a_xAndY

day06a_xAndY = transpose day06a_spool

day06a_spool = spoolList 2 data06

-- Day 05b

day05b = length $ snd $ head day05b_output

day05b_output = sortBy (comparing $ length . snd) $ zip chars (map (day05a_process 0) inputList)
 where
  chars = transpose [['A'..'Z'],['a'..'z']]
  inputList = day05b_inputList

day05b_inputList = lists
 where
  chars = transpose [['A'..'Z'],['a'..'z']]
  lists =
   map (\x -> Seq.filter (`notElem` x) day05a_output) chars

-- Day 05a

day05a_output = day05a_process 0 day05a_input

-- use sequence and step back after deleting

day05a_process j sequence
 | i + 1 == Seq.length sequence =
  sequence
 | toLower a /= toLower b || a == b =
  day05a_process (i + 1) sequence
 | toLower a == b || toUpper a == b =
  day05a_process (i - 1) $
   Seq.deleteAt i $
    Seq.deleteAt i sequence
 where
  i = if j < 0 then 0 else j
  a = Seq.index sequence i
  b = Seq.index sequence $ i + 1
 
day05a_length = length data05

day05a_input = Seq.fromList data05

-- Day 04b

day04b = (fst day04b_output) * (head $ snd day04b_output)

day04b_output =
 maximumBy (comparing (length . snd)) day04b_mostMinuteAssoc

day04b_mostMinuteAssoc = zip guardIds minuteMaximums
 where
  sleepingGuards =
   filter (\x -> snd x /= []) $ Map.toList day04a_sleepMap
  guardIds = fst $ unzip sleepingGuards
  minuteLists =
   map day04a_minuteList $ snd $ unzip sleepingGuards
  minuteMaximums =
   map (maximumBy (comparing length) . group . sort) minuteLists

-- Day 04a

day04a =  day04a_mostSleepID * day04a_mostSleepMinute

day04a_mostSleepMinute =
 head $ maximumBy (comparing length) $ group $ sort minuteList
 where
  minuteList =
   day04a_minuteList $ day04a_sleepMap Map.! day04a_mostSleepID

day04a_minuteList [] = []
day04a_minuteList ((wake,length):next) =
 [wake..wake + length - 1] ++ (day04a_minuteList next)
 

day04a_mostSleepID = fst mostSleep
 where
  sleepSum = fmap (sum . snd . unzip) day04a_sleepMap
  mostSleep = maximumBy (comparing snd) $ Map.toList sleepSum

day04a_sleepMap = Map.fromListWith (++) day04a_assocList

-- (guardId,minuteList)
day04a_assocList = day04a_spoolGuards day04a_split

day04a_spoolGuards [] = []
day04a_spoolGuards (guard:list) =
 (guardId,minuteList):(day04a_spoolGuards nextList)
 where
  guardId = read $ drop 12 $ filter isNumber guard :: Int
  split = break (\x -> elem 'G' x) list
  minuteList = getMinutes $ fst split
  nextList = snd split
  
  getMinutes [] = []
  getMinutes (sleep:wake:next) =
   (sleepMinute,wakeMinute - sleepMinute):(getMinutes next)
   where
    sleepMinute = read $ drop 10 $ filter isNumber sleep :: Int
    wakeMinute  = read $ drop 10 $ filter isNumber wake :: Int
  
day04a_split = sort $ splitOn ';' data04

-- Day 03b

day03b = head day03b_output

day03b_output = nonOverlappedIds
 where
  allIds = Map.keys day03b_rectangleMap
  overlaps = day03a_output
  overLappedIds = nub $ concat overlaps
  nonOverlappedIds = allIds \\ overLappedIds

day03b_rectangleMap =
 Map.fromList day03a_rectangleAssoc

-- Day 03a

day03a = length $ day03a_output

day03a_output =
 filter (\x -> length x > 1) $
  Map.elems day03a_rectangleMap
 
day03a_rectangleMap = addRectangle day03a_rectangleAssoc
 where
  addRectangle [] = Map.empty
  addRectangle ((id,rect):next) =
   Map.unionWith (++) rectMap (addRectangle next)
   where
    assocList =
     map (\p -> (p,[id])) $ day03a_rectangleToPointList $ rect
    rectMap = Map.fromList assocList
 
day03a_rectangleToPointList (Rectangle (Point x1 y1) w h) =
 [Point x y | x <- [x1..(x1+w-1)], y <- [y1..(y1+h-1)]]
 
day03a_rectangleAssoc = map getRectangle day03a_split
 where
 getRectangle string = (id,Rectangle (Point x y) w h)
  where
   split0 = break (== ' ') $ drop 1 string
   split1 = break (== ',') $ drop 3 $ snd split0
   split2 = break (== ':') $ drop 1 $ snd split1
   split3 = break (== 'x') $ drop 2 $ snd split2
   split4 = drop 1 $ snd split3
   id = read $ fst split0 :: Int
   x  = read $ fst split1 :: Int
   y  = read $ fst split2 :: Int
   w  = read $ fst split3 :: Int
   h  = read split4 :: Int
 
day03a_split = splitOn ';' data03

-- Day 02b

day02b = fst unzipped
 where
  zipped = zip (fst day02b_output) (snd day02b_output)
  filtered = filter (\(a,b) -> a == b) zipped
  unzipped = unzip filtered

day02b_output = day02b_calc day02a_split

day02b_calc (x:xs)
 | output == Nothing =
  day02b_calc xs
 | otherwise =
  fromJust output 
 where
  output = day02b_check x xs

day02b_check element [] = Nothing
day02b_check element (x:xs)
 | day02b_justOne element x 0 =
  Just (element,x)
 | otherwise =
  day02b_check element xs

day02b_justOne [] _ count = if count == 1 then True else False
day02b_justOne _ [] count = if count == 1 then True else False
day02b_justOne (x:xs) (y:ys) count
 | count > 1 =
  False
 | otherwise =
  if x /= y 
   then day02b_justOne xs ys (count+1)
   else day02b_justOne xs ys count

-- Day 02a

day02a = (length day02a_doubles) * (length day02a_triples)

day02a_triples =
 filter (any (\x -> length x == 3)) day02a_grouped

day02a_doubles =
 filter (any (\x -> length x == 2)) day02a_grouped

day02a_grouped = map group day02a_sorted

day02a_sorted = map sort day02a_split

day02a_split = splitOn ';' data02

-- Day 01b

day01b = head day01b_output

day01b_output = day01b_iterate [0] [] 

day01b_iterate list input_raw
 | elem next list =
  next:list
 | otherwise =
  day01b_iterate (next:list) $ tail input
 where
  last = head list
  input =
   if input_raw == []
    then day01a_int
    else input_raw 
  next = last + (head input)

-- Day 01a

day01a = sum day01a_int

day01a_int :: [Int]
day01a_int = map read day01a_split

day01a_split = splitOn ';' day01a_filtered

day01a_filtered = filter (/= '+') data01

-- Commons

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn element list =
 foldr
  (\x acc@(acc_h:acc_t) ->
   if x == element
    then []:acc
    else (x:acc_h):acc_t)
  [[]] list

splitOnList :: Eq a => [a] -> [a] -> [[a]]
splitOnList elementList list =
 foldr
  (\x acc@(acc_h:acc_t) ->
   if elem x elementList
    then []:acc
    else (x:acc_h):acc_t)
  [[]] list
  
spoolList n [] = []
spoolList n list = (take n list):(spoolList n (drop n list))

equaling f a b = f a == f b

manhattanDistance (Point x1 y1) (Point x2 y2) =
 abs (x1 - x2) + abs (y1 - y2)
 
swap (a,b) = (b,a)

findInList index list
 | index < 0 || index >= length list =
  Nothing
 | otherwise =
  Just $ list !! index
  
addPoints (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

pointListMinMax list =
 foldl getMinMax
    ((\(Point a b) -> (a,b,a,b)) $ head list) $
    tail list
 where
  getMinMax (xMin,yMin,xMax,yMax) (Point x y) =
   (minX,minY,maxX,maxY)
   where
    minX = if x < xMin then x else xMin
    minY = if y < yMin then y else yMin
    maxX = if x > xMax then x else xMax
    maxY = if y > yMax then y else yMax

takeWhileAscendingBy f [] = []    
takeWhileAscendingBy f [x1] = [x1]
takeWhileAscendingBy f (x1:x2:xs)
 | f x1 x2 /= GT =
  x1:(takeWhileAscendingBy f (x2:xs))
 | otherwise =
  [x1]
  
-- from big endian binary list
fromBinaryList list = getFromBinaryList 1 list
getFromBinaryList :: Int -> [Int] -> Int
getFromBinaryList n [0] = 0
getFromBinaryList n [1] = n
getFromBinaryList n (current:next) =
 current * n + getFromBinaryList (n * 2) next
 
takeWithDefault def n list
 | length list < n =
  take n $ list ++ (replicate n def)
 | otherwise =
  take n list
  
movePoint (Point x y) North = (Point x (y - 1))
movePoint (Point x y) East = (Point (x + 1) y)
movePoint (Point x y) South = (Point x (y + 1))
movePoint (Point x y) West = (Point (x - 1) y)

movePointByList p [] = p
movePointByList p (x:xs) =
 movePointByList (movePoint p x) xs


turnDirection North Clockwise = East 
turnDirection East Clockwise = South
turnDirection South Clockwise = West
turnDirection West Clockwise = North

turnDirection North Counterclockwise = West
turnDirection East Counterclockwise = North
turnDirection South Counterclockwise = East
turnDirection West Counterclockwise = South

adjacentPoints p = map (movePoint p) [North,East,South,West]

surroundingPoints p =
 map (movePointByList p)
  [[North,West],[North],[North,East],[West],[East],
   [South,West],[South],[South,East]]


readingOrder points =
 sortBy (\(Point a b) (Point c d) -> compare (b,a) (d,c)) points
 
listToArray list =
 array (0,max) $ zip [0..max] list
 where
  max = (length list) - 1

listToArrayOne list =
 array (1,max) $ zip [1..max] list
 where
  max = length list
  
readInt string = read string :: Int

fst' (a,b,c) = a
snd' (a,b,c) = b
trd' (a,b,c) = c