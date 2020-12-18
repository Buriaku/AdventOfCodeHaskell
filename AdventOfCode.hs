{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.List
import Data.Array
import Data.Foldable
import Data.Char
import Data.Maybe
import Data.Ord
-- import Data.Function
-- import Data.Bits
-- import Control.Concurrent
import Data.Hashable

import GHC.Generics (Generic)

import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
-- import qualified Data.HashPSQ as HPSQ

import AdventOfCodeData

data Point =
 Point Int Int
 deriving (Show, Eq, Ord, Ix, Generic)
 
instance Hashable Point

data Tobogan =
 Tree | Ground
 deriving (Eq, Show)
 
data Password =
 Password Int Int Char String
 deriving (Show, Eq)

data Bag =
 Bag String [(Int,String)]
 deriving (Show, Eq)

data Operation =
 ACC | JMP | NOP
 deriving (Eq, Ord, Bounded, Enum, Show, Read)
 
data Direction =
 North | East | South | West
 deriving (Show, Eq)

data Turning =
 Counterclockwise | Straight | Clockwise
 deriving (Show, Eq)

data Area =
 Floor | Empty | Occupied
 deriving (Eq, Show)
 
data Point3D =
 Point3D Int Int Int
 deriving (Show, Eq, Ord, Ix, Generic)
 
instance Hashable Point3D

data Point4D =
 Point4D Int Int Int Int
 deriving (Show, Eq, Ord, Ix, Generic)
 
instance Hashable Point4D

-- Day 18b

day18b = sum day18b_calc

day18b_calc =
 map (day18a_processEntry 0 "") day18b_entries

day18b_entries =
 map day18b_insertPar day18b_entriesReplaced

day18b_insertPar :: [String] -> [String]
day18b_insertPar list
 | elem "#" list =
  day18b_insertPar newList
 | otherwise = list
 where
  newList = day18b_processPlus list
  
day18b_processPlus list =
 leftRest ++ "(":leftElem ++ "+":rightElem ++ ")": rightRest
 where
  index = fromJust $ elemIndex "#" list
  (left,hash:right) = splitAt index list
  (rightElem,rightRest) =
   day18b_getElemRight 0 [] right
  (leftElemRev,leftRestRev) =
   day18b_getElemLeft 0 [] $ reverse left
  leftElem = reverse leftElemRev
  leftRest = reverse leftRestRev

day18b_getElemRight level reverseList (curr:rest)
 | curr == "(" =
  day18b_getElemRight
   (level + 1) (curr:reverseList) rest
 | curr == ")" && level == 1 =
  (reverse (curr:reverseList),rest)
 | curr == ")" =
  day18b_getElemRight
   (level - 1) (curr:reverseList) rest
 | all isDigit curr && level == 0 =
  ([curr],rest)
 | otherwise =
  day18b_getElemRight
   level (curr:reverseList) rest

day18b_getElemLeft level reverseList (curr:rest)
 | curr == ")" =
  day18b_getElemLeft
   (level + 1) (curr:reverseList) rest
 | curr == "(" && level == 1 =
  (reverse (curr:reverseList),rest)
 | curr == "(" =
  day18b_getElemLeft
   (level - 1) (curr:reverseList) rest
 | all isDigit curr && level == 0 =
  ([curr],rest)
 | otherwise =
  day18b_getElemLeft
   level (curr:reverseList) rest

day18b_entriesReplaced =
 map (replaceListItem "+" "#") day18a_entries
 
replaceListItem a b list =
 foldr foldFunc [] list
 where
  foldFunc x acc
   | x == a = b:acc
   | otherwise = x:acc

-- Day 18a

day18a = sum day18a_calc

day18a_calc =
 map (day18a_processEntry 0 "") day18a_entries

day18a_processEntry prevRes _ [] = prevRes
day18a_processEntry prevRes prevOp list@(curr:rest)
 | curr == "+" =
  day18a_processEntry
   prevRes "+" rest
 | curr == "*" =
  day18a_processEntry
   prevRes "*" rest
 | all isDigit curr && prevOp == "+" =
  day18a_processEntry
   (prevRes + (readInt curr)) "" rest
 | all isDigit curr && prevOp == "*" =
  day18a_processEntry
   (prevRes * (readInt curr)) "" rest
 | all isDigit curr =
  day18a_processEntry
   (readInt curr) "" rest
 | curr == "(" && prevOp == "+" =
  day18a_processEntry
   (prevRes + parRes) "" parRest
 | curr == "(" && prevOp == "*" =
  day18a_processEntry
   (prevRes * parRes) "" parRest
 | curr == "(" =
  day18a_processEntry
   (parRes) "" parRest
  where
   (parList,parRest) =
    day18a_processPar 0 [] list
   parRes = day18a_processEntry 0 "" parList
    
day18a_processPar level reverseList (curr:rest)
 | curr == "(" && level == 0 =
  day18a_processPar
   (level + 1) reverseList rest
 | curr == "(" =
  day18a_processPar
   (level + 1) (curr:reverseList) rest
 | curr == ")" && level == 1 =
  (reverse reverseList,rest)
 | curr == ")" =
  day18a_processPar
   (level - 1) (curr:reverseList) rest
 | otherwise =
  day18a_processPar level (curr:reverseList) rest

day18a_entries =
 map ((filter (/= "")) . spoolOnList "+*()") day18a_lines

day18a_lines = splitOn ';' day18a_filtered

day18a_filtered = filter (/= ' ') data18
 
-- Day 17b

day17b = length day17b_calc

day17b_calc :: [Bool]
day17b_calc =
 filter id $ HashMap.elems $ day17b_getResult

day17b_getResult :: HashMap.HashMap Point4D Bool  
day17b_getResult =
  day17b_iterate 0 day17b_map

day17b_iterate ::
 Int -> HashMap.HashMap Point4D Bool ->
  HashMap.HashMap Point4D Bool

day17b_iterate 6 (!cubeMap) = cubeMap
day17b_iterate i (!cubeMap) =
 day17b_iterate (i + 1) newCubeMap
 where
  lastPoints = HashMap.keys cubeMap
  points =
   toList $ Set.fromList $ concat $
    map (\p -> p:(day17b_neighbors p)) lastPoints
  
  updates = getUpdates points
  newCubeMap =
   foldl (\acc (a,b) -> HashMap.insert a b acc)
    cubeMap updates
  
  getUpdates [] = []
  getUpdates (curr:rest)
   | currState && elem activeNeighbors [2,3] =
    (curr,True):(getUpdates rest)
   | not currState && activeNeighbors == 3 =
    (curr,True):(getUpdates rest)
   | otherwise =
    (curr,False):(getUpdates rest)
   where
    neighboring = day17b_neighbors curr
    currState = day17b_find cubeMap curr
    neighborStates =
     map (day17b_find cubeMap) neighboring
    activeNeighbors =
     length $ filter id neighborStates

day17b_find ::
 HashMap.HashMap Point4D Bool -> Point4D -> Bool

day17b_find m p =
 HashMap.findWithDefault False p m
 
day17b_neighbors :: Point4D -> [Point4D]
day17b_neighbors (Point4D a b c d) =
 [Point4D x y z w|
   x <- [(a - 1)..(a + 1)],
   y <- [(b - 1)..(b + 1)],
   z <- [(c - 1)..(c + 1)],
   w <- [(d - 1)..(d + 1)],
   x /= a || y /= b || z /= c || w /= d]
   
day17b_map :: HashMap.HashMap Point4D Bool
day17b_map = HashMap.fromList day17b_list

day17b_list = assocList
 where
  coords = day17b_points
  assocList =
   zip coords $ map charToBool $ concat day17a_lines
  charToBool '.' = False
  charToBool '#' = True

day17b_points =
 [Point4D x y 0 0|
  y <- [0..day17a_yMax], x <- [0..day17a_xMax]]
 
-- Day 17a

day17a = length day17a_calc

day17a_calc :: [Bool]
day17a_calc =
 filter id $ HashMap.elems $
  day17a_iterate 0 day17a_map

day17a_iterate ::
 Int -> HashMap.HashMap Point3D Bool ->
  HashMap.HashMap Point3D Bool

day17a_iterate 6 (!cubeMap) = cubeMap
day17a_iterate i (!cubeMap) =
 day17a_iterate (i+1) newCubeMap
 where
  lastPoints = HashMap.keys cubeMap
  points =
   toList $ Set.fromList $ concat $
    map (\p -> p:(day17a_neighbors p)) lastPoints
  updates = getUpdates points
  newCubeMap =
   foldl (\acc (a,b) -> HashMap.insert a b acc)
    cubeMap updates
  
  getUpdates [] = []
  getUpdates (curr:rest)
   | currState && elem activeNeighbors [2,3] =
    (curr,True):(getUpdates rest)
   | not currState && activeNeighbors == 3 =
    (curr,True):(getUpdates rest)
   | otherwise =
    (curr,False):(getUpdates rest)
   where
    neighboring = day17a_neighbors curr
    currState = day17a_find cubeMap curr
    neighborStates =
     map (day17a_find cubeMap) neighboring
    activeNeighbors =
     length $ filter id neighborStates

day17a_find ::
 HashMap.HashMap Point3D Bool -> Point3D -> Bool
 
day17a_find m p =
 HashMap.findWithDefault False p m
 
day17a_neighbors :: Point3D -> [Point3D]
day17a_neighbors (Point3D a b c) =
 [Point3D x y z |
   x <- [(a - 1)..(a + 1)],
   y <- [(b - 1)..(b + 1)],
   z <- [(c - 1)..(c + 1)],
   x /= a || y /= b || z /= c]
   
day17a_map :: HashMap.HashMap Point3D Bool
day17a_map = HashMap.fromList day17a_list

day17a_list = assocList
 where
  coords = day17a_points
  assocList =
   zip coords $ map charToBool $ concat day17a_lines
  charToBool '.' = False
  charToBool '#' = True

day17a_points =
 [Point3D x y 0 |
  y <- [0..day17a_yMax], x <- [0..day17a_xMax]]

day17a_xMax = day17a_xLength - 1
day17a_yMax = day17a_yLength - 1
day17a_xLength = length $ head day17a_lines
day17a_yLength = length day17a_lines

day17a_lines = splitOn ';' data17
 
-- Day 16b

day16b :: Int
day16b = foldl1 (*) departureNumbers
 where
  correctIndexes :: [Int]
  correctIndexes =
   fst $ unzip $ take 6 day16b_calc
  departureNumbers =
   map (data16b !!) correctIndexes

day16b_calc :: [(Int,[Int])]
day16b_calc =
 sortBy (comparing snd) $
  day16b_iterate [] day16b_candidateRanges

day16b_iterate ::
 [(Int, [Int])] -> [(Int, [Int])] ->
  [(Int, [Int])]
day16b_iterate found [] = found
day16b_iterate found list =
 day16b_iterate (justOne:found) filtered
 where
  justOne :: (Int,[Int])
  justOne =
   head $
    filter (\x -> 1 == (length $ snd x)) list
  foundNumber = head $ snd justOne
  filtered =
   map (\(a,b) -> (a,delete foundNumber b)) $
    delete justOne list

day16b_candidateRanges :: [(Int,[Int])]
day16b_candidateRanges =
 zip [0..]
  (getCandidateRanges $
    transpose day16b_validTickets)
 where
  getCandidateRanges [] = []
  getCandidateRanges (curr:rest) =
   (candidates 0 curr):(getCandidateRanges rest)
  
  candidates i list
   | i == length day16a_ranges = []
   | all (`elem` (day16a_ranges !! i)) list =
    i:(candidates (i + 1) list)
   | otherwise =
    candidates (i + 1) list

day16b_validTickets :: [[Int]]
day16b_validTickets = day16b_filter data16c

day16b_filter :: [[Int]] -> [[Int]]
day16b_filter [] = []
day16b_filter (curr:rest)
 | all (`elem` day16a_allInts) curr =
  curr:(day16b_filter rest)
 | otherwise =
  day16b_filter rest
 
-- Day 16a

day16a :: Int
day16a = sum day16a_calc

day16a_calc :: [Int]
day16a_calc =
 concat $ day16a_filter data16c

day16a_filter :: [[Int]] -> [[Int]]
day16a_filter [] = []
day16a_filter (curr:rest) =
 (filter (`notElem` day16a_allInts) curr):
  (day16a_filter rest)

day16a_allInts :: [Int]
day16a_allInts =
 nub $ sort $ concat day16a_ranges

day16a_ranges :: [[Int]]
day16a_ranges = ranges
 where
  ranges =
   map (\[[a,b],[c,d]] -> [a..b] ++ [c..d])
    intPairPairs
    
  intPairPairs :: [[[Int]]]
  intPairPairs =
   map (map (map readInt))
    entriesDigits
   
  entriesDigits :: [[[String]]]
  entriesDigits =
    map (map (map (filter (`elem` ['0'..'9'])))) $
     entries
  
  entries :: [[[String]]]
  entries =
   map (map (splitOn '-'))
    entryOrs
 
  entryOrs :: [[String]]
  entryOrs =
   map (splitOnString " or ") lines
  
  lines :: [String]
  lines = splitOn ';' data16a 
 
-- Day 15b

day15b =
 day15b_generate (length data15)
  (last data15) day15b_intMap


day15b_generate ::
 Int -> Int -> IntMap.IntMap Int -> Int
day15b_generate i curr (!prevOccuranceMap)
 | i == 30000000 = curr
 | otherwise =
  day15b_generate (i + 1) next occuranceMap
 where
  prevOccurance =
   IntMap.lookup curr prevOccuranceMap
  occuranceMap =
   IntMap.insert curr i prevOccuranceMap
  next =
   if prevOccurance == Nothing
    then 0
    else (i - fromJust prevOccurance)

day15b_intMap =
 IntMap.fromList $ zip (init data15) [1..]
 
-- Day 15a

day15a =
 head $ day15a_generate day15a_list

day15a_generate list@(prev:rest)
 | length list == 2020 = list
 | otherwise =
  day15a_generate $ next:list
 where
  index = elemIndex prev rest
  next =
   if index == Nothing
    then 0
    else (fromJust index) + 1
  

day15a_list = reverse data15

-- Day 14b

day14b =
 sum $ Map.elems day14b_calc

day14b_calc =
 day14b_process Map.empty day14a_entries

day14b_process memMap [] = memMap
day14b_process memMap (entry:restEntries) =
 day14b_process newMemMap restEntries
 where
 (mask,pairs) = entry
 newMemMap = processEntry memMap mask pairs
 processEntry memMap mask [] = memMap
 processEntry memMap mask (currPair:restPairs) =
  processEntry newMemMap mask restPairs
  where
   binList =
    toBinaryList day14a_binMax $ currPair !! 0
   value = currPair !! 1
   maskedList = day14b_applyBitmask mask binList
   newMemMap = foldl mapInsert memMap maskedList
   mapInsert acc x =
     Map.insert x value acc
 

day14b_applyBitmask [] [] = [[]]
day14b_applyBitmask
 (currMask:restMask) (currBinList:restBinList)
 | currMask == Nothing =
  (map (0:)
   (day14b_applyBitmask restMask restBinList)) ++
   (map (1:)
    (day14b_applyBitmask restMask restBinList))
 | currMask == Just 0 =
  map (currBinList:)
   (day14b_applyBitmask restMask restBinList)
 | otherwise =
  map (1:)
   (day14b_applyBitmask restMask restBinList)
 
-- Day 14a

day14a =
 sum $ map (fromBinaryList day14a_binMax) $
  Map.elems day14a_calc

day14a_calc =
 day14a_process Map.empty day14a_entries

day14a_process memMap [] = memMap
day14a_process memMap (entry:restEntries) =
 day14a_process newMemMap restEntries
 where
 (mask,pairs) = entry
 newMemMap = processEntry memMap mask pairs
 processEntry memMap mask [] = memMap
 processEntry memMap mask (currPair:restPairs) =
  processEntry newMemMap mask restPairs
  where
   memAddress = currPair !! 0
   binList =
    toBinaryList day14a_binMax $ currPair !! 1
   masked = day14a_applyBitmask mask binList
   newMemMap = Map.insert memAddress masked memMap
 

day14a_applyBitmask [] [] = []
day14a_applyBitmask
 (currMask:restMask) (currBinList:restBinList)
 | currMask == Nothing =
  currBinList:
   (day14a_applyBitmask restMask restBinList)
 | otherwise =
  (fromJust currMask):
   (day14a_applyBitmask restMask restBinList)

fromBinaryList _ [] = 0
fromBinaryList power2 (curr:rest) =
 power2 * curr + (fromBinaryList nextPower2 rest)
 where
  nextPower2 = div power2 2

-- to small endian binary list
toBinaryList power2 n
 | power2 == 1 =
  [digit]
 | otherwise =
  digit:(toBinaryList nextPower2 rest)
 where
  (digit,rest) = divMod n power2
  nextPower2 = div power2 2

day14a_entries = entries
 where
  split = map (init . tail . splitOnList "=;") day14a_units
  filtered =
   map (map $ filter (`elem` 'X':['0'..'9'])) split
  entries = map toEntries filtered
  toEntries stringList =
   (mask,pairs)
   where
    (maskRaw,pairsRaw) = splitAt 1 stringList
    mask = map toMask $ head maskRaw
    pairs = spoolList 2 $ map readInt pairsRaw
    toMask 'X' = Nothing
    toMask '0' = Just 0
    toMask '1' = Just 1

day14a_binMax = 2 ^ 35

day14a_units = tail $ splitOnString "mask" data14

-- Day 13b

day13b_bruteForce n
 | checked && mod n 1000000 == 0 =
  n:(day13b_bruteForce $ n + 1)
 | checked =
  day13b_bruteForce $ n + 1
 | otherwise = [m]
 where
  m = n * 29 * 631 * 19 * 23 * 41 -- 17 * 449 * 13 * 19 * 41
  checked =
   any notDivisible day13b_newBussesOffsets
  notDivisible (bus,offset) =
   mod (m + offset) bus /= 0
   
day13b_newBussesOffsets =
 map (\(a,b) -> (a,b - 29))
  day13b_filteredBussesOffsets

day13b_filteredBussesOffsets =
 day13b_bussesOffsets \\
  [(29,0),(631,29),(19,48),(23,52),(41,70)]
  

lowestCommonMultipleFromPrimeLists primeLists =
 result -- foldl1 (*) result
 where
  result = getResult primes primeLists
  getResult list@(curr:rest) remainingPrimes
   | all (== []) remainingPrimes = []
   | any (elem curr) remainingPrimes =
    curr:(getResult list finned)
   | otherwise =
    getResult rest remainingPrimes
   where
    finned = map (delete curr) remainingPrimes

-- day13b_primeLists =
--  map getPrimeFactors day13b_times

-- day13b_times =
--  map (\(a,b) -> a + b) day13b_bussesOffsets

day13b_bussesOffsets =
 zip day13a_busses day13b_offsets
 
day13b_offsets =
 map fst $
  filter ((/= "x") . snd) $
   zip [0..day13b_highest] day13a_entries

day13b_highest = (length day13a_entries) + 1

-- Day 13a

day13a = (time - day13a_magic) * bus
 where
  (time,busPrimes) = day13a_check day13a_magic
  bus = foldl1 (*) busPrimes

day13a_check n
 | checked == [] = day13a_check $ n + 1
 | otherwise = (n,checked)
 where
  primeFactors = getPrimeFactors n
  checked = dividable n day13a_busPrimes
  dividable n [] = []
  dividable n (primeList:rest)
   | remaining == [] = primeList
   | otherwise = dividable n rest
   where
    remaining = primeList \\ primeFactors
   
day13a_busPrimes = map getPrimeFactors day13a_busses

day13a_busses =
 map readInt $ filter (/= "x") day13a_entries

day13a_entries = splitOn ',' data13

day13a_magic = 1000507 -- 1006401
 :: Int

getPrimeFactors :: Int -> [Int]
getPrimeFactors n = primeFactors n 2 primes []

primeFactors :: Int -> Int -> [Int] -> [Int] -> [Int]
primeFactors n lastPrime primeList@(nextPrime:otherPrimes) divisorList
 | mod n lastPrime == 0 = primeFactors (div n lastPrime) lastPrime primeList (lastPrime:divisorList)
 | mod n nextPrime == 0 = primeFactors (div n nextPrime) nextPrime otherPrimes (nextPrime:divisorList)
 | nextPrime > n        = divisorList
 | otherwise            = primeFactors n nextPrime otherPrimes divisorList

primes :: [Int]
primes =
 [n | n <- [2..], foldl (\acc x -> if mod n x == 0 then n:acc else acc) [] ([2..(floor (sqrt (fromIntegral n)))] :: [Int]) == []]
 
-- Day 12b

day12b = manhattanDistance day12a_start day12b_calc

day12b_calc =
 day12b_follow day12a_start day12b_waypoint
  day12a_instructions

day12b_follow p wp [] = p
day12b_follow p wp (('L',n):rest) =
 day12b_follow p
  (iterate (turnVector Counterclockwise)
   wp !! (div n 90)) rest
day12b_follow p wp (('R',n):rest) =
 day12b_follow p
  (iterate (turnVector Clockwise)
   wp !! (div n 90)) rest
day12b_follow p wp (('F',n):rest) =
 day12b_follow
  (iterate (addPoints wp) p !! n)
  wp rest
day12b_follow p wp ((char,n):rest) =
 day12b_follow p
  (movePointByList wp $ replicate n $
    charToDirection char) rest
 where
  charToDirection 'N' = North
  charToDirection 'E' = East
  charToDirection 'S' = South
  charToDirection 'W' = West

day12b_waypoint = Point 10 (-1)
 
-- Day 12a

day12a = manhattanDistance day12a_start day12a_calc

day12a_calc =
 day12a_follow day12a_start East day12a_instructions

day12a_follow :: Point -> Direction -> [(Char,Int)] -> Point
day12a_follow p heading [] = p
day12a_follow p heading (('L',n):rest) =
 day12a_follow p
  (iterate (turnDirection Counterclockwise)
   heading !! (div n 90)) rest
day12a_follow p heading (('R',n):rest) =
 day12a_follow p
  (iterate (turnDirection Clockwise)
   heading !! (div n 90)) rest
day12a_follow p heading (('F',n):rest) =
 day12a_follow
  (iterate (`movePoint` heading) p !! n)
  heading rest
day12a_follow p heading ((char,n):rest) =
 day12a_follow
  (movePointByList
    p $ replicate n $ charToDirection char)
  heading rest
 where
  charToDirection 'N' = North
  charToDirection 'E' = East
  charToDirection 'S' = South
  charToDirection 'W' = West

day12a_start = Point 0 0

day12a_instructions =
 map getInstructions day12a_lines
 where
  getInstructions (char:rest) =
   (char,readInt rest)

day12a_lines = splitOn ';' data12

-- Day 11b

day11b =
 length $ filter (== Occupied) $ toList day11b_calc

day11b_calc = day11b_iterate day11a_areaArray

day11b_iterate area
 | changed == [] = area
 | otherwise     = day11b_iterate newArea
 where
  changed = changes day11a_points
  newArea = area // changed
  
  changes [] = []
  changes (p:rest)
   | currState == Empty && occuCount == 0 =
    (p,Occupied):(changes rest)
   | currState == Occupied && occuCount >= 5 =
    (p,Empty):(changes rest)
   | otherwise =
    changes rest
   where
    currState = area ! p
    surr = day11b_getSurrounding p
    surrStates = map (area !) surr
    occuCount = day11a_countOccu surrStates

day11b_getSurrounding p = surrounding
 where
  surrounding =
   filter (/= (Point (-1) (-1))) $
    map (checkDirection p) surroundingDirections
  checkDirection p directions
   | x < 0 || y < 0 || x > day11a_xMax ||
     y > day11a_yMax =
    (Point (-1) (-1))
   | areaType /= Floor =
    newP
   | otherwise =
    checkDirection newP directions
   where
    newP = movePointByList p directions
    areaType = day11a_areaArray ! newP
    (Point x y) = newP
 
-- Day 11a

day11a =
 length $ filter (== Occupied) $ toList day11a_calc

day11a_calc = day11a_iterate day11a_areaArray

day11a_iterate area
 | changed == [] = area
 | otherwise     = day11a_iterate newArea
 where
  changed = changes day11a_points
  newArea = area // changed
  
  changes [] = []
  changes (p:rest)
   | currState == Empty && occuCount == 0 =
    (p,Occupied):(changes rest)
   | currState == Occupied && occuCount >= 4 =
    (p,Empty):(changes rest)
   | otherwise =
    changes rest
   where
    currState = area ! p
    surr = day11a_getSurrounding p
    surrStates = map (area !) surr
    occuCount = day11a_countOccu surrStates
  
day11a_countOccu list =
 foldl foldIt 0 list
 where
  foldIt acc Occupied = acc + 1
  foldIt acc _ = acc

day11a_getSurrounding p = legal
 where
  surrounding = surroundingPoints p
  legal = filter filterFunction surrounding
  filterFunction (Point x y) =
   x >= 0 && y >= 0 &&
    x <= day11a_xMax && y <=day11a_yMax
  

day11a_areaArray =
 array
  (Point 0 0,Point day11a_xMax day11a_yMax)
  day11a_list
 
day11a_list = assocList
 where
  coords = day11a_points
  assocList =
   zip coords $ map charToArea $ concat day11a_lines
  charToArea '.' = Floor
  charToArea 'L' = Empty
  charToArae '#' = Occupied

day11a_points =
 [Point x y |
  y <- [0..day11a_yMax], x <- [0..day11a_xMax]]

day11a_xMax = day11a_xLength - 1
day11a_yMax = day11a_yLength - 1
day11a_xLength = length $ head day11a_lines
day11a_yLength = length day11a_lines

day11a_lines = splitOn ';' data11
 
-- Day 10b

-- solvePart2 :: [Int] -> Int
-- solvePart2 ls =
 -- go (1 : repeat 0) (sort ls) 0
  -- where
   -- go [] [] _ = 0
   -- go as [] _ = head as
   -- go as (l : ls) i =
    -- go as' ls l
     -- where
      -- diff = (l - i -1)
      -- s = sum $ take (3 - diff) as
      -- as' = foldr (:) as (s : replicate diff 0)

day10b = day10b_calc day10b_chainChoicesChecked

day10b_calc [] = 1
day10b_calc (a:rest) =
 (length a) * (day10b_calc rest)

day10b_chainChoicesChecked =
 chainChoicesChecked
 where
  chainChoicesChecked =
   map (filter checkDeleteList) chainChoices
  chainChoices = map choicesFromList day10b_chains
  checkDeleteList [] = True
  checkDeleteList list =
   day10b_check (day10a_sorted \\ list)

day10b_chains =
 groupBy' (\a  b -> b - a < 3) day10b_removable

day10b_removable =
 filter checkRemoved $ tail $ init day10a_sorted
 where
  checkRemoved x =
   day10b_check $ delete x day10a_sorted
   
day10b_check [a] = True
day10b_check (a:b:rest)
 | diff > 0 && diff < 4 =
  day10b_check (b:rest)
 | otherwise =
  False
 where
  diff = b - a

 
-- Day 10a

day10a = a * b
 where
  (a,b) = day10a_calc

day10a_calc = day10a_count day10a_sorted (0,0)

day10a_count [a] (jolt1,jolt3) = (jolt1,jolt3)
day10a_count (a:b:rest) (jolt1,jolt3)
 | diff == 1 =
  day10a_count (b:rest) (jolt1 + 1,jolt3) 
 | diff == 3 =
  day10a_count (b:rest) (jolt1,jolt3 + 1) 
 | otherwise =
  day10a_count (b:rest) (jolt1,jolt3)
 where
  diff = b - a


day10a_sorted = sort $ 0:day10a_max:data10

day10a_max = (+) 3 $ maximum data10
 
-- Day 09b

day09b =
 (minimum $ day09b_calc data09) +
  (maximum $ day09b_calc data09)

day09b_calc (curr:rest)
 | checked == [] =
  day09b_calc rest
 | otherwise =
  checked
 where
  checked = day09b_check [curr] curr rest
 

day09b_check list prevSum (curr:rest)
 | newSum > day09a =
  []
 | newSum == day09a =
  newList
 | otherwise =
  day09b_check newList newSum rest
 where
  newSum = prevSum + curr
  newList = curr:list

-- Day 09a

day09a =
 day09a_check (reverse $ take 25 data09) $ drop 25 data09

day09a_check last25 (curr:rest)
 | elem curr sums =
  day09a_check (curr:(init last25)) rest
 | otherwise =
  curr
 where
  sums = [x + y | x <- last25, y <- last25, x /= y]

 
-- Day 08b

day08b = fromJust $ head day08b_calc

day08b_calc = filter (/= Nothing) output
 where
  output =
   map (\x -> day08b_vgCPU x [] 0 0)
    day08b_instructionsList

day08b_vgCPU instructions pointerList pointer acc
 | pointer > day08a_max = Just acc
 | elem pointer pointerList = Nothing
 | otherwise =
  day08b_vgCPU instructions (pointer:pointerList)
   newPointer newAcc
 where
  (op,value) = instructions ! pointer
  (newPointer,newAcc) =
   vgCPU_apply op pointer value acc
   
day08b_instructionsList =
 day08b_jmpToNop ++ day08b_nopToJmp

day08b_jmpToNop =
 map replace day08b_jmps
 where
  replace (pointer,(op,value)) =
   day08a_instructions // [(pointer,(NOP,value))]

day08b_nopToJmp =
 map replace day08b_nops
 where
  replace (pointer,(op,value)) =
   day08a_instructions // [(pointer,(JMP,value))]

day08b_jmps =
 filter (\x -> JMP == (fst $ snd x)) $
  zip [0..day08a_max]  day08a_instructionList
   
day08b_nops =
 filter (\x -> NOP == (fst $ snd x)) $
  zip [0..day08a_max] day08a_instructionList

-- Day 08a

day08a = day08a_vgCPU [] 0 0

day08a_vgCPU pointerList pointer acc
 | elem pointer pointerList = acc
 | otherwise =
  day08a_vgCPU (pointer:pointerList)
   newPointer newAcc
 where
  (op,value) = day08a_instructions ! pointer
  (newPointer,newAcc) =
   vgCPU_apply op pointer value acc
  
vgCPU_apply operation pointer value acc =
 case operation of
  ACC -> (pointer + 1,acc + value)
  JMP -> (pointer + value, acc)
  NOP -> (pointer + 1,acc)
  
day08a_max = snd $ bounds day08a_instructions

day08a_instructions =
 listToArray day08a_instructionList

day08a_instructionList =
 map getInstruction $ day08a_lines
 where
  getInstruction string =
   (op,value)
   where
    split = splitOn ' ' string
    op =
     read $ map toUpper $ head split :: Operation
    value =
     readInt $ filter (/= '+') $ last split

day08a_lines = splitOn ';' data08
 
-- Day 07b

day07b =
 (day07b_countBags $ day07a_getBag "shiny gold") - 1

day07b_countBags (Bag color [(0,"")]) = 1
day07b_countBags (Bag color list) =
 (sum bagAmounts) + 1
 where
  (amounts,colors) = unzip list
  bags = map day07a_getBag colors
  bagAmounts =
   map (\(n,x) -> n * (day07b_countBags x)) $
    zip amounts bags

-- Day 07a

day07a = length day07a_calc

day07a_calc =
 filter (day07a_checkBags "shiny gold") bagList
 where
  bagList = Map.elems day07a_bags

day07a_checkBags check (Bag _ [(0,"")]) = False
day07a_checkBags check (Bag _ list)
 | any (== check) containedColors =
  True
 | otherwise =
  any (day07a_checkBags check) containedBags
 
 where
  containedColors = map snd list
  containedBags =
   map day07a_getBag containedColors

day07a_getBag color = 
 Map.findWithDefault
  (Bag color [(0,"")]) color day07a_bags


day07a_bags = Map.fromList $ zip bagColors bags
 where
  containSplit =
   map (splitOnString " contain ") day07a_lines
  bagColors =
   map (head . splitOnString " bags" . head) containSplit
  contains =
   map (splitOnString ", " . last) containSplit
  containsFiltered =
   map (map (head . splitOnString " bag")) contains
  containsNumberColor =
   map (map $ span isDigit) containsFiltered
  containsRead =
   map (map readContains) containsNumberColor
  readContains ("",b) = (0,"")
  readContains (a,b) = (readInt a, tail b)
  bags =
   map makeBag $ zip bagColors containsRead
  
  makeBag (color,(list)) = Bag color list

day07a_lines = splitOn ';' data07
 
-- Day 06b

day06b =
 foldl (\acc x -> acc + (length (head x))) 0 day06b_calc

day06b_calc =
 day06b_filterChar chars day06b_groups
 where
  chars = ['a'..'z']

day06b_filterChar [] groups = groups
day06b_filterChar (char:rest) groups
 = day06b_filterChar rest filteredGroups
 where
  filteredGroups =
   map (day06b_filter char) groups

day06b_filter char group
 | all (elem char) group =
  group
 | otherwise =
  map (filter (/= char)) group

day06b_groups = map (splitOn ',') day06a_lines
 
-- Day 06a

day06a =
 foldl (\acc x -> acc + (length x)) 0 day06a_nubbed

day06a_nubbed = nubbed
 where
  filtered = map (filter (/= ',')) day06a_lines
  sorted = map sort filtered
  nubbed = map nub sorted

day06a_lines = splitOn ';' data06
 
-- Day 05b

day05b = day05b_calc 0 day05a_calc

day05b_calc :: Int -> [[Int]] -> Int
day05b_calc _ [] = 0
day05b_calc 0 ((current:b:c:[]):rest) =
 day05b_calc current rest
day05b_calc last ((current:b:c:[]):rest)
 | last + 1 == current =
  day05b_calc current rest
 | otherwise = last + 1
 
-- Day 05a

day05a = head $ last day05a_calc

day05a_calc :: [[Int]]
day05a_calc =
 sort $ map day05a_process day05a_passes

day05a_process (frontback,leftright) =
 [row * 8 + col,row,col]
 where
  row = day05a_binComp 128 frontback
  col = day05a_binComp 8 leftright

day05a_binComp n [] = 0

day05a_binComp n (pop:rest)
 | elem pop "FL" =
  0 + (day05a_binComp half rest)
 | otherwise =
  half + (day05a_binComp half rest)
 where
  half = div n 2

day05a_passes = map (splitAt 7) day05a_lines

day05a_lines = splitOn ';' data05

-- Day 04b

day04b = length day04b_calc

day04b_calc = filter id day04b_checkMaps

day04b_checkMaps = map day04b_check day04a_maps

day04b_check entryMap = all id checked 
 where
  types = tail day04a_types
  values = map (`Map.lookup` entryMap) types
  zipped = zip types values
  checked = map day04b_checkItem zipped

day04b_checkItem (key, Nothing) = False

day04b_checkItem ("byr", Just value) =
 intValue >= 1920 && intValue <= 2002
 where
  intValue = readInt value

day04b_checkItem ("iyr", Just value) =
 intValue >= 2010 && intValue <= 2020
 where
  intValue = readInt value
  
day04b_checkItem ("eyr", Just value) =
 intValue >= 2020 && intValue <= 2030
 where
  intValue = readInt value
  
day04b_checkItem ("hgt", Just value)
 | unit == "cm" =
  intValue >= 150 && intValue <= 193
 | unit == "in" =
  intValue >= 59 && intValue <= 76
 where
  unit = filter (`elem` ['a'..'z']) value
  digits = filter (`elem` ['0'..'9']) value
  intValue = readInt digits

day04b_checkItem ("hcl", Just value)
 = l2 == 6 && l1 == 7 && leading == '#'
 where
  leading = head value
  elems = ['0'..'9'] ++ ['a'..'f']
  filteredHex = filter (`elem` elems) $ tail value
  l1 = length value
  l2 = length filteredHex
  
day04b_checkItem ("ecl", Just value)
 = elem value colors
 where
  colors =
   ["amb","blu","brn","gry","grn","hzl","oth"] 
  
day04b_checkItem ("pid", Just value)
 = l2 == 9 && l1 == 9
 where
  elems = ['0'..'9']
  filtered = filter (`elem` elems) value
  l1 = length value
  l2 = length filtered

day04b_checkItem (key, Just value) = False

-- Day 04a

day04a = length day04a_calc

day04a_calc = filter id day04a_checkMaps

day04a_checkMaps = map day04a_check day04a_maps

day04a_check entryMap = all id checked
 where
  types = tail day04a_types
  checked = map (`Map.member` entryMap) types

day04a_maps = map (Map.fromList) assocLists
 where
  assocLists =
   map (map $ toAssoc . splitOn ':') day04a_entries
  toAssoc (a:b:[]) = (a,b)
  
day04a_entries = map (splitOn ',') day04a_lines

day04a_lines = splitOn ';' data04

day04a_types =
 ["cid","byr","iyr","eyr","hgt","hcl","ecl","pid"]

-- Day 03b

day03b = foldr1 (*) day03b_calc

day03b_calc = map (\v -> day03a_count v day03a_start 0) day03b_vectors

day03b_vectors =
 [(1, 1),(3, 1),(5, 1),(7, 1),(1, 2)]

-- Day 03a

day03a = day03a_count day03a_vector day03a_start 0

day03a_count vector p@(Point x y) trees
 | y > day03a_yMax =
  trees
 | check == Tree =
  day03a_count vector next $ trees + 1
 | check == Ground =
  day03a_count vector next trees
 where
  check = day03a_woodArray ! p
  next = day03a_move vector p
 
day03a_woodArray =
 array (Point 0 0,Point day03a_xMax day03a_yMax) day03a_list

day03a_list = assocList
 where
  coords =
   [Point x y | y <- [0..day03a_yMax], x <- [0..day03a_xMax]]
  assocList = zip coords $ map charToTobogan $ concat day03a_lines
  charToTobogan '.' = Ground
  charToTobogan '#' = Tree
  
day03a_move vector (Point x y) =
 Point
  (mod (x + (fst vector)) day03a_xLength)
  (y + (snd vector))

day03a_vector = (3,1)
day03a_start = Point 0 0

day03a_xMax = day03a_xLength - 1
day03a_yMax = day03a_yLength - 1
day03a_xLength = length $ head day03a_lines
day03a_yLength = length day03a_lines

day03a_lines = splitOn ';' data03

-- Day 02b

day02b = length day02b_correct

day02b_correct = filter day02b_check day02a_passwords
 
day02b_check (Password int1 int2 char string) =
 elem1 /= elem2
 where
  char1 = string !! (int1 - 1)
  char2 = string !! (int2 - 1)
  elem1 = char1 == char
  elem2 = char2 == char

-- Day 02a
 
day02a = length day02a_correct
 
day02a_correct = filter day02a_check day02a_passwords
 
day02a_check (Password int1 int2 char string) =
 filtered_length >= int1 && filtered_length <= int2
 where
  filtered = filter (== char) string
  filtered_length = length filtered
 
day02a_passwords = map assign day02a_split
 where
  assign (a:b:c:d:e) =
   Password int1 int2 char string
   where
    int1 = readInt a
    int2 = readInt b
    char = head c
    string = head e

day02a_split = map (splitOnList "-: ") day02a_lines

day02a_lines = splitOn ';' data02

-- Day 01b

day01b = head day01b_calc

day01b_calc = day01b_first data01

day01b_first (first:rest)
 | rest == [] =
  [0,0,0]
 | checked == [] =
  day01b_first rest
 | otherwise =
  checked
 where
  checked = day01b_second first rest
  
day01b_second first (current:rest)
 | rest == [] =
  []
 | checked == [] =
  day01b_second first rest
 | otherwise =
  checked
 where
  checked = day01b_third first current rest

day01b_third first second (current:rest)
 | rest == [] =
  []
 | first + second + current == day01a_magic =
  [multiplied,first,second,current]
 | otherwise =
  day01b_third first second rest
 where
  multiplied = first * second * current

-- Day 01a

day01a = head day01a_calc

day01a_calc = day01a_first data01

day01a_first (first:list)
 | list == [] =
  [0,0,0]
 | checked == [] =
  day01a_first list
 | otherwise =
  checked
 where
  checked = day01a_second first list

day01a_second first (current:rest)
 | rest == [] =
  []
 | first + current == day01a_magic =
  [first * current,first,current]
 | otherwise =
  day01a_second first rest

day01a_magic = 2020

-- Commons

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn element list =
 foldr
  (\x acc@(acc_h:acc_t) ->
   if x == element
    then []:acc
    else (x:acc_h):acc_t)
  [[]] list

splitOnString :: Eq a => [a] -> [a] -> [[a]]
splitOnString string list =
 reverse $ map reverse $ foldIt string list [[]]
 where
  foldIt _ [] reverseOutput =
   reverseOutput
  foldIt string list@(currList:restList)
   reverseOutput@(currOut:restOut)
   | l2 < l1 =
    ((reverse list) ++ currOut):restOut
   | string == compare =
    foldIt string dropped ([]:reverseOutput)
   | otherwise =
    foldIt string restList
     ((currList:currOut):restOut)
   where
    l1 = length string
    l2 = length list
    compare = take l1 list
    dropped = drop l1 list

splitOnList :: Eq a => [a] -> [a] -> [[a]]
splitOnList elementList list =
 foldr
  (\x acc@(acc_h:acc_t) ->
   if elem x elementList
    then []:acc
    else (x:acc_h):acc_t)
  [[]] list

spoolOnList :: Eq a => [a] -> [a] -> [[a]]
spoolOnList elementList list =
 foldr
  (\x acc@(acc_h:acc_t) ->
   if elem x elementList
    then []:[x]:acc
    else (x:acc_h):acc_t)
  [[]] list
  
spoolList n [] = []
spoolList n list = taken:(spoolList n dropped)
 where
  (taken,dropped) = splitAt n list

readInt string = read string :: Int

fst' (a,b,c) = a
snd' (a,b,c) = b
trd' (a,b,c) = c

listToArray list =
 array (0,max) $ zip [0..max] list
 where
  max = (length list) - 1
  
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _   []                        = []
groupBy' _   [x]                       = [[x]]
groupBy' cmp (x:xs@(x':_)) | cmp x x'  = (x:y):ys
                           | otherwise = [x]:r
  where r@(y:ys) = groupBy' cmp xs
  
choicesFromList list =
 getChoices list [[]]
 where
  getChoices [] output = output
  getChoices (a:rest) output =
   getChoices rest $ output ++ (map (a:) output)

turnDirection Clockwise North = East 
turnDirection Clockwise East = South
turnDirection Clockwise South  = West
turnDirection Clockwise West = North

turnDirection Counterclockwise North = West
turnDirection Counterclockwise East = North
turnDirection Counterclockwise South = East
turnDirection Counterclockwise West = South

turnVector Counterclockwise (Point x y) =
 (Point y (-x))
turnVector Clockwise (Point x y) =
 (Point (-y) x)

movePoint (Point x y) North = (Point x (y - 1))
movePoint (Point x y) East = (Point (x + 1) y)
movePoint (Point x y) South = (Point x (y + 1))
movePoint (Point x y) West = (Point (x - 1) y)

movePointByList p [] = p
movePointByList p (x:xs) =
 movePointByList (movePoint p x) xs

adjacentPoints p =
 map (movePoint p) [North,East,South,West]

surroundingPoints p =
 map (movePointByList p) surroundingDirections

surroundingDirections =
 [[North,West],[North],[North,East],[West],[East],
   [South,West],[South],[South,East]]

equaling f a b = f a == f b

manhattanDistance (Point x1 y1) (Point x2 y2) =
 abs (x1 - x2) + abs (y1 - y2)

addPoints (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)
 
-- swap (a,b) = (b,a)

-- findInList index list
 -- | index < 0 || index >= length list =
  -- Nothing
 -- | otherwise =
  -- Just $ list !! index

-- pointListMinMax list =
 -- foldl getMinMax
    -- ((\(Point a b) -> (a,b,a,b)) $ head list) $
    -- tail list
 -- where
  -- getMinMax (xMin,yMin,xMax,yMax) (Point x y) =
   -- (minX,minY,maxX,maxY)
   -- where
    -- minX = if x < xMin then x else xMin
    -- minY = if y < yMin then y else yMin
    -- maxX = if x > xMax then x else xMax
    -- maxY = if y > yMax then y else yMax

-- takeWhileAscendingBy f [] = []    
-- takeWhileAscendingBy f [x1] = [x1]
-- takeWhileAscendingBy f (x1:x2:xs)
 -- | f x1 x2 /= GT =
  -- x1:(takeWhileAscendingBy f (x2:xs))
 -- | otherwise =
  -- [x1]
  
-- -- from big endian binary list
-- fromBinaryList list = getFromBinaryList 1 list
-- getFromBinaryList :: Int -> [Int] -> Int
-- getFromBinaryList n [0] = 0
-- getFromBinaryList n [1] = n
-- getFromBinaryList n (current:next) =
 -- current * n + getFromBinaryList (n * 2) next
 
-- takeWithDefault def n list
 -- | length list < n =
  -- take n $ list ++ (replicate n def)
 -- | otherwise =
  -- take n list
  


-- readingOrder points =
 -- sortBy (\(Point a b) (Point c d) -> compare (b,a) (d,c)) points