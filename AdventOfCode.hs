import Data.List
import Data.Array
import Data.Foldable
import Data.Char
import Data.Maybe
-- import Data.Ord
-- import Data.Function
-- import Data.Bits
-- import Control.Concurrent
-- import Data.Hashable

-- import GHC.Generics (Generic)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
-- import qualified Data.HashPSQ as HPSQ

import AdventOfCodeData

data Point =
 Point Int Int
 deriving (Show, Eq, Ord, Ix)

data Tobogan =
 Tree | Ground
 deriving (Eq, Show)
 
data Password =
 Password Int Int Char String
 deriving (Show, Eq)
 
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

readInt string = read string :: Int

fst' (a,b,c) = a
snd' (a,b,c) = b
trd' (a,b,c) = c

-- equaling f a b = f a == f b

-- manhattanDistance (Point x1 y1) (Point x2 y2) =
 -- abs (x1 - x2) + abs (y1 - y2)
 
-- swap (a,b) = (b,a)

-- findInList index list
 -- | index < 0 || index >= length list =
  -- Nothing
 -- | otherwise =
  -- Just $ list !! index
  
-- addPoints (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

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
  
-- movePoint (Point x y) North = (Point x (y - 1))
-- movePoint (Point x y) East = (Point (x + 1) y)
-- movePoint (Point x y) South = (Point x (y + 1))
-- movePoint (Point x y) West = (Point (x - 1) y)

-- movePointByList p [] = p
-- movePointByList p (x:xs) =
 -- movePointByList (movePoint p x) xs


-- turnDirection North Clockwise = East 
-- turnDirection East Clockwise = South
-- turnDirection South Clockwise = West
-- turnDirection West Clockwise = North

-- turnDirection North Counterclockwise = West
-- turnDirection East Counterclockwise = North
-- turnDirection South Counterclockwise = East
-- turnDirection West Counterclockwise = South

-- adjacentPoints p = map (movePoint p) [North,East,South,West]

-- surroundingPoints p =
 -- map (movePointByList p)
  -- [[North,West],[North],[North,East],[West],[East],
   -- [South,West],[South],[South,East]]


-- readingOrder points =
 -- sortBy (\(Point a b) (Point c d) -> compare (b,a) (d,c)) points
 
-- listToArray list =
 -- array (0,max) $ zip [0..max] list
 -- where
  -- max = (length list) - 1