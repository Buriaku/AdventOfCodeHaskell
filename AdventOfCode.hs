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

-- Day 01b

-- Day 01a

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

-- splitOn :: Eq a => a -> [a] -> [[a]]
-- splitOn element list =
 -- foldr
  -- (\x acc@(acc_h:acc_t) ->
   -- if x == element
    -- then []:acc
    -- else (x:acc_h):acc_t)
  -- [[]] list

-- splitOnList :: Eq a => [a] -> [a] -> [[a]]
-- splitOnList elementList list =
 -- foldr
  -- (\x acc@(acc_h:acc_t) ->
   -- if elem x elementList
    -- then []:acc
    -- else (x:acc_h):acc_t)
  -- [[]] list
  
-- spoolList n [] = []
-- spoolList n list = (take n list):(spoolList n (drop n list))

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
  
-- readInt string = read string :: Int

-- fst' (a,b,c) = a
-- snd' (a,b,c) = b
-- trd' (a,b,c) = c