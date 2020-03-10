import Data.List
import Data.Array
import Data.Foldable
import Data.Char
import Data.Maybe
import Data.Ord

import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import AdventOfCodeData

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

data Point =
 Point Int Int
 deriving (Show, Eq, Ord)

data Rectangle =
 Rectangle Point Int Int
 deriving (Show, Eq, Ord)

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