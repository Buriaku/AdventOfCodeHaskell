{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}

import Data.List
import Data.Array
import Data.Foldable
import Data.Char
import Data.Maybe
import Data.Ord

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
-- import qualified Data.Field.Galois (Prime, Extension, IrreducibleMonic(poly), Binary,pattern X, pattern X2, pattern X3, pattern Y) as Gal
import Data.Field.Galois (Prime, Extension, IrreducibleMonic(poly), Binary,pattern X, pattern X2, pattern X3, pattern Y)

import AdventOfCodeData
import AdventOfCodeDataOld


--Day 01a

day01a = day01a_calc data_day01a

day01a_calc :: (Integral a) => [a] -> a
day01a_calc m_list = foldl (\acc m -> acc + (fuelEquation m)) 0 m_list

fuelEquation:: (Integral a) => a -> a
fuelEquation m = floor((fromIntegral m) / 3) - 2

--Day 01b

day01b = day01b_calc data_day01a

day01b_calc :: (Integral a) => [a] -> a
day01b_calc m_list = foldl (\acc m -> acc + recursiveFuel(m)) 0 m_list

recursiveFuel:: (Integral a) => a -> a
recursiveFuel m
 | fuel <= 0 = 0
 | otherwise = fuel + recursiveFuel fuel
 where fuel = fuelEquation m

-- Day 02a

day02a = (day02a_calc data_day02a 0)

data_day02a = data_day02_prep 12 2

data_day02_prep noun verb = 1:noun:verb:(snd (splitAt 3 data_day02a_raw))

day02a_calc :: [Int] -> Int -> ([Int], [Char])
day02a_calc intCode position
 | opCode == 1  =
  day02a_calc (fst split ++ (operand1 + operand2):tail(snd split)) (position + 4)
  --([operand1+operand2,operand1,operand2,opPosition],"Test opCode 1")
 | opCode == 2  =
  day02a_calc (fst split ++ (operand1 * operand2):tail(snd split)) (position + 4)             
  --([operand1*operand2,operand1,operand2,opPosition],"Test opCode 2")
 | opCode == 99  = (intCode, "Done 99 at position " ++ show position)
 | otherwise     = (intCode, "Error" ++ show opCode ++ "at position " ++ show position)
 where
  opCode = intCode !! position
  operand1 = intCode !! (intCode !! (position + 1))
  operand2 = intCode !! (intCode !! (position + 2))
  opPosition = intCode !! (position + 3)
  split = splitAt opPosition intCode

-- Day 02b

day02b_calc intCode = head (fst (day02a_calc intCode 0))

day02b = [(day02b_calc (data_day02_prep noun verb), noun, verb) | verb <- [0..99], noun <- [0..99], day02b_calc (data_day02_prep noun verb) == 19690720] 
 --where
 -- list = concat(replicate 100 [0..99])
 -- tupleList = zip list (sort list)
 
-- Day 03a

day03a = take 1 (sort data_day03a)

data_day03a = [data_day03a_output pa1 pa2 pb1 pb2 | a@(pa1@(ax1,ay1),pa2@(ax2,ay2)) <- data_day03a_wire_a, b@(pb1@(bx1,by1),pb2@(bx2,by2)) <- data_day03a_wire_b, (ax1 == ax2 && by1 == by2) || (bx1 == bx2 && ay1 == ay2), if (ax1 == ax2) then ((ay1 < by1) == (ay2 > by1)) && ((bx1 < ax1) == (bx2 > ax1)) else ((by1 < ay1) == (by2 > ay1)) && ((ax1 < bx1) == (ax2 > bx1))]

data_day03a_output pa1@(ax1,ay1) pa2@(ax2,ay2)pb1@(bx1,by1) pb2@(bx2,by2)
 | ax1 == ax2 = (abs ax1 + abs by1,ax1,by1)
 | otherwise  = (abs bx1 + abs ay1,bx1,ay1)

data_day03a_wire_a = zip ((0,0):a) a
 where
  a = data_day03a_spool_wire (head data_day03a_prep) (0,0)

data_day03a_wire_b = zip ((0,0):b) b
 where
  b = data_day03a_spool_wire (last data_day03a_prep) (0,0)

data_day03a_spool_wire [] point = []

data_day03a_spool_wire list@(instruction@(heading,distance):tail) point@(x,y)
 | heading == 'U' = (x,y + distance):(data_day03a_spool_wire tail (x,y + distance))
 | heading == 'D' = (x,y - distance):(data_day03a_spool_wire tail (x,y - distance))
 | heading == 'R' = (x + distance,y):(data_day03a_spool_wire tail (x + distance,y))
 | heading == 'L' = (x - distance,y):(data_day03a_spool_wire tail (x - distance,y))
 
data_day03a_prep = b
 where
  a = map (splitOn ',') $ splitOn ';' data_day03a_raw
  b = map (foldr (\x@(x_h:x_t) acc -> (x_h, read x_t :: Int):acc) []) a

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn element list = foldr (\x acc@(acc_h:acc_t) -> if x == element then []:acc else (x:acc_h):acc_t) [[]] list

-- Day 03b

day03b = take 1 (sort data_day03b)

data_day03b = [data_day03b_output pa1 pa2 pb1 pb2 (a_length + b_length) | a@(pa1@(ax1,ay1),pa2@(ax2,ay2),a_length) <- data_day03b_wire_a, b@(pb1@(bx1,by1),pb2@(bx2,by2),b_length) <- data_day03b_wire_b, (ax1 == ax2 && by1 == by2) || (bx1 == bx2 && ay1 == ay2), if (ax1 == ax2) then ((ay1 < by1) == (ay2 > by1)) && ((bx1 < ax1) == (bx2 > ax1)) else ((by1 < ay1) == (by2 > ay1)) && ((ax1 < bx1) == (ax2 > bx1))]

data_day03b_output pa1@(ax1,ay1) pa2@(ax2,ay2) pb1@(bx1,by1) pb2@(bx2,by2) length
 | ax1 == ax2 = (length - abs (ay2-by2) - abs (bx2-ax2),ax1,by1)
 | otherwise  = (length - abs (by2-ay2) - abs (ax2-bx2),bx1,ay1)

data_day03b_wire_a = foldl data_day03b_wire_acc [] data_day03a_wire_a

data_day03b_wire_b = foldl data_day03b_wire_acc [] data_day03a_wire_b

data_day03b_wire_acc :: (Num a) => [((a,a),(a,a),a)] -> ((a,a),(a,a)) -> [((a,a),(a,a),a)]
data_day03b_wire_acc [] x@(p1@(x1,y1),p2@(x2,y2)) = [(p1,p2,abs (x1 - x2) + abs (y1 - y2))]
data_day03b_wire_acc acc x@(p1@(x1,y1),p2@(x2,y2)) = acc ++ [(p1,p2,(\(_,_,l) -> l) (last acc) + abs (x1 - x2) + abs (y1 - y2))]

-- Day 04a

day04a = length day04a_output

day04a_output = [x | x@(a:b:c:d:e:f:[]) <- data_day04a, (a == b) || (b == c) || (c == d) || (d == e) || (e == f), (a <= b) && (b <= c) && (c <= d) && (d <= e) && (e <= f)]

data_day04a = map show data_day04a_raw

-- Day 04b

day04b = length day04b_output

day04b_output = [x | x@(a:b:c:d:e:f:[]) <- data_day04a, ((a == b) && (b /= c)) || ((a /= b) && (b == c) && (c /= d)) || ((b /= c) && (c == d) && (d /= e)) || ((c /= d) && (d == e) && (e /= f)) || ((d /= e) && (e == f)), (a <= b) && (b <= c) && (c <= d) && (d <= e) && (e <= f)]

-- Day 05a

day05a = day05a_calc data_day05a 0 [1] []

-- updated for  Day 7a
day05a_calc :: [Int] -> Int -> [Int] -> [Int] -> ([Int], ([Char], Int, Int), [Int], [Int])

-- opCodes with three arguments
day05a_calc intCode position input output
 | opCode == 1   = day05a_calc (fst split_arg3 ++ (arg1 + arg2):tail(snd split_arg3)) (position + 4) input output
 | opCode == 2   = day05a_calc (fst split_arg3 ++ (arg1 * arg2):tail(snd split_arg3)) (position + 4) input output
 | opCode == 7   = day05a_calc (fst split_arg3 ++ (boolToInt (arg1 < arg2)):tail(snd split_arg3)) (position + 4) input output
 | opCode == 8   = day05a_calc (fst split_arg3 ++ (boolToInt (arg1 == arg2)):tail(snd split_arg3)) (position + 4) input output
 where
  opCode_raw = intCode !! position
  opCode = mod opCode_raw 100
  
  param1 = div (mod opCode_raw 1000 - mod opCode_raw 100) 100
  arg1 = if param1 == 0
          then intCode !! (intCode !! (position + 1))
          else intCode !! (position + 1)

  param2 = div (mod opCode_raw 10000 - mod opCode_raw 1000) 1000
  arg2 = if param2 == 0
          then intCode !! (intCode !! (position + 2))
          else intCode !! (position + 2)
  
  -- write operation never immediate? never position???
  arg3 = intCode !! (position + 3)
  {-
  param3 = div (mod opCode_raw 100000 - mod opCode_raw 10000) 10000
  arg3 = if param3 == 0
          then intCode !! (intCode !! (position + 3))
          else intCode !! (position + 3)-}

  split_arg3 = splitAt arg3 intCode

-- opCodes with two arguments
day05a_calc intCode position input output
 | opCode == 5   = day05a_calc intCode (if (arg1 /= 0) then arg2 else (position + 3)) input output
 | opCode == 6   = day05a_calc intCode (if (arg1 == 0) then arg2 else (position + 3)) input output
 
 where
  opCode_raw = intCode !! position
  opCode = mod opCode_raw 100
  
  param1 = div (mod opCode_raw 1000 - mod opCode_raw 100) 100
  arg1 = if param1 == 0
          then intCode !! (intCode !! (position + 1))
          else intCode !! (position + 1)

  param2 = div (mod opCode_raw 10000 - mod opCode_raw 1000) 1000
  arg2 = if param2 == 0
          then intCode !! (intCode !! (position + 2))
          else intCode !! (position + 2)
  
  -- write operation never immediate? never position???
  --arg3 = intCode !! (position + 3)
  {-
  param3 = div (mod opCode_raw 100000 - mod opCode_raw 10000) 10000
  arg3 = if param3 == 0
          then intCode !! (intCode !! (position + 3))
          else intCode !! (position + 3)-}

  --split_arg3 = splitAt arg3 intCode

-- opCodes with one arguments  
day05a_calc intCode position input output 
 | opCode == 3   = if input == []
                    then (intCode, ("Halt", opCode, position), input, output)
                    else day05a_calc (fst split_arg1 ++ (head input):tail(snd split_arg1)) (position + 2) (tail input) output
 | opCode == 4   = day05a_calc intCode (position + 2) input (arg1:output)
 where
  opCode_raw = intCode !! position
  opCode = mod opCode_raw 100
  split_arg1 = splitAt arg1 intCode
  
  param1 = div (mod opCode_raw 1000 - mod opCode_raw 100) 100
  arg1 = if (param1 == 0) && (opCode /= 3)
          then intCode !! (intCode !! (position + 1))
          else intCode !! (position + 1)

-- opCodes with no arguments
day05a_calc intCode position input output
 | opCode == 99  = (intCode, ("Done", opCode, position), input, output)
 | otherwise     = (intCode, ("Error", opCode, position), input, output)
 where
  opCode_raw = intCode !! position
  opCode = mod opCode_raw 100

-- Day 05b

day05b = day05a_calc data_day05a 0 [5] []

-- boolToInt for Day 05b  
boolToInt bool = if bool then 1 else 0

-- Day 06a

-- updated to not be dirty anymore using Data.Map

day06a = foldr (\x acc -> acc + (length (findPathToCOM x)) - 1) 0 data_day06a_list

findPathToCOM "COM" = ["COM"]
findPathToCOM object = object:(findPathToCOM parent)
 where parent = (Map.!) data_day06a object

data_day06a_list = Map.keys data_day06a

data_day06a = Map.fromList (map ((\[a,b] -> (b,a)).(splitOn ')')) $ splitOn ';' data_day06a_raw)


-- Day 06b

day06b = (length santaPath) + (length yourPath) - (2 * (length (intersect santaPath yourPath))) - 2

santaPath = findPathToCOM "SAN"

yourPath = findPathToCOM "YOU"

-- Day 07a

day07a = reverse(sort (map (day07a_calc) data_day07a_codelist))

day07a_calc (code0:code1:code2:code3:code4:[]) = amp_output output_amp_4
 where
  amp_output (_,_,_,(code:_)) = code 
  output_amp_0 = day05a_calc data_day07a 0 [code0,0] []
  output_amp_1 = day05a_calc data_day07a 0 [code1,(amp_output output_amp_0)] []
  output_amp_2 = day05a_calc data_day07a 0 [code2,(amp_output output_amp_1)] []
  output_amp_3 = day05a_calc data_day07a 0 [code3,(amp_output output_amp_2)] []
  output_amp_4 = day05a_calc data_day07a 0 [code4,(amp_output output_amp_3)] []

data_day07a_codelist = [[a, b, c, d, e] | a <- [0..4], b <- [0..4], c <- [0..4], d <- [0..4], e <- [0..4], a /= b, a /= c, a /= d, a /= e, b /= c, b /= d, b /= e, c /= d, c /= e, d /= e]

-- Day 07b

--day07b = reverse(sort (map (day07a_calc) data_day07a_codelist))

day07b = reverse(sort (map (day07b_output) data_day07b_codelist))

day07b_output codes = day07b_calc (map (\x -> [x]) codes) (replicate 5 data_day07a) (replicate 5 0) 0

day07b_calc inputs@(input0:input1:input2:input3:[input4]) (memory0:memory1:memory2:memory3:[memory4]) (position0:position1:position2:position3:[position4]) input_amp_0 =
 if amp_opCode(output_amp_4) == 3
  then
   day07b_calc
    (replicate 5 [])
    ((amp_memory output_amp_0):(amp_memory output_amp_1):(amp_memory output_amp_2):(amp_memory output_amp_3):[amp_memory output_amp_4])
    ((amp_position output_amp_0):(amp_position output_amp_1):(amp_position output_amp_2):(amp_position output_amp_3):[amp_position output_amp_4])
    (amp_output output_amp_4)
  else amp_output output_amp_4
 where
  amp_opCode (_,(_,opCode,_),_,_) = opCode
  amp_position (_,(_,_,position),_,_) = position
  amp_memory (memory,_,_,(input:_)) = memory
  amp_output (_,_,_,(output:_)) = output
  output_amp_0 = day05a_calc memory0 position0 (input0 ++ [input_amp_0]) []
  output_amp_1 = day05a_calc memory1 position1 (input1 ++ [(amp_output output_amp_0)]) []
  output_amp_2 = day05a_calc memory2 position2 (input2 ++ [(amp_output output_amp_1)]) []
  output_amp_3 = day05a_calc memory3 position3 (input3 ++ [(amp_output output_amp_2)]) []
  output_amp_4 = day05a_calc memory4 position4 (input4 ++ [(amp_output output_amp_3)]) []

data_day07b_codelist = [[a, b, c, d, e] | a <- [5..9], b <- [5..9], c <- [5..9], d <- [5..9], e <- [5..9], a /= b, a /= c, a /= d, a /= e, b /= c, b /= d, b /= e, c /= d, c /= e, d /= e]

-- Day 08a

day08a = (\([_,a,b]) -> (length a) * (length b) ) (group . head $ data_day08a_sorted_layers)

data_day08a_sorted_layers = reverse . sort . map (sort) $ data_day08a_layers

data_day08a_layers = spoolList (25*6) data_day08a

--spoolList :: Int -> String -> [String]
spoolList n [] = []
spoolList n string = (take n string):(spoolList n (drop n string))

-- Day 08b

day08b = spoolList 25 [day08b_pixel n data_day08a_layers | n <- [0..((25*6)-1)]]

day08b_pixel n list
 | pixel == '2' = day08b_pixel n (tail list)
 | otherwise    = pixel
 
 where pixel = (head list) !! n
 
-- Day 09a

day09a = cICC_output (cICC_init data_day09a) 0 0 [1]

cICC_init program = Map.fromList [(n, (program !! n)) | n <- [0..((length program)-1)]]

cICC_output memory position relativeBase input  = (\(_,a,b,c,d,e,f) -> (a,b,c,d,e,f)) (cICC memory position relativeBase input [])

cICC_output_raw memory position relativeBase input  = cICC memory position relativeBase input []

-- Complete IntCode Computer
-- cICC :: [Int] -> Int -> Int -> [Int] -> [Int] -> ([Int], [Char], Int, Int, Int, [Int], [Int])

-- opCodes with three arguments
cICC intMap position relativeBase input output
 | opCode == 1   = cICC (Map.insert arg3 (arg1 + arg2) intMap) (position + 4) relativeBase input output
 | opCode == 2   = cICC (Map.insert arg3 (arg1 * arg2) intMap) (position + 4) relativeBase input output
 | opCode == 7   = cICC (Map.insert arg3 (boolToInt (arg1 < arg2)) intMap) (position + 4) relativeBase input output
 | opCode == 8   = cICC (Map.insert arg3 (boolToInt (arg1 == arg2)) intMap) (position + 4) relativeBase input output
 where
  opCode_raw = intMapFind position intMap
  opCode = mod opCode_raw 100
  
  param1 = div (mod opCode_raw 1000 - mod opCode_raw 100) 100
  arg1 = case param1 of
          0 -> intMapFind (intMapFind (position + 1) intMap) intMap
          1 -> intMapFind (position + 1) intMap
          2 -> intMapFind (relativeBase + (intMapFind (position + 1) intMap)) intMap

  param2 = div (mod opCode_raw 10000 - mod opCode_raw 1000) 1000
  arg2 = case param2 of
          0 -> intMapFind (intMapFind (position + 2) intMap) intMap
          1 -> intMapFind (position + 2) intMap
          2 -> intMapFind (relativeBase + (intMapFind (position + 2) intMap)) intMap
  
  -- <bullshit>
  param3 = div (mod opCode_raw 100000 - mod opCode_raw 10000) 10000
  arg3 = case param3 of
          0 -> intMapFind (position + 3) intMap
          2 -> relativeBase + (intMapFind (position + 3) intMap)
  -- </bullshit>

-- opCodes with two arguments
cICC intMap position relativeBase input output
 | opCode == 5   = cICC intMap (if (arg1 /= 0) then arg2 else (position + 3)) relativeBase input output
 | opCode == 6   = cICC intMap (if (arg1 == 0) then arg2 else (position + 3)) relativeBase input output
 
 where
  opCode_raw = intMapFind position intMap
  opCode = mod opCode_raw 100
    
  param1 = div (mod opCode_raw 1000 - mod opCode_raw 100) 100
  arg1 = case param1 of
          0 -> intMapFind (intMapFind (position + 1) intMap) intMap
          1 -> intMapFind (position + 1) intMap
          2 -> intMapFind (relativeBase + (intMapFind (position + 1) intMap)) intMap

  param2 = div (mod opCode_raw 10000 - mod opCode_raw 1000) 1000
  arg2 = case param2 of
          0 -> intMapFind (intMapFind (position + 2) intMap) intMap
          1 -> intMapFind (position + 2) intMap
          2 -> intMapFind (relativeBase + (intMapFind (position + 2) intMap)) intMap
  
-- opCodes with one arguments  
cICC intMap position relativeBase input output 
 | opCode == 3   = if input == []
                    then (intMap, "Halt", opCode, position, relativeBase, input, output)
                    else cICC (Map.insert arg1 (head input) intMap) (position + 2) relativeBase (tail input) output
 | opCode == 4   = cICC intMap (position + 2) relativeBase input (arg1:output)
 | opCode == 9   = cICC intMap (position + 2) (relativeBase + arg1)  input output
 where
  opCode_raw = intMapFind position intMap
  opCode = mod opCode_raw 100
    
  param1 = div (mod opCode_raw 1000 - mod opCode_raw 100) 100
  arg1 = if (opCode /= 3)
          then case param1 of
           0 -> intMapFind (intMapFind (position + 1) intMap) intMap
           1 -> intMapFind (position + 1) intMap
           2 -> intMapFind (relativeBase + (intMapFind (position + 1) intMap)) intMap
          -- <bullshit> 
          else case param1 of
           0 -> intMapFind (position + 1) intMap
           2 -> relativeBase + (intMapFind (position + 1) intMap)
          -- </bullshit>
         

-- opCodes with no arguments
cICC intMap position relativeBase input output
 | opCode == 99  = (intMap, "Done", opCode, position, relativeBase, input, output)
 | otherwise     = (intMap, "Error", opCode, position, relativeBase, input, output)
 where
  opCode_raw = intMapFind position intMap
  opCode = mod opCode_raw 100
  
intMapFind position intMap = Map.findWithDefault 0 position intMap
  
-- Day 09b

day09b = cICC_output (cICC_init data_day09a) 0 0 [2]

-- Day 10a

day10a = (day10a_pos,day10a_max)

day10a_pos = head [(x,y) | x <- [0..day10a_xmax], y <- [0..day10a_ymax], (day10a_output ! (x,y)) == day10a_max ] 

day10a_max = maximum day10a_output

day10a_output = array ((0,0),(day10a_xmax,day10a_ymax)) [((x,y), countAsteroids x y) | x <- [0..day10a_xmax], y <- [0..day10a_ymax]]
 where
  input = data_day10a
  
countAsteroids x1 y1 =
 if input ! (x1,y1)
  then
   length [asteroid | asteroid <- [checkAsteroid x1 y1 x2 y2 | x2 <- [0..day10a_xmax], y2 <- [0..day10a_ymax], not ((x1 == x2) && (y1 == y2))] , asteroid]
  else
   0
 where
  input = data_day10a

checkAsteroid x1 y1 x2 y2 =
 if input ! (x2,y2)
  then
   checkLineOfSight x1 y1 (x2 + stepx) (y2 + stepy) stepx stepy
  else
   False 
 where
  input = data_day10a
  steps_ggt = ggt (x1-x2) (y1-y2)
  stepx = div (x1-x2) steps_ggt
  stepy = div (y1-y2) steps_ggt
  
checkLineOfSight x1 y1 x2 y2 stepx stepy
 | (x1 == x2) && (y1 == y2) = True
 | input ! (x2,y2)          = False
 | otherwise                = checkLineOfSight x1 y1 (x2 + stepx) (y2 + stepy) stepx stepy
 
 where
  input = data_day10a

ggt c d = maximum [n | n <- [1..(max a b)], (mod a n) == 0, (mod b n) == 0]
 where
  a = abs c
  b = abs d

data_day10a = array ((0,0),(day10a_xmax,day10a_ymax)) [((x,y),(list !! y) !! x ) | x <- [0..day10a_xmax], y <- [0..day10a_ymax]]
 where
  list = data_day10a_bool
  
day10a_xmax = (length (head data_day10a_bool)) - 1
day10a_ymax = (length data_day10a_bool) - 1

data_day10a_bool = map (foldr (\x acc -> if x == '#' then True:acc else False:acc) []) (splitOn ';' data_day10a_raw)

-- Day 10b

day10b = head (drop 199 (reverse day10b_output))

day10b_output = day10b_calc data_day10b []

day10b_calc input acc
 | new_acc == acc = acc
 | otherwise      = day10b_calc (fmap (\x -> case x of _:a -> a; [] -> []) input) new_acc
 where
  new_acc = (Map.foldl (\acc x -> case x of a:b -> a:acc; [] -> acc) acc input)

data_day10b = fmap (sortBy data_day10b_sort {-(\(a,b) (c,d) -> compare ((a*a) + (b * b)) ((c * c) + (d * d)))-}) data_day10b_unsorted

data_day10b_sort p q = compare ((a*a) + (b * b)) ((c * c) + (d * d))
 where
  (a,b) = vectorSub p day10a_pos
  (c,d) = vectorSub q day10a_pos

data_day10b_unsorted = Map.fromListWith (\[a] b -> a:b) [((angleFromVector (vectorSub (x,y) day10a_pos)),[(x,y)]) | x <- [0..day10a_xmax], y <- [0..day10a_ymax], not (((fst day10a_pos) == x) && ((snd day10a_pos) == y)), data_day10a ! (x,y)]


vectorSub (x1,y1) (x2,y2) = (x1 - x2,y1 - y2)

-- (0,-1) equals 0, (1,0) equals pi/2
angleFromVector (a,b)
 | y == 0 && x > 0 = pi / 2
 | y == 0 && x < 0 = 3 / 2 * pi
 | y < 0           = atan (x/(abs y)) + if x < 0 then 2 * pi else 0
 | y > 0           = pi - atan (x/(abs y)) -- - if x < 0 then 2 * pi else 0
 where
  x = fromIntegral a
  y = fromIntegral b
  
-- Day 11a

day11a = length (Map.keys (fst day11a_output))

day11a_output = day11a_robot (cICC_init data_day11a) 0 0 [0] data_day11a_map (0,0) 0


-- day11a_robot :: [Int] -> Int -> Int -> [Int] -> Map.Map (Int, Int) Int -> (Int, Int) -> Int -> (Map.Map (Int, Int) Int, ([Int], [Char], Int, Int, Int, [Int], [Int]))
day11a_robot memoryOld positionOld relativeBaseOld input mapOld robotPositionOld@(x,y) robotFacingOld
 -- if halt then do next step
 | opCode == 3  = day11a_robot memory position relativeBase [mapColor] map robotPosition robotFacing                 
 -- if done ore error then end
 | opCode == 99 = (map, output_raw)
 | otherwise    = (map, output_raw)

 where
  -- do step
  output_raw = cICC_output_raw memoryOld positionOld relativeBaseOld input
  memory = (\(a,b,c,d,e,f,g) -> a) output_raw
  opCode = (\(a,b,c,d,e,f,g) -> c) output_raw
  position = (\(a,b,c,d,e,f,g) -> d) output_raw
  relativeBase = (\(a,b,c,d,e,f,g) -> e) output_raw
  output = (\(a,b,c,d,e,f,g) -> g) output_raw
  
  -- don't swap these -.-'
  paintColor = last output
  rotate = head output
  
  -- put color from output into map
  mapColorOld = day11a_getColor mapOld robotPositionOld
  map = Map.insert robotPositionOld paintColor mapOld
         
  -- calculate robot movement
  robotFacing = mod (robotFacingOld + 3 + 2 * rotate) 4
  robotPosition = case robotFacing of
                   0 -> (x,y-1)
                   1 -> (x+1,y)
                   2 -> (x,y+1)
                   3 -> (x-1,y)         
  -- get color from camera
  mapColor = day11a_getColor map robotPosition
  
--data_day11a_map :: Map.Map (Int, Int) Int
data_day11a_map = Map.empty

day11a_getColor :: Map.Map (Int, Int) Int -> (Int,Int) -> Int
day11a_getColor map p = case color of
                         Just a  -> a
                         Nothing -> 0
 where color = Map.lookup p map

-- Day 11b

day11b = spoolList (xmax - xmin + 1) [(day11a_getColor day11b_output (x,y)) | y <- [ymin..ymax], x <- [xmin..xmax]]
 where
  keys = Map.keys day11b_output
  keys_x = map (fst) keys
  keys_y = map (snd) keys
  xmin = minimum keys_x
  xmax = maximum keys_x
  ymin = minimum keys_y
  ymax = maximum keys_y
 
day11b_output = fst (day11a_robot (cICC_init data_day11a) 0 0 [1] data_day11a_map (0,0) 0)

-- Day 12a

day12a = calcTotalEnergy (last (take 1001 (day12a_calc day12a_vec day12a_vel)))

calcTotalEnergy (vectors,velocities) = sum (zipWith(*) (map (\(a,b,c) -> (abs a) + (abs b) + (abs c)) vectors) (map (\(a,b,c) -> (abs a) + (abs b) + (abs c)) velocities))

day12a_calc vectors velocities = [(vectors, velocities)] ++ (day12a_calc vectors_new velocities_new)
 where
  velocities_new = zipWith (vectorAdd3) velocities (getAccelerations vectors)
  vectors_new = zipWith (vectorAdd3) vectors velocities_new
  
getAccelerations vectors = foldr (\vec1 acc -> (foldl1 (\acc x -> vectorAdd3 acc x) [calcAcceleration vec1 vec2 | vec2 <- vectors, vec1 /= vec2]):acc) [] vectors

calcAcceleration (x,y,z) (r,s,t) = (a,b,c)
 where
  a = compareToTrinary r x
  b = compareToTrinary s y
  c = compareToTrinary t z
  
compareToTrinary a b = case (compare a b) of
                        GT -> 1
                        EQ -> 0
                        LT -> (-1)

day12a_vel = replicate 4 (0,0,0)

day12a_vec = data_day12a

vectorAdd3 (x1,y1,z1) (x2,y2,z2) = (x1 + x2, y1 + y2, z1 + z2)

-- Day 12b

-- for solution divide product of the 3 periods by common prime factors

day12b = (x_period,y_period,z_period)
 where
  x_period = length (takeWhile (compareCoordinate 0 (day12a_vec,day12a_vel)) (tail (day12a_calc day12a_vec day12a_vel))) + 1
  y_period = length (takeWhile (compareCoordinate 1 (day12a_vec,day12a_vel)) (tail (day12a_calc day12a_vec day12a_vel))) + 1
  z_period = length (takeWhile (compareCoordinate 2 (day12a_vec,day12a_vel)) (tail (day12a_calc day12a_vec day12a_vel))) + 1


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

compareCoordinate :: Int -> ([(Int,Int,Int)],[(Int,Int,Int)]) -> ([(Int,Int,Int)],[(Int,Int,Int)]) -> Bool
compareCoordinate coord ([a,b,c,d],[e,f,g,h]) ([i,j,k,l],[m,n,o,p])
 = not (((getCoordinate coord a) == (getCoordinate coord i)) &&
        ((getCoordinate coord b) == (getCoordinate coord j)) &&
        ((getCoordinate coord c) == (getCoordinate coord k)) &&
        ((getCoordinate coord d) == (getCoordinate coord l)) &&
        ((getCoordinate coord e) == (getCoordinate coord m)) &&
        ((getCoordinate coord f) == (getCoordinate coord n)) &&
        ((getCoordinate coord g) == (getCoordinate coord o)) &&
        ((getCoordinate coord h) == (getCoordinate coord p)))

getCoordinate :: Int -> (Int,Int,Int) -> Int
getCoordinate coord (x,y,z) = case coord of
                               0 -> x
                               1 -> y
                               2 -> z
                       

-- the idiots way
-- day12b_nope = length (takeWhile ((day12a_vec,day12a_vel) /=) (tail (day12a_calc day12a_vec day12a_vel))) + 1

day12b_test = length (takeWhile ((data_day12b_test,day12a_vel) /=) (tail (day12a_calc data_day12b_test day12a_vel))) + 1

data_day12b_test :: [(Int,Int,Int)]
data_day12b_test = [(-1, 0, 2),(2,-10,-7),(4,-8,8),(3,5,-1)]

-- Day 13a

day13a = length (Map.keys (Map.filter (== 2) map))
 where
  map = Map.fromList (foldr (\[a,b,c] acc -> ((a,b),c):acc) [] day13a_output)

day13a_output = spoolList 3 (reverse ((\(a,b,c,d,e,f,g) -> g) day13a_calc))

day13a_calc = cICC_output_raw (cICC_init data_day13a) 0 0 []

-- Day 13b

day13b =
 do
  io <- sequence (map (day13b_print) map_list)
  return ()
 where
  map_list = day13b_output_auto (cICC_init data_day13b) []
  
day13b_print map =
 do
  io <- putStrLn (concat [[day13b_intToChar (map Map.! (x,y)) | x <- [0..xmax]] ++ "\n" | y <- [0..ymax]])
  print (map Map.! (-1,0))
 where
  xmax = 42
  ymax = 22

day13bOld input0 =
 do
  io <- putStrLn (concat [[day13b_intToChar (map Map.! (x,y)) | x <- [0..xmax]] ++ "\n" | y <- [0..ymax]])
  print (map Map.! (-1,0))
 where
  map = Map.fromList (foldr (\[a,b,c] acc -> ((a,b),c):acc) [] (day13b_output input))
  xmax = 42
  ymax = 22
  input = input1 ++ input0
  
input1=[]

day13b_intToChar int
 | int == 0 = ' '
 | int == 1 = '#'
 | int == 2 = 'x'
 | int == 3 = '='
 | int == 4 = 'o'

day13b_output_auto intMap input
 | block == Nothing = [map]
 | opCode == 3      = case compare ball_x paddle_x of
                       GT -> map:day13b_output_auto intMap_new [1]
                       EQ -> map:day13b_output_auto intMap_new [0]
                       LT -> map:day13b_output_auto intMap_new [-1]
 | opCode == 99     = [map]
 where
  calc_output = day13b_calc_auto intMap input
  output = spoolList 3 (reverse ((\(a,b,c,d,e,f,g) -> g)calc_output))
  map = Map.fromList (foldr (\[a,b,c] acc -> ((a,b),c):acc) []output)
  inverseMap = Map.fromList (foldr (\[a,b,c] acc -> (c,(a,b)):acc) [] output)
  intMap_new = (\(a,b,c,d,e,f,g) -> a) calc_output
  opCode = (\(a,b,c,d,e,f,g) -> c) calc_output
    
  block = Map.lookup 1 inverseMap  
  ball = inverseMap Map.! 4
  ball_x = (\(a,b) -> a) ball
  paddle = inverseMap Map.! 3
  paddle_x = (\(a,b) -> a) paddle

day13b_output input = spoolList 3 (reverse ((\(a,b,c,d,e,f,g) -> g) (day13b_calc input)))

day13b_calc_auto intMap input = cICC_output_raw intMap 0 0 input

day13b_calc input = cICC_output_raw (cICC_init data_day13b) 0 0 input

data_day13b = 2:(tail data_day13a)

-- Day 14a

day14a = totalOre - reducedSurplusOre
 where
  output = day14a_getOre "FUEL"
  totalOre = (\(a,b,c) -> b) output
  surplus = Map.fromListWith (\a b -> a + b) (map (\(a,b) -> (b,a)) ((\(a,b,c) -> c) output))
  moleculeList = Map.keys surplus
  
  reducedSurplusOre = reduceSurplus moleculeList surplus

reduceSurplus :: [String] -> Map.Map String Int -> Int
reduceSurplus moleculeList surplus
 | surplus == next_surplus = Map.findWithDefault 0 "ORE" surplus
 | otherwise               = reduceSurplus (Map.keys next_surplus) next_surplus
 where
  next_surplus = reduceSurplusOnce moleculeList surplus

reduceSurplusOnce [] surplus = surplus
reduceSurplusOnce molecules@("ORE":nextMolecules) surplus =
 reduceSurplusOnce nextMolecules surplus
reduceSurplusOnce molecules@(currentMolecule:nextMolecules) surplus =
 reduceSurplusOnce nextMolecules nextSurplus
 where
  reaction = data_day14a Map.! currentMolecule
  product = (\(a,b) -> b) reaction
  educts = (\(a,b) -> a) reaction
  
  productSurplus = surplus Map.! currentMolecule
  productMultiplier = (\(a,b) -> a) product
  maxReactionMultiplier = div productSurplus productMultiplier
  
  -- remove product from surplus then add educts to surplus
  prodSurplus = day14a_mapSum currentMolecule (-1 * maxReactionMultiplier * productMultiplier) surplus
  nextSurplus = foldl (\acc (a,b) -> day14a_mapSum b (a * maxReactionMultiplier) acc) prodSurplus educts 
  
-- add or substract from value in map
day14a_mapSum :: String -> Int -> Map.Map String Int -> Map.Map String Int
day14a_mapSum key value map = Map.insert key (previous + value) map
 where
  previous = Map.findWithDefault 0 key map

day14a_getOre "ORE" = (1,1,[])

day14a_getOre product = (\a (b,c) -> (a,b,c)) productMultiplier (foldl (foldOre) (0,[]) eductList)
 where
  reaction = data_day14a Map.! product
  eductList = fst reaction
  productMultiplier = fst (snd reaction)

-- foldOre :: Int -> (Int,String) -> Int
foldOre (oreAcc,surplusAcc) (eductNeeded,educt) =
 ((oreAcc + reactionMultiplier * eductOre),nextSurplus)
 where
  eductOreReaction = day14a_getOre educt
  eductFromReaction = (\(a,b,c) -> a) eductOreReaction
  eductOre = (\(a,b,c) -> b) eductOreReaction
  eductSurplus = (\(a,b,c) -> c) eductOreReaction
  reactionMultiplier = divCeiling eductNeeded eductFromReaction
  reactionSurplus = (reactionMultiplier * eductFromReaction) - eductNeeded
  eductSurplusMultiplied = map (\(a,b) -> ((a * reactionMultiplier),b)) eductSurplus
  nextSurplus = case reactionSurplus of
                 0         -> eductSurplusMultiplied ++ surplusAcc
                 otherwise -> (reactionSurplus,educt):eductSurplusMultiplied ++ surplusAcc

divCeiling a b = ceiling (c / d)
 where
  c = fromIntegral a
  d = fromIntegral b

data_day14a = Map.fromList reactionsWithProductKey
 where
  lines = splitOn ';' data_day14a_raw -- [String]
  eductsProduct = map (splitOn '=') lines -- [[String]]
  eductsList = foldr (\x acc -> (head x):acc) [] eductsProduct -- [String]
  eductListList = map (splitOn ',') eductsList --[[String]]
  eductListListTuple = map (map ((\(a:b:[]) -> ((read a :: Int),b)) . splitOn ' ')) eductListList
  productList = foldr (\x acc -> (last x):acc) [] eductsProduct -- [String]
  productListTuple = map ((\(a:b:[]) -> ((read a :: Int),b)) . splitOn ' ') productList -- [(Int,String)]
  reactions = zip eductListListTuple productListTuple
  reactionsWithProductKey = zip (map (snd) productListTuple) reactions
  
-- Day 14b

day14b = last (takeWhile (\(a,b) -> a < 1000000000000) [day14b_calc n | n <- [3060000..]])

day14b_calc multiplier = (totalOre - reducedSurplusOre,multiplier) --(totalOre,reducedSurplusOre,(totalOre - reducedSurplusOre))
 where
  output = day14a_getOre "FUEL"
  totalOre = ((\(a,b,c) -> b) output) * multiplier
  surplus = Map.fromListWith (\a b -> a + b) (map (\(a,b) -> (b,a * multiplier)) ((\(a,b,c) -> c) output))
  moleculeList = Map.keys surplus
  reducedSurplusOre = reduceSurplus moleculeList surplus
  
-- Day 15a

day15a = (day15b_output start (Map.singleton start 0) 0) Map.! (0,0)
 where
  start = Map.fromList (map (\(a,b) -> (b,a)) (Map.toList day15a_calc)) Map.! 2

day15a_map =
 do
  io <- day15a_showMap day15a_calc
  return ()
 
day15a_showMap mapData =
 do 
  io <- mapM (putStrLn) (day15a_convertMap mapData)
  return ()

day15a_convertMap :: Map.Map (Int,Int) Int -> [String]
day15a_convertMap mapData = map (map (day15a_intToChar)) [[Map.findWithDefault 3 (x,y) mapDataNew | x <- [xmin..xmax]] | y <- [ymin..ymax]]
 where
  keys = Map.keys mapData
  xmin = minimum (map (\(a,b) -> a) keys)
  xmax = maximum (map (\(a,b) -> a) keys)
  ymin = minimum (map (\(a,b) -> b) keys)
  ymax = maximum (map (\(a,b) -> b) keys)
  mapDataNew = Map.insert (0,0) 4 mapData

day15a_intToChar :: Int -> Char  
day15a_intToChar n
 | n == 0 = '#'
 | n == 1 = '.'
 | n == 2 = 'o'
 | n == 3 = ' '
 | n == 4 = 'x'
 
day15a_calc = day15a_algorithm1 (cICC_init data_day15a) 0 0 [1] 0 (Map.fromList [((0,0),1)]) (0,0) 1 (0,0)

day15a_algorithm1 memoryOld positionOld relativeBaseOld input phase mapDataOld robotPositionOld@(x,y) robotHeading firstWallOld
 -- | length (Map.keys mapData) > 1000 = mapData
 | phase == 0 = if firstWall==(0,0)
                 then -- continue to robotHeading
                  day15a_algorithm1 memory position relativeBase [robotHeading] phase mapData robotPosition robotHeading firstWall
                 else
                  if hitWall
                   then -- turn right
                    day15a_algorithm1 memory position relativeBase [turnRight] phase mapData robotPosition turnRight firstWall
                   else -- turn left
                    day15a_algorithm1 memory position relativeBase [turnLeft] phase mapData robotPosition turnLeft firstWall
 | phase == 1 = mapData
 | otherwise  = mapData

 -- day15a_algorithm1 memory position relativeBase [direction] phase map robotPosition robotHeading -- lastWall

 where
  -- do step
  output_raw = cICC_output_raw memoryOld positionOld relativeBaseOld input
  memory = (\(a,b,c,d,e,f,g) -> a) output_raw
  opCode = (\(a,b,c,d,e,f,g) -> c) output_raw
  position = (\(a,b,c,d,e,f,g) -> d) output_raw
  relativeBase = (\(a,b,c,d,e,f,g) -> e) output_raw
  output = head ((\(a,b,c,d,e,f,g) -> g) output_raw)
  robotPosition_next = case robotHeading of
                        1 -> (x,y-1)
                        2 -> (x,y+1)
                        3 -> (x-1,y)
                        4 -> (x+1,y)
  mapData = Map.insert (robotPosition_next) output mapDataOld
    
  robotPosition = if output == 0
                   then robotPositionOld
                   else robotPosition_next
                   
  hitWall = if output == 0 then True else False
  circledMap = if (mapData == mapDataOld && robotPosition_next == firstWallOld && robotHeading == 1) then True else False
  
  firstWall = if hitWall && firstWallOld == (0,0) then robotPosition_next else firstWallOld
  
  turnRight = case robotHeading of
               1 -> 4
               2 -> 3
               3 -> 1
               4 -> 2
  
  turnLeft  = case robotHeading of
               4 -> 1
               3 -> 2
               1 -> 3
               2 -> 4
  
  -- choose algorithm phase 
  phase = if circledMap then 1 else 0
  -- ... what a joke, eh?

-- Day 15b
day15b = maximum (Map.elems (day15b_output start (Map.singleton start 0) 0))
 where
  start = Map.fromList (map (\(a,b) -> (b,a)) (Map.toList day15a_calc)) Map.! 2

day15b_output (x,y) distanceMap n = westMap

 where
  m = n + 1
  mapData = day15a_calc
  northTile = Map.findWithDefault (-1) (x,y-1) mapData
  northVoid = Map.lookup (x,y-1) distanceMap == Nothing
  
  northMap = if northTile > 0 && northVoid
              then day15b_output (x,y-1) (Map.insert (x,y-1) m distanceMap) m
              else distanceMap
              
  eastTile = Map.findWithDefault (-1) (x+1,y) day15a_calc
  eastVoid = Map.lookup (x+1,y) distanceMap == Nothing
  
  eastMap = if eastTile > 0 && eastVoid
              then day15b_output (x+1,y) (Map.insert (x+1,y) m northMap) m
              else northMap
  
  southTile = Map.findWithDefault (-1) (x,y+1) day15a_calc
  southVoid =  Map.lookup (x,y+1) distanceMap == Nothing
  
  southMap = if southTile > 0 && southVoid
              then day15b_output (x,y+1) (Map.insert (x,y+1) m eastMap) m
              else eastMap
  
  westTile = Map.findWithDefault (-1) (x-1,y) day15a_calc
  westVoid = Map.lookup (x-1,y) distanceMap == Nothing
  
  westMap = if westTile > 0 && westVoid
              then day15b_output (x-1,y) (Map.insert (x-1,y) m southMap) m
              else southMap
              
-- Day 16a

day16a = intListToInt (take 8 day16a_output)

day16a_output = day16a_calc data_day16a 100

day16a_calc list 0 = list
day16a_calc list n = day16a_calc (day16a_step list) (n-1)
  
day16a_step [] = []
day16a_step list@(_:shortList) = (mod10abs (day16a_line n (spoolList (4*n) list))):(day16a_step shortList)
 where
  n = day16a_length - (length list) + 1

day16a_line :: Int -> [[Int]] -> Int
day16a_line n listList = foldl' (\acc list -> acc + day16a_sum (spoolList n list)) 0 listList

day16a_sum (a:_:c:_) = sum a - sum c
day16a_sum (a:_) = sum a

day16a_length = length data_day16a

mod10abs n = mod (abs n) 10

intListToInt list = read (concat . map show $ list) :: Int

-- Day 16b

day16b = intListToInt (take 8 day16b_output)

day16b_output = day16b_calc data_day16b 100

day16b_calc list 0 = list
day16b_calc list n = day16b_calc (day16b_step list) (n-1)

day16b_step [n] = [n]
day16b_step (n:shortList) = (mod (n + acc) 10):(newList)
 where
  newList= day16b_step shortList
  acc = head newList


data_day16b = drop offsetDrop (concat (replicate replication data_day16a))
 where
  listLength = day16a_length * day16b_multiplicator - day16b_offset
  replication = divCeiling listLength day16a_length
  offsetDrop = replication * day16a_length - listLength
  


day16b_length = (day16a_length * day16b_multiplicator) - day16b_offset

day16b_offset = intListToInt (take 7 data_day16a)

day16b_multiplicator = 10000

-- main = print (day16b_calc data_day16b 1) -- (intListToInt (take 8 day16b_output))

-- Day 17a

day17a = foldl (\acc (x,y) -> acc + x * y) 0 intersections
 where
 intersections = [(x,y) |
                  y <- [0..(day17a_yLength - 1)],
                  x <- [0..(day17a_xLength - 1)],
                  Map.findWithDefault ' ' (x,y) day17a_map == '#',
                  Map.findWithDefault ' ' (x+1,y) day17a_map == '#',
                  Map.findWithDefault ' ' (x-1,y) day17a_map == '#',
                  Map.findWithDefault ' ' (x,y+1) day17a_map == '#',
                  Map.findWithDefault ' ' (x,y-1) day17a_map == '#']

day17a_map = Map.fromList day17a_assocList

day17a_assocList = zip [(x,y) | y <- [0..(yLength - 1)], x <- [0..(xLength - 1)]] (concat field)
 where
  xLength = day17a_xLength
  yLength = day17a_xLength
  field = day17a_field

day17a_xLength = length (head day17a_field)
day17a_yLength = length day17a_field 
day17a_field = lines day17a_output

day17a_showMap =
 do
  io <- putStr day17a_output
  return ()

day17a_output = map (chr) output
 where
  output = reverse ((\(a,b,c,d,e,f,g) -> g) day17a_calc)

day17a_calc = cICC_output_raw (cICC_init data_day17a) 0 0 []

-- Day 17b

day17b = head day17b_output

day17b_output = output
 where
  output = ((\(a,b,c,d,e,f,g) -> g) day17b_calc)

day17b_calc = cICC_output_raw (cICC_init data_day17b) 0 0 day17b_solution

data_day17b = 2:(tail data_day17a)

day17b_solution = map (ord) "A,A,B,C,B,C,B,C,A,C\nR,6,L,8,R,8\nR,4,R,6,R,6,R,4,R,4\nL,8,R,6,L,10,L,10\nn\n"

-- Day 18a

--   Path = Place Type Position  [(next Place, distance)])
data Path = Place Char (Int,Int) [(Path, Int)] deriving (Show, Eq)

-- LabBranch char pos directContent totalContent [branchList]
-- LabStem char pos directContent totalContent [branchList]

data LabTree =
 LabStem Char (Int,Int) String String [LabTree] |
 LabBranch Char (Int,Int) String String [LabTree]
 deriving (Show, Eq)

day18a = day18a_output

day18a_output = day18a_queuingAlgorithm day18a_distanceFunction day18a_hashFunction Map.empty Map.empty

day18a_queuingAlgorithm distanceFunction hashFunction itemMap priorityMap
  | length keys == length day18a_keys =
  (lowestItem)
 | otherwise =
  day18a_queuingAlgorithm distanceFunction hashFunction nextItemMap nextPriorityMap
 where
  lowestPriorityKey :: Int
  lowestPriorityKey
   | priorityMap == Map.empty = 0
   | otherwise                = head $ Map.keys priorityMap
 
  lowestPriority
   | priorityMap == Map.empty = [""]
   | otherwise                = priorityMap Map.! lowestPriorityKey
   
  lowestPriorityHead = head lowestPriority
  lowestPriorityTail = tail lowestPriority
  
  lowestItem
   | lowestPriority == [""] =
    (0,"")
   | otherwise              =
    Map.findWithDefault (maxBound,"") lowestPriorityHead itemMap 
  
  distance = fst lowestItem
  keys = snd lowestItem
  
  possibleKeys
   | distance == maxBound = ""
   | otherwise            = day18a_possibleKeys keys
  
  nextItems :: [(Int,String)]
  nextItems =
   map
    (\x -> (distance + (distanceFunction keys x),x:keys))
    possibleKeys
  
  nextHashItemAssocs =
   map
    (\x@(a,b) -> (hashFunction b,x))
    nextItems
  
  itemKeys = Map.keys itemMap  
  
  insertFlag =
   map
    (\(a,(b,c)) ->
     if
      notElem a itemKeys ||
      (\(d,e) -> d > b) (itemMap Map.! a)
      then True
      else False)
    nextHashItemAssocs
  
  nextItemMap =
   foldl
    (\acc (a,(b,c)) -> if a then Map.insert b c acc else acc)
    (Map.delete lowestPriorityHead itemMap) $
    zip insertFlag nextHashItemAssocs    
  
  priorityMapCleared
   | lowestPriorityTail == [] =
    Map.deleteMin priorityMap
   | otherwise                =
    Map.insert lowestPriorityKey lowestPriorityTail priorityMap
  
  nextPriorityMap =
    foldl
     (\acc (a,(b,(c,d))) ->
      if not a
       then acc
       else Map.insertWith (\[x] y -> x:y) c [b] acc)
     priorityMapCleared $
     zip insertFlag nextHashItemAssocs 
 
day18a_distanceFunction keys nextKey =
 day18a_shortestPathMapLength Map.! (pos1,pos2)
 where
  pos1 | keys == "" = day18a_start
       | otherwise  = day18a_charToPos $ head keys
  pos2 = day18a_charToPos nextKey
  
day18a_hashFunction keys = (head keys):';':(sort keys)
  
day18a_possibleKeys keys = filteredKeys
 where
  attainableKeys =
   filter
    (\x -> all (`elem` keys)
     (map (toLower) $ day18a_locksOnKey Map.! x))
    (day18a_keys \\ (keys))
     
  containedKeys =
   concat $ 
    foldr foldFunction [] attainableKeys
     
  filteredKeys = attainableKeys \\ containedKeys
  
  foldFunction x acc = y:acc
   where
    pos = day18a_charToPos x
    branch = fst $ day18a_labTreeMap Map.! pos
    total = (\(LabBranch a b c d e) -> d) branch
    y = filter isLower total

day18a_stringToPath [char] = ([day18a_charToPos char],0)
day18a_stringToPath string =
 day18a_concatPaths
  (map (day18a_shortestPathMapFull Map.!)
   ((\a -> zip a (tail a)) (map day18a_charToPos string)))

day18a_charToPos char = data_day18a_inverse Map.! char

day18a_shortestPathMapLength = fmap (\(a,b) -> b) day18a_shortestPathMapFull

day18a_shortestPathMapPath = fmap (\(a,b) -> a) day18a_shortestPathMapFull

day18a_shortestPathMapFull = Map.fromList [(\(a,b) -> ((x,y),(a,b))) (day18a_shortestPath x y) | x <- allPos, y <- allPos{-, x /= y-}]
 where
  keyPos = day18a_start:(map (day18a_charToPos) day18a_keys)
  allPos = Map.keys day18a_connectionMap

day18a_locksOnKey = Map.fromList (zip day18a_keys (map getLocksBefore (map (day18a_charToPos) day18a_keys)))
 where
  getLocksBefore (-1,-1) = []
  getLocksBefore pos =
   if isUpper char
    then char:list
    else list
   where
    mapElem = day18a_labTreeMap Map.! pos
    tree = fst mapElem
    parent = snd mapElem
    char = (\(LabBranch a b c d e) -> a) tree
    list = getLocksBefore parent

day18a_labTreeMap = Map.unions (map (getLabTreeMap (-1,-1)) day18a_labTree)
 where  
  getLabTreeMap parent x@(LabBranch a b c d e) = Map.insert b (x,parent) treeMap
   where
    treeMap = Map.unions (map (getLabTreeMap b) e)
    
-- map of stem pos of keys    

day18a_keyToStem = Map.fromList $ zip day18a_keys $ map getStemPos day18a_keys
 where
  getStemPos char = stemPos
   where
    pos = day18a_charToPos char
    branch = head $ filter (\x -> elem pos $ snd x) day18a_branchPosList
    stemPos = (\(LabBranch a b c d e) -> b) $ fst branch

day18a_getStemPos pos = stemPos
   where
    branch = head $ filter (\x -> elem pos $ snd x) day18a_branchPosList
    stemPos = (\(LabBranch a b c d e) -> b) $ fst branch

-- list of branches and contained pos
day18a_branchPosList = zip day18a_labTree posLists
 where
  posLists = map gatherPos day18a_labTree
  
  gatherPos (LabBranch a b c d []) = [b]
  gatherPos (LabBranch a b c d e) =
   b:(concat (map gatherPos e))

day18a_labTree = map stemToTree (day18a_buildLabTree day18a_traverse)
 where
  stemToTree (LabStem a b c d e) = LabBranch a b c d e
  stemToTree x = x

day18a_buildLabTree :: Path -> [LabTree]
day18a_buildLabTree (Place char pos placeList)
 | elem pos (day18a_fieldPos \\ day18a_stemPos) = -- field
  stems
 | placeList == [] && elem pos day18a_stemPos = -- artifact of circlePos
  stems
 | elem pos day18a_stemPos = -- stem
  (LabStem char pos direct total branches):stems
 | placeList == [] = -- leaf
  (LabBranch char pos [] [] []):stems
 | otherwise = -- branch
  (LabBranch char pos direct total branches):stems
 where
  tree = concat (map (\a -> day18a_buildLabTree (fst a)) placeList)
  stems = [x | x@(LabStem a b c d e) <- tree]
  branches = tree \\ stems
  direct = sort (filter (/='.') (map (\(LabBranch a b c d e) -> a) branches))
  total = sort (concat (map (\(LabBranch a b c d e) -> d) branches) ++ direct)

day18a_concatPaths :: [([(Int,Int)],Int)] -> ([(Int,Int)],Int)
day18a_concatPaths pathList = (path,distance)
 where
  paths = map fst pathList
  path = (head paths) ++ concat (map (tail) (tail paths))
  distance = sum (map snd pathList)

day18a_shortestPath :: (Int,Int) -> (Int,Int) -> ([(Int,Int)],Int)
day18a_shortestPath pos1 pos2
 | sameBranch =
  day18a_shortestPathSameBranch pos1 pos2
 | bothField =
  day18a_shortestPathField pos1 pos2
 | noField    =
  day18a_concatPaths [path1,(day18a_shortestPathField root1 root2),path2] -- branch1 + field + branch2
  | pos1Field =
  day18a_concatPaths [(day18a_shortestPathField pos1 root2),path2] -- field + branch2
 | pos2Field =
  day18a_concatPaths [path1,(day18a_shortestPathField root1 pos2)] -- branch1 + field
 | otherwise  =
  ([(0,0)],0)
  where
   fieldPos = day18a_fieldPos
   
   pos1Field = (elem pos1 fieldPos)
   pos2Field = (elem pos2 fieldPos)
   bothField = pos1Field && pos2Field
   noField = not (pos1Field || pos2Field)
   
   pos1BranchList = filter (\(a,b) -> elem pos1 b) day18a_branchPosList
   pos1Branch = fst (head pos1BranchList)
   pos2BranchList = filter (\(a,b) -> elem pos2 b) day18a_branchPosList
   pos2Branch = fst (head pos2BranchList)
   sameBranch = (pos1BranchList == pos2BranchList) && pos1BranchList /= []
   
   commList1 = day18a_branchPath pos1
   commList2 = day18a_branchPath pos2
   
   root1 = fst (last commList1)
   root2 = fst (last commList2)
   
   path1 = foldr (\(a,b) (c,d) -> (a:c,b+d)) ([],0) commList1
   path2 = foldl (\(c,d) (a,b) -> (a:c,b+d)) ([],0) commList2

day18a_shortestPathSameBranch pos1 pos2 = day18a_concatPaths [path1,path2]
 where
  branchPath1full = day18a_branchPath pos1
  branchPath2full = day18a_branchPath pos2
  branchPath1dif = difFst branchPath1full branchPath2full
  branchPath2dif = difFst branchPath2full branchPath1full
  branchPath1cut = takeWhileSameFstPlus branchPath1full branchPath1dif
  branchPath2cut = takeWhileSameFstPlus branchPath2full branchPath2dif
  path1 = foldr (\(a,b) (c,d) -> (a:c,b+d)) ([],0) branchPath1cut
  path2 = foldl (\(c,d) (a,b) -> (a:c,b+d)) ([],0) branchPath2cut

difFst x [] = x
difFst [] _ = []
difFst list1 list2@(item2:rest2) =
 difFst (deleteItem list1 item2) rest2
 where
  deleteItem [] _ = []
  deleteItem list@(item:rest) delete =
   if fst item == fst delete
    then rest
    else item:(deleteItem rest delete)
    
takeWhileSameFstPlus [] _ = []
takeWhileSameFstPlus (x:xs) [] = [x]
takeWhileSameFstPlus (x:xs) (y:ys) =
 if fst x == fst y
  then x:(takeWhileSameFstPlus xs ys)
  else [x]

day18a_branchPath pos = getBranchPath pos pos
 where
  getBranchPath (-1,-1) _ = []
  getBranchPath pos child
   | pos == child =
    (pos,0):getBranchPath parent pos
   | otherwise    =
    (pos,distance):(getBranchPath parent pos)
   where
    mapElem = day18a_labTreeMap Map.! pos 
    tree = fst mapElem
    parent = snd mapElem
    branch = (\(LabBranch a b c d e) -> e) tree    
    distance = snd (head (filter (\x -> fst x == child) (day18a_connectionMap Map.! pos)))

-- outdated but only gets field members
day18a_shortestPathField :: (Int,Int) -> (Int,Int) -> ([(Int,Int)],Int)
day18a_shortestPathField pos1 pos2
 | pos1 == pos2 = ([pos1],0)
 | otherwise = getShortestPath [([pos1],0)] [([pos2],0)]
 where
  getShortestPath :: [([(Int,Int)],Int)] -> [([(Int,Int)],Int)] -> ([(Int,Int)],Int)
  getShortestPath  pathList1 pathList2
   | pathPositionsIntersect /= [] =
    finishShortestPath pathList1 pathList2
   | snd path1head <= snd path2head =
    getShortestPath pathList1updated pathList2
   | otherwise =
    getShortestPath pathList1 pathList2updated
   where
    pathList1sorted =
     dropWhile (\(a,b) -> b < 0)
      (sortBy (\(a,b) (c,d) -> compare b d) pathList1)
    pathList2sorted =
     dropWhile (\(a,b) -> b < 0)
      (sortBy (\(a,b) (c,d) -> compare b d) pathList2)
    path1head = head pathList1sorted
    path2head = head pathList2sorted
    pos1 = head (fst path1head)
    pos2 = head (fst path2head)
    path1positions = concat (map fst pathList1)
    path2positions = concat (map fst pathList2)
    pathPositionsIntersect = intersect path1positions path2positions
       
    pos1options =
     [conn | conn <- (day18a_connectionMap Map.! pos1),
      notElem (fst conn) (fst path1head)]
    pos2options =
     [conn | conn <- (day18a_connectionMap Map.! pos2),
      notElem (fst conn) (fst path2head)]
    pathList1snubbed = delete path1head pathList1
    pathList2snubbed = delete path2head pathList2
    
    -- dead ends get -1
    pathList1updated =
     if pos1options /= []
      then
       foldr (\(a,b) acc -> 
        (a:(fst path1head),b + (snd path1head)):acc)
        [] pos1options ++ pathList1snubbed
      else
       ((\(a,b) -> (a,-1)) path1head):pathList1snubbed
    pathList2updated = 
     if pos2options /= []
      then
       foldr (\(a,b) acc ->
        (a:(fst path2head),b + (snd path2head)):acc)
        [] pos2options ++ pathList2snubbed
      else
       ((\(a,b) -> (a,-1)) path2head):pathList2snubbed
       
  finishShortestPath pathList1 pathList2 = output
   where
    pathList1sorted =
     dropWhile (\(a,b) -> b < 0)
      (sortBy (\(a,b) (c,d) -> compare b d) pathList1)
    pathList2sorted =
     dropWhile (\(a,b) -> b < 0)
      (sortBy (\(a,b) (c,d) -> compare b d) pathList2)
    path1sortedPositions = concat (map fst pathList1sorted)
    path2sortedPositions = concat (map fst pathList2sorted)
    
    shortestIntersectPos =
     head (intersect path1sortedPositions path2sortedPositions)
    
    path1intersected =
     take 1 [path | path <- pathList1sorted, elem shortestIntersectPos (fst path)]
    path2intersected =
     take 1 [path | path <- pathList2sorted, elem shortestIntersectPos (fst path)]
    
    path1minLength = snd (head path1intersected)
    path2minLength = snd (head path2intersected)
     
    pathList1long =
     dropWhile (\(a,b) -> b < path1minLength) pathList1sorted
    pathList2long =
     dropWhile (\(a,b) -> b < path2minLength) pathList2sorted
     
    pathList1short =
     takeWhile (\(a,b) -> b < path1minLength) pathList1sorted
    pathList2short =
     takeWhile (\(a,b) -> b < path2minLength) pathList2sorted
    
    pathList1notShort =
     concat (map (fillOutPath path1minLength) pathList1short)
    pathList2notShort =
     concat (map (fillOutPath path2minLength) pathList2short)
     
    pathList1updated = pathList1long ++ pathList1notShort
    pathList2updated = pathList2long ++ pathList2notShort
    
    path1updatedPositions = concat (map fst pathList1)
    path2updatedPositions = concat (map fst pathList2)
    pathUpdatedPositionsIntersect = nub (intersect path1updatedPositions path2updatedPositions)
    
    remainingPaths1 =
     foldr
      (\x acc -> head (sortBy (\(a,b) (c,d) -> compare b d)
       [y | y <- pathList1updated, elem x (fst y)]):acc) [] pathUpdatedPositionsIntersect
    remainingPaths2 =
     foldr
      (\x acc -> head (sortBy (\(a,b) (c,d) -> compare b d)
       [y | y <- pathList2updated, elem x (fst y)]):acc) [] pathUpdatedPositionsIntersect
       
    output =
     head
      (sortBy (\(a,b) (c,d) -> compare b d)
       (foldr (\((a,b),(c,d)) acc -> ((reverse a) ++ (tail c),b + d):acc) []
        (zip remainingPaths1 remainingPaths2)))
    
  fillOutPath minLength path = list
   where
    -- get options
    pathPos = fst path
    length = snd path
    pos = head pathPos
    options =
     [conn | conn <- (day18a_connectionMap Map.! pos),
      notElem (fst conn) pathPos] -- check for keys
       
    -- dead ends get -1
    newPaths = if options == []
                then
                 [(pathPos,-1)]
                else
                 foldr (\(a,b) acc -> (a:pathPos,b + length):acc) [] options
              
    -- check options for length (dead ends are long enough)
    newPathsLong = [x | x <- newPaths, (snd x) < 0 || (snd x) >= minLength]
    newPathsShort = newPaths \\ newPathsLong
    
    -- recursive longer for short
    newPathsNotShort =
     foldr (\x acc -> (fillOutPath minLength x) ++ acc) [] newPathsShort 
    
    list = newPathsNotShort ++ newPathsLong

day18a_connectionMap :: Map.Map (Int,Int) [((Int,Int),Int)]
day18a_connectionMap = connectionMap
 where
  connections = getConnections day18a_traverse
  reverse_connections = map (\(a,b,c) -> (b,a,c)) connections
  condensedList = nub (connections ++ reverse_connections)
  assocList = map (\(a,b,c) -> (a,[(b,c)])) condensedList
  connectionMap = Map.fromListWith (\a b -> a ++ b) assocList
  
  getConnections (Place a b c) = fullList
   where
    localList = foldr (\((Place d e f),g) acc -> (b,e,g):acc) [] c
    fullList = localList ++ concat (map (\(a,b) -> getConnections a) c) 
    

day18a_traverse = (\(a,b) -> a) (day18a_checkPlace start start [start])
 where
  start = day18a_start

-- checkPath -> (Path,distance,posList
-- Place char pos PlaceList@[(Place,distance)
-- PlaceListWest -> [(Path,distance)]

day18a_checkPlace :: (Int,Int) -> (Int,Int) -> [(Int,Int)] -> (Path,[(Int,Int)])
day18a_checkPlace pos@(x,y) lastPlace posList =
 ((Place char pos placeListWest),posListWest)
 where
  char = data_day18a Map.! pos
   
  northPath = day18a_checkPath (x,y-1) pos pos posList --checkNorthPath
  northDistance = (\(a,b,c) -> b) northPath
  posListNorth = (\(a,b,c) -> c) northPath
  north = (\(a,b,c) -> (a,b)) northPath
  northPos = (\(Place d e f,b,c) -> e) northPath
  
  eastPath = day18a_checkPath (x+1,y) pos pos posListNorth  --checkEast
  eastDistance = (\(a,b,c) -> b) eastPath
  posListEast = (\(a,b,c) -> c) eastPath
  east = (\(a,b,c) -> (a,b)) eastPath
  eastPos = (\((Place d e f),b,c) -> e) eastPath
  
  southPath = day18a_checkPath (x,y+1) pos pos posListEast  --checkSouth
  southDistance = (\(a,b,c) -> b) southPath
  posListSouth = (\(a,b,c) -> c) southPath
  south = (\(a,b,c) -> (a,b)) southPath
  southPos = (\((Place d e f),b,c) -> e) southPath
  
  westPath = day18a_checkPath (x-1,y) pos pos posListSouth  --checkWest
  westDistance = (\(a,b,c) -> b) westPath
  posListWest = (\(a,b,c) -> c) westPath
  west = (\(a,b,c) -> (a,b)) westPath
  westPos = (\((Place d e f),b,c) -> e) westPath
  
  placeListNorth = if northDistance > 0 && northPos /= lastPlace
                    then north:[]
                    else []
                    
  placeListEast  = if eastDistance > 0 && eastPos /= lastPlace
                    then east:placeListNorth
                    else placeListNorth
                    
  placeListSouth = if southDistance > 0 && southPos /= lastPlace
                    then south:placeListEast
                    else placeListEast
                    
  placeListWest  = if westDistance > 0 && westPos /= lastPlace
                    then west:placeListSouth
                    else placeListSouth
                    
  
--                  Position     OldPosition  lastPlacePos usedPosList    Place Distance newUsedPosList  
day18a_checkPath :: (Int,Int) -> (Int,Int) -> (Int,Int) -> [(Int,Int)] -> (Path,Int,[(Int,Int)])
day18a_checkPath pos@(x,y) oldPos lastPlace posList
 | char == '#'          = (Place char pos [], 0, posList)
 | length pathList == 0 = if char == '.'
                           then (Place char pos [], 0, posList)
                           else (Place char pos [], 1, posList)
 | length pathList == 1 && char == '.' = 
  (\(a,b,c) -> if b /= 0 then (a,b+1,c) else (a,b,c))
   (day18a_checkPath (fst (head pathList)) pos lastPlace posList)
 | elem pos posList     = (Place char pos [], 1, posList) -- circles
 | nextPlaceTrivial     = (\(a,b) -> (a,0,b)) nextPlace   -- trivial place (empty '.' Place that is not in posList (because of circle))
 | trivialNode          = (nextNextPlace, 1 + nextNextDistance ,nextPlacePosList)
 | otherwise            = (\(a,b) -> (a,1,b)) nextPlace   -- nextPlace is key, lock or non-trivial
 
 where
  char = data_day18a Map.! pos
  north = (x,y-1)
  northChar = data_day18a Map.! north
  east = (x+1,y)
  eastChar = data_day18a Map.! east
  south = (x,y+1)
  southChar = data_day18a Map.! south
  west = (x-1,y)
  westChar = data_day18a Map.! west
  
  pathList :: [((Int,Int),Char)]
  pathList = foldr
              (\(a,b) acc -> if a /= oldPos && b/='#' then (a,b):acc else acc) []
              (zip
               [north,east,south,west]
               [northChar,eastChar,southChar,westChar])
  
  -- nextPlaceTrivial might be wrong <- apparently not
  nextPlace = day18a_checkPlace pos lastPlace (pos:posList)
  nextPlaceChar = (\(Place a b c,d) -> a) nextPlace
  nextPlacePos = (\(Place a b c,d) -> b) nextPlace
  nextPlaceBranchList = (\(Place a b c,d) -> c) nextPlace
  nextPlacePosList = (\(Place a b c,d) -> d) nextPlace
  
  nextPlaceTrivial =
   nextPlaceChar == '.' && nextPlaceBranchList == [] && notElem nextPlacePos posList
   
  -- reduce trivialized pathnodes
  
  trivialNode = length nextPlaceBranchList == 1 && nextPlaceChar == '.'
  nextNextPlace = fst (head nextPlaceBranchList)
  nextNextDistance = snd (head nextPlaceBranchList)
                    
  

day18a_keys = [x | x <- Map.keys data_day18a_inverse, isLower x]
day18a_locks = [x | x <- Map.keys data_day18a_inverse, isUpper x]

data_day18a_inverse = Map.fromList (map (\(a,b) -> (b,a)) data_day18a_assocList)
data_day18a = Map.fromList data_day18a_assocList
data_day18a_assocList = zip
                         [(x,y) | y <- [0..day18a_yMax], x <- [0..day18a_xMax]]
                         (concat data_day18a_split)

day18a_xMax = (length (head data_day18a_split)) - 1
day18a_yMax = (length data_day18a_split) - 1
day18a_xLength = length (head data_day18a_split)
day18a_yLength = length data_day18a_split

data_day18a_split = splitOn ';' data_day18a_raw

day18a_fieldPos =
 [(x,y) |
  x <- [(day18a_start_x - 1)..(day18a_start_x + 1)],
  y <- [(day18a_start_y - 1)..(day18a_start_y + 1)]]
  
day18a_stemPos =
 [(x,y) |
  x <- [(day18a_start_x - 1),(day18a_start_x + 1)],
  y <- [(day18a_start_y - 1),(day18a_start_y + 1)]]
  
day18a_start_x = fst day18a_start
day18a_start_y = snd day18a_start

day18a_start = day18a_charToPos '@'


-- Day 18b

day18b =
 (\(a,b) -> (day18b_stringToDistance b,b))
 day18b_output

day18b_output = day18a_queuingAlgorithm day18b_distanceFunction day18b_hashFunction Map.empty Map.empty

day18b_distanceFunction keys nextKey =
 day18a_shortestPathMapLength Map.! (pos1,pos2)
 where
  branchKeys = head $ filter (elem nextKey) day18b_stemKeys
  branchPath = filter (`elem` branchKeys) keys
  pos1 | branchPath == "" = day18a_start
       | otherwise        = day18a_charToPos $ head branchPath
  pos2 = day18a_charToPos nextKey
  
day18b_hashFunction keys = hashHead ++ ';':(sort keys)
 where
  keyHeads =
   map
    (\x -> take 1 $ filter (`elem` x) keys)
    day18b_stemKeys
  hashHead = concat keyHeads

day18b_stringToDistance string = distance
 where 
  subStrings = map (\x -> filter (`elem` x) string) day18b_stemKeys
  paths =
   map
    (\x -> day18a_concatPaths
     (map (day18a_shortestPathMapFull Map.!)
      ((\a -> zip a (tail a)) (map day18a_charToPos x))))
    (map (++ "@") subStrings)
  distance = foldl (\acc x -> if x > 0 then acc + x - 2 else acc) 0 $ snd $ unzip paths
  
day18b_stemKeys = stemKeys
 where
  keyPosList = map day18a_charToPos day18a_keys
  stemKeyPos = map (\x -> filter (`elem` (snd x)) keyPosList) day18a_branchPosList
  stemKeys = map (map (data_day18a Map.!)) stemKeyPos

-- Day 19a

day19a = foldl (\acc (a,b) -> acc + b) 0 day19a_assocList

day19a_stringList = [[intToString $ day19a_map Map.! (x,y) | x <- day19a_xRange] | y <- day19a_yRange]
 where
  intToString x =
   case x of
    0 -> '.'
    1 -> '#'

day19a_map = Map.fromList day19a_assocList 

day19a_assocList = [((x,y),day19a_calc x y) | x <- day19a_xRange, y <- day19a_yRange] 

day19a_xRange = [0..day19a_xMax]
day19a_yRange = [0..day19a_yMax]

day19a_xMax = 49
day19a_yMax = 49

day19a_calc x y = head $ (\(a,b,c,d,e,f,g) -> g) output
 where
 output = cICC_output_raw (cICC_init data_day19a) 0 0 [x,y]
 
-- Day 19b

day19b = last $ day19b_search False day19b_start 20

day19b_search False pos@(x,y) i
 | i == 0 =
  []
 | north == 1 =
  if diagonal == 1
   then (x - 99,y):day19b_search False (x,y - 1) 20
   else day19b_search False (x,y - 1) (i - 1)
 | northWest == 1 =
  if diagonal == 1
   then (x - 99,y):day19b_search False (x - 1,y - 1) 20
   else day19b_search False (x - 1,y - 1) (i - 1)
 | otherwise =
  if diagonal == 1
   then (x - 99,y):day19b_search False (x - 1,y) 20
   else day19b_search False (x - 1,y) (i - 1)
 where
  northWest = day19a_calc (x - 1) (y - 1)
  north = day19a_calc x (y - 1)
  diagonal =
   day19a_calc (x - 99) (y + 99)
    
day19b_test x y = (northEast,(x,y),southWest,(x - 99,y + 99))
 where
  northEast = day19a_calc x y
  southWest = day19a_calc (x - 99) (y + 99)

day19b_start = day19b_walk output x y 
 where
  x = fst day19b_northGuess
  y = snd day19b_northGuess
  output = day19a_calc x y
 
day19b_walk lastOutput x y
 | output == lastOutput =
  day19b_walk output x nextY
 | otherwise =
  (x,y * output + (y + 1) * lastOutput)
 where
  output = day19a_calc x y
  nextY = y + 1 - 2 * lastOutput

day19b_northGuess = (x,y)
 where
  x = floor $ (99 / day19b_westSlope + 99 ) / (1 - day19b_northSlope / day19b_westSlope)
  y = floor ((fromIntegral x) * day19b_northSlope)

day19b_northSlope = quotientFromIntPair day19b_northPoint

day19b_westSlope = quotientFromIntPair day19b_westPoint

quotientFromIntPair (a,b) = y / x
 where
  x = fromIntegral a
  y = fromIntegral b

day19b_northPoint
 | day19b_verticalEdgeCount > 0 =
  (day19a_xMax,
   length
    (takeWhile (== 0)
     [day19a_map Map.! (day19a_xMax,y) | y <- day19a_yRange]))
 | otherwise =
  (day19a_xMax - length
    (takeWhile (== 0)
     [day19a_map Map.! (x,day19a_yMax) | x <- (reverse day19a_xRange)]),
   day19a_yMax)

day19b_westPoint
 | day19b_horizontalEdgeCount > 0 =
  (length
    (takeWhile (== 0)
     [day19a_map Map.! (x,day19a_yMax) | x <- day19a_xRange])
   ,day19a_yMax)
 | otherwise =
  (day19a_xMax,
   day19a_yMax - length
    (takeWhile (== 0)
     [day19a_map Map.! (day19a_xMax,y) | y <- (reverse day19a_yRange)]))

day19b_horizontalEdgeCount =
 sum [day19a_map Map.! (x,day19a_yMax) | x <- day19a_xRange]

day19b_verticalEdgeCount =
 sum [day19a_map Map.! (day19a_xMax,y) | y <- day19a_yRange]
 
-- Day 20a

day20a = fst $ day20a_output day20a_start day20a_end

day20a_output start end =
 day20a_queuingAlgorithm
  end
  (Map.singleton start (0,[start]))
  (Map.singleton 0 [start])

day20a_queuingAlgorithm goal itemMap priorityMap
 | elem goal posList =
  lowestItem
 | otherwise =
  day20a_queuingAlgorithm goal nextItemMap nextPriorityMap
 where
  lowestPriorityKey :: Int
  lowestPriorityKey = head $ Map.keys priorityMap
 
  lowestPriority = priorityMap Map.! lowestPriorityKey
   
  lowestPriorityHead = head lowestPriority
  lowestPriorityTail = tail lowestPriority
  
  lowestItem = Map.findWithDefault (maxBound,[]) lowestPriorityHead itemMap 
  
  distance = fst lowestItem
  posList = snd lowestItem
  pos = head posList
  
  options = (day20a_connectionMap Map.! pos) \\ posList
  
  nextItems :: [(Int,[(Int,Int)])]
  nextItems =
   map
    (\x -> (distance + 1,x:posList))
    options
  
  nextHashItemAssocs =
   map
    (\x@(a,b) -> ((head b),x))
    nextItems
  
  itemKeys = Map.keys itemMap  
  
  insertFlag =
   map
    (\(a,(b,c)) ->
     if
      notElem a itemKeys ||
      (\(d,e) -> d > b) (itemMap Map.! a)
      then True
      else False)
    nextHashItemAssocs
  
  nextItemMap =
   foldl
    (\acc (a,(b,c)) -> if a then Map.insert b c acc else acc)
    itemMap $ -- (Map.delete lowestPriorityHead itemMap) $
    zip insertFlag nextHashItemAssocs    
  
  priorityMapCleared
   | lowestPriorityTail == [] =
    Map.deleteMin priorityMap
   | otherwise                =
    Map.insert lowestPriorityKey lowestPriorityTail priorityMap
  
  nextPriorityMap =
    foldl
     (\acc (a,(b,(c,d))) ->
      if not a
       then acc
       else Map.insertWith (\[x] y -> x:y) c [b] acc)
     priorityMapCleared $
     zip insertFlag nextHashItemAssocs

day20a_connectionMap =
 Map.fromList $ (\x -> zip x $ map day20a_getConnections x) $ day20a_inverse Map.! '.'

day20a_getConnections pos@(x,y) = connections
 where
  adjacent =
   [(x - 1,y),(x + 1,y),(x,y - 1),(x,y + 1)]
  adjacentAssoc = zip adjacent $ map (day20a_map Map.!) adjacent
  dotPosList = fst $ unzip $ filter (\(x,y) -> y == '.') adjacentAssoc
  portalPos = Map.lookup pos day20a_portalConnections
  portalPosList =
   case portalPos of
    Just x -> [x]
    Nothing -> []
  connections = portalPosList ++ dotPosList

day20a_start :: (Int,Int)
day20a_start = head $ day20a_portals Map.! "AA"
day20a_end = head $ day20a_portals Map.! "ZZ"

day20a_portalConnections = Map.fromList assocList
 where
  portalMap = Map.delete "AA" $ Map.delete "ZZ" day20a_portals
  pairs = Map.elems portalMap
  assocList = foldr (\[x,y] acc -> (x,y):(y,x):acc) [] pairs

day20a_portals = Map.fromListWith (\[x] y -> x:y) assocList
 where
  labelPos = concat $ map (day20a_inverse Map.!) $ filter isUpper $ Map.keys day20a_inverse
  
  horizontalPosA =
   filter (\(x,y) -> isUpper $ Map.findWithDefault ' ' (x,y+1) day20a_map) labelPos
  horizontalPosB =
   map (\(x,y) -> (x,y+1)) horizontalPosA
  horizontalPosDot =
   map
    (\(x,y) ->
     if Map.findWithDefault ' ' (x,y-1) day20a_map == '.'
      then (x,y-1)
      else (x,y+2))
    horizontalPosA
    
  verticalPosA =
   filter (\(x,y) -> isUpper $ Map.findWithDefault ' ' (x+1,y) day20a_map) labelPos
  verticalPosB =
   map (\(x,y) -> (x+1,y)) verticalPosA
  verticalPosDot =
   map
    (\(x,y) ->
     if Map.findWithDefault ' ' (x-1,y) day20a_map == '.'
      then (x-1,y)
      else (x+2,y))
    verticalPosA
   
  labelsA = map (day20a_map Map.!) $ horizontalPosA ++ verticalPosA
  labelsB = map (day20a_map Map.!) $ horizontalPosB ++ verticalPosB
  dots = horizontalPosDot ++ verticalPosDot
  
  assocList = map (\(x,y,z) -> ([x,y],[z])) $ zip3 labelsA labelsB dots
   
    

day20a_inverse =
 Map.fromListWith
  (\[x] y -> x:y) $ map (\(a,b) -> (b,[a]))
   day20a_assocList
day20a_map = Map.fromList day20a_assocList
day20a_assocList =
 zip
  [(x,y) | y <- [0..day20a_yMax], x <- [0..day20a_xMax]]
  $ concat day20a_split

day20a_xMax = (length $ head day20a_split) - 1
day20a_yMax = (length day20a_split) - 1
day20a_xLength = length $ head day20a_split
day20a_yLength = length day20a_split

day20a_split = splitOn ';' data_day20a

-- Day 20b

day20b = fst $ day20b_output

day20b_output =
 day20b_queuingAlgorithm
  (Map.singleton (0,day20a_start) (0,[(0,day20a_start)]))
  (Map.singleton 0 [(0,day20a_start)])

day20b_queuingAlgorithm itemMap priorityMap
 | elem (0,end) levelPosList =
  lowestItem
 | otherwise =
  day20b_queuingAlgorithm nextItemMap nextPriorityMap
 where
  start = day20a_start
  end = day20a_end
  outerPortals = day20b_outerPortals
  innerPortals = day20b_innerPortals
 
  lowestPriorityKey :: Int
  lowestPriorityKey = head $ Map.keys priorityMap
 
  lowestPriority = priorityMap Map.! lowestPriorityKey
   
  lowestPriorityHead = head lowestPriority
  lowestPriorityTail = tail lowestPriority
  
  lowestItem = Map.findWithDefault (maxBound,[]) lowestPriorityHead itemMap 
  
  distance :: Int
  distance = fst lowestItem
  levelPosList = snd lowestItem
  levelPos = head levelPosList
  level = fst levelPos
  pos = snd levelPos
  
  nonPortalOptions =
   map
    (\(a,b) -> (a,(level,b))) $
    day20b_nonPortalConnections Map.! pos
  
  outerPortalsLevelZero =
   map (\a -> (0,a)) outerPortals
  
  nonPortalOptionsClipped =
   filter (\(a,b) -> notElem b outerPortalsLevelZero)
    nonPortalOptions
  
  portalOptions =
   map nonPortaltoPortalOption nonPortalOptionsClipped
  
  nonPortaltoPortalOption option
   | elem portal (start:end:[]) =
    (fst option,[snd option])
   | otherwise =
    portalOption
   where
    portal = snd $ snd option
    distance = (fst option) + 1
    throughPortal =
     day20a_portalConnections Map.! portal
    portalOption =
     (distance,[(modLevel portal throughPortal,throughPortal),snd option])
  
  options :: [(Int,[(Int,(Int,Int))])]  
  options =
   filter (\(a,b) -> all (`notElem` levelPosList) b) portalOptions
  
  modLevel pos nextPos = newLevel
   where
    -- deeper into maze
    innerToOuter = elem nextPos day20b_outerPortals && elem pos day20b_innerPortals
    -- less deep in maze
    outerToInner = elem nextPos day20b_innerPortals && elem pos day20b_outerPortals
    newLevel
     | innerToOuter = level + 1
     | outerToInner = level - 1
     | otherwise    = level
  
  nextItems :: [(Int,[(Int,(Int,Int))])]
  nextItems = map (\(a,b) -> (distance + a,b ++ levelPosList)) options    
  
  nextHashItemAssocs =
   map
    (\x@(a,b) -> ((head b),x))
    nextItems
  
  itemKeys = Map.keys itemMap  
  
  insertFlag =
   map
    (\(a,(b,c)) ->
     if
      notElem a itemKeys ||
      (\(d,e) -> d > b) (itemMap Map.! a)
      then True
      else False)
    nextHashItemAssocs
  
  nextItemMap :: Map.Map (Int, (Int, Int)) (Int,[(Int, (Int, Int))])
  nextItemMap =
   foldl
    (\acc (a,(b,c)) -> if a then Map.insert b c acc else acc)
    itemMap $
    zip insertFlag nextHashItemAssocs    
  
  priorityMapCleared
   | lowestPriorityTail == [] =
    Map.deleteMin priorityMap
   | otherwise                =
    Map.insert lowestPriorityKey lowestPriorityTail priorityMap
  
  nextPriorityMap =
    foldl
     (\acc (a,(b,(c,d))) ->
      if not a
       then acc
       else Map.insertWith (\[x] y -> x:y) c [b] acc)
     priorityMapCleared $
     zip insertFlag nextHashItemAssocs

day20b_nonPortalConnections = Map.fromList assocList
 where
  portals = Map.keys day20a_portalConnections
  exits =
   day20a_start:day20a_end:portals
  
  portalGroups = getGroups portals
  
  groupConnections :: [((Int,Int),(Int,(Int,Int)))]
  groupConnections =
   concat $
    map getGroupConnections portalGroups
  
  groupConnectionsSorted = sort groupConnections
  groupConnectionsGrouped :: [[((Int,Int),(Int,(Int,Int)))]]
  groupConnectionsGrouped =
   groupBy (\x y -> fst x == fst y) groupConnectionsSorted
  groupConnectionsGroupedUnzipped :: [([(Int,Int)],[(Int,(Int,Int))])]
  groupConnectionsGroupedUnzipped =
   map unzip groupConnectionsGrouped
  assocList =
   map
    (\(a,b) -> (head a,b))
    groupConnectionsGroupedUnzipped
    
  
  getGroupConnections :: [(Int,Int)] -> [((Int,Int),(Int,(Int,Int)))]
  getGroupConnections [_] = []
  getGroupConnections posList@(current:next) =
   assocList ++ (getGroupConnections next)
   where
    distance =
     fst $ unzip $
      map (day20a_output current) next
    assocList =
     foldr
      (\(a,b) acc ->
       (current,(a,b)):(b,(a,current)):acc)
      [] $
      zip distance next
  
  getGroups [] = []
  -- getGroups [x] = [[x]] -- should not happen
  getGroups portalList@(portal:otherPortals)
   = (portal:group):(getGroups nextPortalList)
   where
    linkedPortal =
     day20a_portalConnections Map.! portal
    group =
     walkToPortals
      portal [portal,linkedPortal]
    nextPortalList = otherPortals \\ group
    
  walkToPortals pos posList
   | elem pos exits && notElem pos posList =
    [pos]
   | connections == [] =
    []
   | otherwise =
    concat $
     map (`walkToPortals` newPosList) connections
   where
    connections = (day20a_connectionMap Map.! pos) \\ posList
    newPosList = pos:posList   

day20b_outerPortals = filteredPortalPos
 where
  portalPos = Map.keys day20a_portalConnections
  filteredPortalPos =
   portalPos \\ day20b_innerPortals

day20b_innerPortals = filteredPortalPos
 where
  portalPos = Map.keys day20a_portalConnections
  filteredPortalPos =
   filter (`elem` day20b_innerEdgeArea) portalPos

-- contains all but outermost donut line
day20b_outerEdgeArea =
 [(x,y) |
  x <- [(day20b_xLimits !! 1)..(day20b_xLimits !! 2)],
  y <- [(day20b_yLimits !! 1)..(day20b_yLimits !! 2)]]

-- only contains innermost donut line
day20b_innerEdgeArea =
 [(x,y) |
  x <- [(day20b_xLimits !! 1)..(day20b_xLimits !! 2)],
  y <- [(day20b_yLimits !! 1)..(day20b_yLimits !! 2)]]

day20b_xLimits =
 [donutLeftOuter,donutLeftInner,donutRightInner,donutRightOuter]
 where
  split0 = [day20a_map Map.! (x,day20b_yMid) | x <- [0..day20a_xMax]]
  donut = "#."
  split1 = dropWhile (`notElem` donut) split0
  split2 = dropWhile (`elem` donut) split1
  split3 = dropWhile (`notElem` donut) split2
  split4 = dropWhile (`elem` donut) split3
  
  donutLeftOuter = (length split0) - (length split1) + 1  -- 2nd line from left of donut
  donutLeftInner = (length split0) - (length split2) - 1  -- 1st line from inner left of donut
  donutRightInner = (length split0) - (length split3)     -- 1st line from inner right of donut
  donutRightOuter = (length split0) - (length split4) - 2 -- 2nd line from right of donut

day20b_yLimits =
 [donutTopOuter,donutTopInner,donutBottomInner,donutBottomOuter]
 where
  split0 = [day20a_map Map.! (day20b_xMid,y) | y <- [0..day20a_yMax]]
  donut = "#."
  split1 = dropWhile (`notElem` donut) split0
  split2 = dropWhile (`elem` donut) split1
  split3 = dropWhile (`notElem` donut) split2
  split4 = dropWhile (`elem` donut) split3
  
  donutTopOuter = (length split0) - (length split1)
  donutTopInner = (length split0) - (length split2) - 1
  donutBottomInner = (length split0) - (length split3)
  donutBottomOuter = (length split0) - (length split4) - 1

day20b_xMid = div day20a_xLength 2
day20b_yMid = div day20a_yLength 2

-- Day 21a

day21a = day21a_print day21a_solution

day21a_print solution =
 do
  putStrLn (day21a_output solution)
  return ()

day21a_output solution = string
 where
  output = reverse $ day21a_calc solution
  string
   | last output > 256 =
    (map chr $ init output) ++ (show $ last output)
   | otherwise =
    map chr output
    
day21a_calc solution = (\(a,b,c,d,e,f,g) -> g) output
 where
  output = cICC_output_raw (cICC_init data_day21a) 0 0 ord_solution
  ord_solution = map ord solution

day21a_solution = "NOT A T\nNOT C J\nOR T J\nAND D J\nWALK\n"

-- Day 21b

day21b = day21a_print day21b_solution

day21b_solution = "NOT C T\nAND D T\nOR D J\nAND F J\nNOT J J\nOR E J\nOR H J\nAND T J\nNOT A T\nOR T J\nOR B T\nOR E T\nNOT T T\nOR T J\nRUN\n"

-- Day 22a

day22a = fromJust $ Seq.elemIndexL 2019 day22a_output

day22a_output =
 day22a_calc day22a_length day22a_cards day22a_instructions

day22a_calc length cards [] = cards
day22a_calc length cards ((letters,numbers):nextInstructions) =
 day22a_calc length nextCards nextInstructions
 where
  nextCards
   | letters == "dealintonewstack" =
    day22a_deal cards
   | letters == "cut" =
    day22a_cut length cards (read numbers :: Int)
   | letters == "dealwithincrement" =
    day22a_increment length cards (read numbers :: Int)

day22a_increment length sequence n = newSequence
 where
  offsetLists = takeLists n 0 sequence
  riffleLists = snd $ unzip $ sort offsetLists
  newSequence = popUntilEmpty riffleLists 
  
  popUntilEmpty [] = Seq.empty
  popUntilEmpty list =
   currSeq Seq.>< nextSeq
   where
    output = popOneFromEachToSeq list
    currSeq = fst output
    nextList = snd output
    nextSeq = popUntilEmpty nextList   
  
  popOneFromEachToSeq [] = (Seq.empty,[])
  popOneFromEachToSeq (([]:listTail)) =
   popOneFromEachToSeq listTail
  popOneFromEachToSeq ((listHead:listTail):nextLists) =
    (listHead Seq.:<| nextSeq,listTail:tailList)
   where
    output = popOneFromEachToSeq nextLists
    nextSeq = fst output
    tailList = snd output
  
  takeLists m i sequence
   | m == i =
    []
   | otherwise =
    (offset,list):(takeLists m (i + 1) newSequence)
   where
    prevElement = (div (length * i - 1) m) + 1
    offset = mod (mod (m * prevElement) length) m
    amount = (div (length - offset - 1) m) + 1
    
    output = splitSeqToList sequence amount
    list = fst output
    newSequence = snd output
  
  splitSeqToList Seq.Empty _ = ([],Seq.empty)
  splitSeqToList sequence 0 = ([],sequence)
  splitSeqToList (sHead Seq.:<| sTail) m =
   (sHead:(fst output),snd output)
   where
    output = splitSeqToList sTail (m - 1)

day22a_cut length sequence m = (\(a,b) -> b Seq.>< a) split
 where
  n | m > 0 = m
    | otherwise = length + m
  split = Seq.splitAt n sequence

day22a_deal sequence = Seq.reverse sequence

day22a_instructions = zip letters numbers
 where
  split = splitOn ';' data_day22a
  letters = map (filter isLower) split
  numbers = map (filter (`elem` '-':['0'..'9'])) split

day22a_cards = Seq.fromList [0..(day22a_length - 1)]
day22a_length = 10007

-- Day 22b

day22b = day22b_apply day22b_endCard operator
 where
  binaryList = toBinaryList day22b_iterations
  iteratedOperatorList =
   iterateOperator binaryList day22b_compounded
  compounded = compoundOperatorList iteratedOperatorList
  operator =  day22b_inverse compounded

iterateOperator [] operator = []
iterateOperator (curr:next) operator
 | curr == 1 =
  operator:(iterateOperator next doubledOperator)
 | otherwise =
  iterateOperator next doubledOperator
 where
  doubledOperator = doubleOperator operator

compoundOperatorList [op] = op
compoundOperatorList (op1:op2:rest) =
 compoundOperatorList (newOp:rest)
 where
  newOp = compoundTwoOperators op1 op2

doubleOperator op = compoundTwoOperators op op

compoundTwoOperators (a,b) (c,d) = (a * c,b * c + d)  

-- from big endian binary list
fromBinaryList [0] n = 0
fromBinaryList [1] n = n
fromBinaryList (current:next) n =
 current * n + fromBinaryList next (n * 2)

-- to big endian binary list
toBinaryList 0 = [0]
toBinaryList 1 = [1]
toBinaryList number = modulo:(toBinaryList divided)
 where
  output = divMod number 2
  divided = fst output
  modulo = snd output

-- make inverse instruction x = y/a - b/a
day22b_inverse (a,b) = (1 / a,-1 * b / a)

day22b_compounded = day22b_compound day22a_instructions (1,0)

-- apply instruction/operator to number
day22b_apply number (a,b) = (number * a) + b

-- compound instructions to a*x + b = y form
day22b_compound [] coeffs = coeffs
day22b_compound ((letters,numbers):nextInstructions) coeffs =
 day22b_compound nextInstructions newCoeffs
 where
  newCoeffs
   -- multiplication with -1, then addition of -1
   | letters == "dealintonewstack" =
    day22b_mult (-1) $ day22b_add 1 coeffs
   -- subtraction
   | letters == "cut" =
    day22b_add (-1 * toField) coeffs 
   -- multiplication
   | letters == "dealwithincrement" =
    day22b_mult toField coeffs
   where
    readNumbers = read numbers :: Int    
    toField =  fromIntegral readNumbers :: Field

day22b_add add (a,b) = (a,b + add)

day22b_mult mult (a,b) = (a * mult,b * mult)
 
day22b_endCard = 2020
 
day22b_iterations = 101741582076661

type Field = Prime 119315717514047

-- Day 23a

-- (intMap, "Halt", opCode, position, relativeBase, input, output)

day23a = last day23a_output

day23a_output =
 day23a_loop nodeList packetList
 where
  mem0 = cICC_init data_day23a
  nodeList = [(n,(mem0,0,0)) | n <- [0..49]]
  packetList = [[n,n,-1] | n <- [0..49]] :: [[Int]]

day23a_loop :: [(Int,(Map.Map Int Int,Int,Int))] -> [[Int]] -> [[Int]]
day23a_loop nodeList packetList
 | head result == 255 =
   [result]
 | otherwise =
  result:(day23a_loop nextNodeList nextPacketList)
 where
  result
   | packetList /= [] =
    maximumBy (comparing head) packetList
   | otherwise =
    [-1]
  
  output =
   getPackets nodeList packetList
  
  output_unzipped = unzip output
  
  nextNodeList = zip [0..49] $ fst output_unzipped
  nextPacketList = sort $ concat $ snd output_unzipped
  
  getPackets [] packetList = [] -- may break for b
  getPackets ((n,state):nextNodeList) packetList = 
   (day23a_calc state packets):
    getPackets nextNodeList nextPacketList
   where
    split = break (\[a,b,c] -> a > n) packetList
    packetsRaw = concat $ map tail $ fst split
    packets
     | packetsRaw == [] = [-1]
     | otherwise = packetsRaw
    nextPacketList = snd split

day23a_calc (memory,position,offset) input =
 (\(a,b,c,d,e,f,g) -> ((a,d,e),spoolList 3 $ reverse g)) output
 where
 output = cICC_output_raw memory position offset input

-- Day 23b

day23b = take 2 day23b_output

day23b_output =
 day23b_loop nodeList packetList []
 where
  mem0 = cICC_init data_day23a
  nodeList = [(n,(mem0,0,0)) | n <- [0..49]]
  packetList = [[n,n,-1] | n <- [0..49]] :: [[Int]]

day23b_loop nodeList packetList natList
 | packetList == [] && natList /= [] =
  day23b_loop nodeList [0:(tail $ head natList)] natList
 | length natList > 2 &&
    (last $ head natList) == (last $ head $ tail natList) =
  natList
 | otherwise =
  day23b_loop nextNodeList nextPacketList nextNatList
 where
  nextNatList
   | packetList /= [] =
    (take 1 $ filter (\x -> head x == 255) packetList) ++ natList
   | otherwise =
    natList
  
  output =
   getPackets nodeList packetList
  
  output_unzipped = unzip output
  
  nextNodeList = zip [0..49] $ fst output_unzipped
  nextPacketList = sort $ concat $ snd output_unzipped
  
  getPackets [] packetList = [] -- may break for b
  getPackets ((n,state):nextNodeList) packetList = 
   (day23a_calc state packets):
    getPackets nextNodeList nextPacketList
   where
    split = break (\[a,b,c] -> a > n) packetList
    packetsRaw = concat $ map tail $ fst split
    packets
     | packetsRaw == [] = [-1]
     | otherwise = packetsRaw
    nextPacketList = snd split 
    
-- Day 24a

day24a = day24a_output

day24a_output = fromBinaryList list 0
 where
  list = map ((head day24a_calc) Map.!) day24a_coords

day24a_calc = day24a_iterate [day24a_map]

day24a_iterate bugMaps@(currMap:_)
 | elem nextBugMap bugMaps =
  nextBugMap:bugMaps
 | otherwise =
  day24a_iterate $ nextBugMap:bugMaps
 where
  nextBugMap =
   Map.fromList
    [(pos,day24a_check pos currMap) | pos <- day24a_coords]

day24a_check (x,y) bugMap
 | curr == 1 =
  if count == 1 then 1 else 0
 | otherwise =
  if count == 1 || count == 2 then 1 else 0
 where
  curr  = Map.findWithDefault 0 (x,y) bugMap
  north = Map.findWithDefault 0 (x,y-1) bugMap
  east  = Map.findWithDefault 0 (x+1,y) bugMap
  south = Map.findWithDefault 0 (x,y+1) bugMap 
  west  = Map.findWithDefault 0 (x-1,y) bugMap
  count = north + east + south + west  

day24a_map = Map.fromList day24a_assocList

day24a_assocList :: [((Int,Int),Int)]
day24a_assocList =
 zip
  day24a_coords
  $ map (\x -> if x == '#' then 1 else 0) $ concat day24a_split

day24a_coords = [(x,y) | y <- [0..day24a_yMax], x <- [0..day24a_xMax]]

day24a_xMax = (length $ head day24a_split) - 1
day24a_yMax = (length day24a_split) - 1
day24a_xLength = length $ head day24a_split
day24a_yLength = length day24a_split

day24a_split = splitOn ';' data_day24a

-- Day 24b

day24b = sum day24b_output

day24b_output = snd $ unzip $ fst day24b_calc

day24b_calc = day24b_iterate 0 $ Map.singleton 0 day24a_map

day24b_iterate n bugMapMap
 | n == 200 =
  (bugsPerLevel,bugMapMap) -- sum $ snd $ unzip bugsPerLevel
 | otherwise =
  day24b_iterate (n + 1) nextBugMapMap
 where
  levels = Map.keys bugMapMap
  bugsPerLevel =
   map
    (\(level,bugMap) -> (level,sum $ Map.elems bugMap)) $
    Map.toList $ bugMapMap
  filteredBugLevels =
   fst $ unzip $ filter (\(x,y) -> y > 0) bugsPerLevel
  minLevel = (minimum filteredBugLevels) - 1
  maxLevel = (maximum filteredBugLevels) + 1
  nextBugMapMap =
   Map.fromList
    [(level,day24b_nextMap level bugMapMap) |
     level <- [minLevel..maxLevel]]

day24b_nextMap level bugMapMap = Map.fromList assocList
 where
  assocList =
   [(pos,day24b_check level pos bugMapMap) | pos <- day24b_coords]

day24b_check level (x,y) bugMapMap
 | curr == 1 =
  if count == 1 then 1 else 0
 | otherwise =
  if count == 1 || count == 2 then 1 else 0
 where
  curr = day24b_findBugs 0 level (x,y) bugMapMap
  north = day24b_findBugs 0 level (x,y-1) bugMapMap
  east  = day24b_findBugs 1 level (x+1,y) bugMapMap
  south = day24b_findBugs 2 level (x,y+1) bugMapMap 
  west  = day24b_findBugs 3 level (x-1,y) bugMapMap
  count = north + east + south + west
  
day24b_findBugs direction level (x,y) bugMapMap
 | x < 0 =
  Map.findWithDefault 0 (1,2) $
   Map.findWithDefault Map.empty (level - 1)
    bugMapMap
 | x > day24a_xMax =
  Map.findWithDefault 0 (3,2) $
   Map.findWithDefault Map.empty (level - 1)
    bugMapMap
 | y < 0 =
  Map.findWithDefault 0 (2,1) $
   Map.findWithDefault Map.empty (level - 1)
    bugMapMap
 | y > day24a_yMax =
  Map.findWithDefault 0 (2,3) $
   Map.findWithDefault Map.empty (level - 1)
    bugMapMap
 | x == 2 && y == 2 && direction == 0 = -- north to south
   sum $
    map
     (\x ->
      Map.findWithDefault 0 (x,day24a_yMax) $
       Map.findWithDefault Map.empty (level + 1)
        bugMapMap)
     [0..day24a_xMax]
 | x == 2 && y == 2 && direction == 1 = -- east to west
  sum $
   map
    (\y ->
     Map.findWithDefault 0 (0,y) $
      Map.findWithDefault Map.empty (level + 1)
       bugMapMap)
    [0..day24a_yMax]
 | x == 2 && y == 2 && direction == 2 = -- south to north
  sum $
   map
    (\x ->
     Map.findWithDefault 0 (x,0) $
      Map.findWithDefault Map.empty (level + 1)
       bugMapMap)
    [0..day24a_xMax]
 | x == 2 && y == 2 && direction == 3 = -- west to east
  sum $
   map
    (\y ->
     Map.findWithDefault 0 (day24a_xMax,y) $
      Map.findWithDefault Map.empty (level + 1)
       bugMapMap)
    [0..day24a_yMax]
 | otherwise =
  Map.findWithDefault 0 (x,y) $
   Map.findWithDefault Map.empty level
    bugMapMap
     
day24b_map = Map.fromList day24b_assocList

day24b_assocList :: [((Int,Int),Int)]
day24b_assocList = delete ((2,2),0) day24b_assocList

day24b_coords = [(x,y) | y <- [0..day24a_yMax], x <- [0..day24a_xMax], x /= 2 || y /= 2]

-- Day 25b

-- Day 23a

-- (intMap, "Halt", opCode, position, relativeBase, input, output)

-- day25a = last day25a_output

day25a = day25a_init

day25a_init =
 do
  let
   output = day25a_calc (cICC_init data_day25a,0,0) []
   state = fst output
   string = snd output
  putStrLn string
  day25a_loop state
  return ()

day25a_loop state =
 do
  input <- getLine
  let
   output = day25a_calc state $ map ord $ input ++ "\n"
   newState = fst output
   string = snd output
  putStrLn string
  day25a_loop newState
  return ()
 where
  

-- day25a_output

-- day25a_input
  
day25a_calc (memory,position,offset) input =
 (\(a,b,c,d,e,f,g) -> ((a,d,e),map chr $ reverse g)) output
 where
 output = cICC_output_raw memory position offset input