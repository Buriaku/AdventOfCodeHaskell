import Data.List
import qualified Data.Map as Map
import AdventOfCodeData


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

data_day08a_layers = spoolString (25*6) data_day08a

spoolString :: Int -> String -> [String]
spoolString n [] = []
spoolString n string = (take n string):(spoolString n (drop n string))

-- Day 08b

day08b = spoolString 25 [day08b_pixel n data_day08a_layers | n <- [0..((25*6)-1)]]

day08b_pixel n list
 | pixel == '2' = day08b_pixel n (tail list)
 | otherwise    = pixel
 
 where pixel = (head list) !! n