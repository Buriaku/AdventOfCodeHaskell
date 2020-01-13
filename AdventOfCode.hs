import Data.List
import Data.Array
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
day11a_robot memory_old position_old relativeBase_old input map_old robotPosition_old@(x,y) robotFacing_old
 -- if halt then do next step
 | opCode == 3  = day11a_robot memory position relativeBase [mapColor] map robotPosition robotFacing                 
 -- if done ore error then end
 | opCode == 99 = (map, output_raw)
 | otherwise    = (map, output_raw)

 where
  -- do step
  output_raw = cICC_output_raw memory_old position_old relativeBase_old input
  memory = (\(a,b,c,d,e,f,g) -> a) output_raw
  opCode = (\(a,b,c,d,e,f,g) -> c) output_raw
  position = (\(a,b,c,d,e,f,g) -> d) output_raw
  relativeBase = (\(a,b,c,d,e,f,g) -> e) output_raw
  output = (\(a,b,c,d,e,f,g) -> g) output_raw
  
  -- don't swap these -.-'
  paintColor = last output
  rotate = head output
  
  -- put color from output into map
  mapColor_old = day11a_getColor map_old robotPosition_old
  map = Map.insert robotPosition_old paintColor map_old
         
  -- calculate robot movement
  robotFacing = mod (robotFacing_old + 3 + 2 * rotate) 4
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
 where color = map Map.!? p

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
primes = [n | n <- [2..], foldl (\acc x -> if mod n x == 0 then n:acc else acc) [] [2..(floor (sqrt (fromIntegral n)))] == []]

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

day13b_old input0 =
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
    
  block = inverseMap Map.!? 1 
  ball = inverseMap Map.! 4
  ball_x = (\(a,b) -> a) ball
  paddle = inverseMap Map.! 3
  paddle_x = (\(a,b) -> a) paddle

day13b_output input = spoolList 3 (reverse ((\(a,b,c,d,e,f,g) -> g) (day13b_calc input)))

day13b_calc_auto intMap input = cICC_output_raw intMap 0 0 input

day13b_calc input = cICC_output_raw (cICC_init data_day13b) 0 0 input

data_day13b = 2:(tail data_day13a)

-- Day 14a

day14a = (show totalOre) ++ " - " ++ (show redundantOre) ++ " = " ++ (show (totalOre - redundantOre))
 where
  output = day14a_getOre "FUEL"
  totalOre = (\(a,b,c) -> b) output
  waste = Map.toList (Map.fromListWith (\a b -> a + b) (map (\(a,b) -> (b,a)) ((\(a,b,c) -> c) output)))
  redundantOre = foldl (getRedundantOre) 0 waste

getRedundantOre acc (product,wasteMultiplier) = acc + (div wasteMultiplier productMultiplier) * productOre
 where
  output = day14a_getOre product
  productOre = (\(a,b,c) -> b) output
  productMultiplier = (\(a,b,c) -> a) output

-- day14a_getOre :: String -> (Int,Int)

day14a_getOre "ORE" = (1,1,[])

day14a_getOre product = (\a (b,c) -> (a,b,c)) productMultiplier (foldl (foldOre) (0,[]) eductList)
 where
  reaction = data_day14a Map.! product
  eductList = fst reaction
  productMultiplier = fst (snd reaction)

-- foldOre :: Int -> (Int,String) -> Int
foldOre (ore,waste) x@(multiplier,educt) =
 ((ore + reactionMultiplier * eductOre),
  if wasteMultiplier /= 0
   then ((wasteMultiplier,educt):eductWaste ++ waste)
   else waste)
 where
  eductOreReaction = day14a_getOre educt
  eductMultiplier = (\(a,b,c) -> a) eductOreReaction
  eductOre = (\(a,b,c) -> b) eductOreReaction
  eductWaste = (\(a,b,c) -> c) eductOreReaction
  reactionMultiplier = ((div multiplier eductMultiplier) +
                        if (mod (fromIntegral multiplier) (fromIntegral eductMultiplier) /= 0)
                         then 1
                         else 0)
  wasteMultiplier = reactionMultiplier * eductMultiplier - multiplier

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
  
  
{-
unspool ';' -> lines

unspool '=' lines -> (educts,product)

unspool ',' educt -> [educt]

unspool ' ' [educt],product -> number,element

read number
-}