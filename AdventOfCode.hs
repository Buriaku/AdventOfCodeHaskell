import Data.List
import Data.Array
import Data.Foldable
import Data.Char
import Data.Maybe
import Data.Ord
import Data.Function

import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import AdventOfCodeData

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
data IntTree =
 IntBranch [IntTree] [Int]
 deriving (Show, Eq)
 
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