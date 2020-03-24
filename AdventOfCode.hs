{-# LANGUAGE BangPatterns #-}

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
movePoint (Point x y) South = (Point x (y + 1))
movePoint (Point x y) West = (Point (x - 1) y)
movePoint (Point x y) East = (Point (x + 1) y)

turnDirection North Clockwise = East 
turnDirection East Clockwise = South
turnDirection South Clockwise = West
turnDirection West Clockwise = North

turnDirection North Counterclockwise = West
turnDirection East Counterclockwise = North
turnDirection South Counterclockwise = East
turnDirection West Counterclockwise = South