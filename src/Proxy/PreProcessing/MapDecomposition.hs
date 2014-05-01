module Proxy.PreProcessing.MapDecomposition where
import Proxy.Math.Rectangle
import Proxy.Math.Interval
import Proxy.Math.Line
import Proxy.Types.Game hiding (height,x,y)
import Control.Monad.State
import Data.List (maximumBy)
import Data.Array
import Data.Maybe
import Debug.Trace
import Data.Ratio
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Control.Applicative
import Data.List

type NodeLookup = [LNode (Rectangle Int)]

mapToGraph :: [[Tile]] -> Gr (Rectangle Int) LineSeg
mapToGraph xs = mkGraph nodes (edgeFinder nodes)
  where nodes = mapDecomp xs  

manhattanD :: Rectangle Int -> Rectangle Int -> Int 
manhattanD r s = interIntervalDistance (xI r) (xI s) + interIntervalDistance (yI r) (yI s)

edgeRectangle :: LEdge LineSeg -> Rectangle Int
edgeRectangle (_,_,ls) = if invarientX.lineEq $ ls 
                               then Rectangle (mkInterval h (typeFix.x.end $ ls)) (mkInterval (v+1) (v-1))
                               else Rectangle (mkInterval (h+1) (h-1)) (mkInterval v (typeFix.y.end $ ls))
                                 where v = typeFix.y.start $ ls
                                       h = typeFix.x.start $ ls
                                       typeFix = fromIntegral.numerator


nodeNamed :: Node -> [LNode (Rectangle Int)] -> Rectangle Int
nodeNamed n = snd.head.filter ((==n).fst)

edgeLength :: (Show a,Floating a) => [LNode (Rectangle Int)] -> LEdge LineSeg -> a
edgeLength nm (s,d,l) = case lineIntersect (lineEq idealRoute) (lineEq l) of
  Just (Left p) -> if onLineSeg p l 
                   then lineSegLength idealRoute 
                   else distance source (wayPoint p) + distance (wayPoint p) destination 
  otherwise -> error "None intersecting lineSegments - edgeLength"
  where idealRoute = mkLineSegment source destination 
        source = center . (flip nodeNamed nm) $ s
        destination = center.(flip nodeNamed nm) $ d
        wayPoint p = minBy (distance p) source destination

minBy :: (Ord b) => (a -> b) -> a -> a -> a
minBy f a b = if f a <= f b then a else b

edgeFinder :: [LNode (Rectangle Int)] -> [LEdge LineSeg]
edgeFinder xs = foldl edgeMaker [] allPairs
  where allPairs = [(x,y) | x <- xs, y <- tail.dropWhile (/= x) $ xs]
        edgeMaker es ((n,r),(m,s)) = case rectangleI r s of 
          Nothing -> es
          Just r -> if isPoint r
                    then es
                    else addEdges n m (trivalRectToLineSeg r) es        
        addEdges n m r xs = (n,m,r) : (m,n,r) : xs
        isPoint r = width r == 0 && height r == 0
        
trivalRectToLineSeg :: (Real a) => Rectangle a -> LineSeg
trivalRectToLineSeg r 
 | height r == 0 = toLine (inf.yI $ r) xI mkPointyFirst
 | width r == 0 = toLine (inf.xI $ r) yI mkPointxFirst
 | otherwise = error "None trivial Rectangle"
  where toLine fixed inter fixedFirstPointCons = mkLineSegment (fixedFirstPointCons fixed (inf.inter $ r)) (fixedFirstPointCons fixed (sup.inter $ r))
        
mapDecomp :: [[Tile]] -> [LNode (Rectangle Int)]
mapDecomp = zip (iterate (+1) 1) . maxRectDecomp 

batCheck n m = [[f i j | i <- [0..(m-1)]] | j <- [0..(n-1)]]
               where f i j = Tile 1 (i /= j) False
  

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--------------------Rectangular Decompostion Stuff-----------------------

maxRectDecomp :: [[Tile]] -> [Rectangle Int] 
maxRectDecomp = evalState allMax.tileToWalk
  where tileToWalk = map (map walkable)

allMax :: State [[Bool]] [Rectangle Int]
allMax = do 
  r <- nextMax 
  case r of
    Nothing -> return []
    Just r -> do 
      rs <- allMax
      return $ r:rs

nextMax :: State [[Bool]] (Maybe (Rectangle Int))
nextMax = state (\xs -> let h = length xs 
                            w = length.head $ xs in case mapMax xs of 
                          Just max -> (Just max, mask applyMask (craftMask h w max) xs)
                          otherwise -> (Nothing,xs))


------------------MASK STUFF STARTS HERE-------------------------
craftMask :: Height -> Width -> Rectangle Int -> [[Bool]]                        
craftMask h w r = [[f i j | i <- [0..(w-1)]]| j <- [0..(h-1)]]
                  where f i j = not $ mkPointxFirst (fromIntegral i + 0.5) (fromIntegral j + 0.5) `isInRect` r
                                     
applyMask :: [Bool] -> [Bool] -> [Bool]
applyMask = zipWith (&&)

mask :: (a -> b -> c) -> [a] -> [b] -> [c]
mask f mask ys 
  | length mask /= length ys = error "Mask not correct size"
  | otherwise = zipWith f mask ys
                
--------------------------------------------------------------------------
---------------MAP STUFF STARTS HERE-------------------------------

mapMax :: [[Bool]] -> Maybe (Rectangle Int)
mapMax ys = maximum . (mapMaybe (uncurry maxOnRow)).assocs $ h
  where h = array (1,(length ys)) $ [(i, zipWith g (h!(i-1)) ys)| (i,ys) <- tail xs] ++ [head xs]
        g x y = if y == 0 then 0 else x+y
        xs = zip nats (map (map (\b -> if b then 1 else 0)) ys)
        nats = iterate (+1) 1
        maximum xs = case xs of
          [] -> Nothing
          otherwise -> Just $ maximumBy largerOrSquarer xs 
                
maxOnRow :: Int -> [Int] -> Maybe (Rectangle Int)
maxOnRow n = fmap (addHeight) .  maxRectangle 
  where addHeight r = Rectangle (xI r) (mkInterval n ((n-).sup.yI $ r))
        



        
-------------HISTOGRAM STUFF STARTS HERE----------------------


maxRectangle :: [Int] -> Maybe (Rectangle Int)
maxRectangle xs = if null open 
                  then Nothing
                  else Just $ maximumBy largerOrSquarer rs
  where rs = runs open close []
        open = openings xs
        close = closings xs
        
maxRectangle' :: [Int] -> Maybe (Rectangle Int)
maxRectangle' xs = if null open 
                   then Nothing
                   else Just $ maximumBy largerOrSquarer rs
  where rs = runs open close []
        (open,close) = openingAndClosing xs


runs :: [(Index,Range)] -> [(Index,Range)] -> [(Index,Range)] -> [Rectangle Int]
runs _ [] _ = []
runs oS@(~(o:os)) (c:cs) (~(s:ss)) 
  | null oS = getRectangleAndContinue 
  | index o < index c = runs os (c:cs) (o:s:ss)
  | index o > index c = getRectangleAndContinue
  where index (i,r) = i
        (r,con) = rangeMerge s c 
        getRectangleAndContinue = case con of
          Nothing -> r : runs (o:os) cs ss
          Just (i,range) -> if i == index c
                           then r : runs oS ((i,range):cs) ss
                           else r : runs oS cs ((i,range):ss) 
        
rangeMerge :: (Index,Range) -> (Index,Range) -> (Rectangle Int, Maybe (Index,Range))
rangeMerge (oi,or) (ci,cr) | or == cr = (Rectangle (mkInterval oi ci) (mkInterval 0 (fst or)), Nothing)
                           | snd or < snd cr = (Rectangle (mkInterval oi ci) (mkInterval 0 (fst or)), Just (oi,(snd cr - 1, snd or)))
                           | otherwise = (Rectangle (mkInterval oi ci) (mkInterval 0 (fst or)), Just (ci, (snd or - 1,snd cr)))


type Index = Int
type Range = (Int,Int)
                                 
openingAndClosing :: [Int] -> ([(Index,Range)],[(Index,Range)])
openingAndClosing xs = cs 0 (zip nats xs)
  where nats = iterate (+1) 0 
        cs n [] = ([],if n == 0 then [] else [(e,(n,1))])
        cs n (y:ys) = let (as,bs) = cs (snd y) ys in
          (o n y as,c n y bs)
        o n (p,h) ys | h > n = (p,(h,n+1)) : ys
                     | otherwise = ys
        c n (p,h) ys | h < n = (p,(n,h+1)) : ys
                     | otherwise = ys
        e = length xs


openings :: [Int] -> [(Index,Range)]
openings = (o 0) . (zip nats)
  where nats = iterate (+1) 0
        o _ [] = []
        o n ((p,h):ys) | h > n = (p,(h,n+1)) : o h ys
                       | otherwise = o h ys

                                     
closings :: [Int] -> [(Index,Range)]
closings xs = c 0 (zip nats xs)
  where nats = iterate (+1) 0 
        c n [] | n == 0 = []
               | otherwise = [(e,(n,1))]
        c n ((p,h): ys) | h < n = (p ,(n,h+1)) : c h ys
                        | otherwise = c h ys
        e = length xs


-------Slightly less naieve------------------


runHeights :: (Eq a) => [a] -> [a] 
runHeights [] = []
runHeights (x:xs) = f x xs 
  where f x [] = [x]
        f x (y:ys) | x == y = f x ys
                   | otherwise = x : f y ys

f :: [Int] -> Maybe (Rectangle Int)
f xs = g . catMaybes .(takeWhile isJust) $ [biggestOfHeight i xs | i <- (runHeights xs)]
  where g [] = Nothing
        g xs = Just $ maximumBy largerOrSquarer xs
        
maxRectangleOfHistogram :: [Int] -> Maybe (Rectangle Int)
maxRectangleOfHistogram xs = g.catMaybes $ [biggestOfHeight i xs | i <- [1..(maximum xs)]]
  where g [] = Nothing
        g xs = Just $ maximumBy largerOrSquarer xs

biggestOfHeight :: Int -> [Int] -> Maybe (Rectangle Int)
biggestOfHeight n = histRectangle.longestRun.map (>=n)
  where histRectangle r = case r of 
          Nothing -> Nothing
          Just (s,f) -> Just $ Rectangle (mkInterval s (f+1)) (mkInterval 0 n)

longestRunStartAndFinish :: [Bool] -> Maybe (Int,Int)
longestRunStartAndFinish = lR 0 0 0 0 0
  where lR a b c d _ [] | b == 0 = Nothing
                        | d - c > b - a = Just (c,d)
                        | otherwise = Just (a,b)
        lR a b c d n (x:xs) | x = lR a b c n (n+1) xs
                            | d - c > b - a = lR c d (n+1) (n+1) (n+1) xs
                            | otherwise = lR a b (n+1) (n+1) (n+1) xs

longestRun :: [Bool] -> Maybe (Int,Int)
longestRun xs = case rs of 
  [] -> Nothing
  otherwise -> Just $ maximumBy intervalSize rs
 where intervalSize (a,b) (c,d) = compare (b-a) (d-c)
       rs = listOfRuns xs

listOfRuns :: [Bool] -> [(Int,Int)]
listOfRuns xs = lR 0 xs 
  where lR n [] = []
        lR n xs | null ts = lR (n+1) (tail fs)
                | otherwise = (n,e) : lR (e+1) fs
          where e = n + (length ts) - 1
                (ts,fs) = span id xs
                

  
------NAieve------------------
        
allRectangles :: Int -> Int -> [Rectangle Int]
allRectangles h w = [Rectangle (mkInterval i (i+w')) (mkInterval j (j+h'))| i <- [0..(h-1)], j <- [0..(w-1)], w' <- [1..h], h' <- [1..w], i+w'<=w,j+h' <= h]

tileToRectangle :: [[Tile]] -> [Rectangle Int] 
tileToRectangle ts = map fst $ filter (not.walkable.snd) $ zip [oneByoneFrom i j | j <- [0..(h-1)], i <- [0..(w-1)]] (concat ts)
  where oneByoneFrom i j = Rectangle (mkInterval i (i+1)) (mkInterval j (j+1))
        w = length.head $ ts
        h = length ts
        
legalRectangles :: [Rectangle Int] -> [Rectangle Int] -> [Rectangle Int]
legalRectangles os rs = filter f rs
  where f r = null . (filter (not.trivial)) . catMaybes $ (osf <*> [r])
        osf = map rectangleI os
        trivial r = area r == 0 

rectanglesInOrder :: [Rectangle Int] -> [Rectangle Int]
rectanglesInOrder = reverse.sortBy (largerOrSquarer) 

covers :: Rectangle Int -> Rectangle Int -> Bool
covers r s = f $ rectangleI r s
  where f m = case m of 
          Nothing -> False
          Just r -> not.trivial $ r
        trivial r = area r == 0

decomp :: [[Tile]] -> [Rectangle Int]
decomp ts = d rs
  where rs = rectanglesInOrder . (legalRectangles os) $ allRectangles h w
        os = tileToRectangle ts
        w = length.head $ ts
        h = length ts
        d [] = []
        d (r:rs) = r : d (filter (not.(covers r)) rs)