{-# LANGUAGE FlexibleContexts #-}
module Proxy.PreProcessing.GraphConstructors where 
import Proxy.Math.Rectangle
import Proxy.Math.Interval
import Proxy.Math.Graph
import Data.Array
import Data.List
import qualified Data.Map as M
import Debug.Trace
import Proxy.PathFinding.HinzAStar
import Proxy.PathFinding.Specification


rPathFind :: (LabelledGraph g (Rectangle Int), WeightedGraph g Float) => g (Rectangle Int) Float -> (Int,Int) -> (Int,Int) -> Heuristic Float -> Maybe [Rectangle Int]
rPathFind g o d h = do
  n <- nodeOf o
  m <- nodeOf d
  path <- aStarSearch g n m h 
  return (map (label g) (completePath path))
  where nodeOf x = findIndex (x `iIR`) (allLabels g)

data Decomposition n = DD {recLookup :: M.Map Int (Rectangle n), nodeLookup :: M.Map (Rectangle n) Int}

data AdjR = FourWay | EightWay

adjacentTiles :: AdjR -> ((Int,Int) -> [(Int,Int)])
adjacentTiles FourWay = (\ (x,y) -> [(x-1,y),(x+1,y), (x,y+1), (x,y-1)])
adjacentTiles EightWay = (\ (x,y) -> filter (/= (x,y)) (range ((x-1,y-1),(x+1,y+1))))

                          
transferRect :: (Real a) => Rectangle a -> Rectangle a -> Rectangle a 
transferRect r s = case piX r `intersection` piX s of
  Nothing -> transposeR $ transferRect (transposeR r) (transposeR s) 
  Just xtr -> Rectangle xtr (mkInterval (sup ytr) (sup ytr - 1))
  where (Just ytr) = piY r `trivialIntersection` piY s

                     
rectiLPath :: AdjR -> (Int,Int) -> (Int,Int) -> [Rectangle Int] -> [(Int,Int)]
rectiLPath _ _ _ [] = error "rectiLPath : no Path can exist"
rectiLPath ar a b [r] = if a `iIR` r && b `iIR` r
                     then approach ar a b
                     else error "rectiLPath : source and destination not in remaining Rect"
rectiLPath ar a b (r:s:rs) = trace (show a) $ if a `iIR` r 
                                         then  (init intraPath) ++ (init interPath) ++  rectiLPath ar exit b (s:rs)
                                         else error "rectiLPath : origin not in rectangle"
  where intraPath = pathToRect ar a (transferRect r s)
        temp = last intraPath
        interPath = pathToRect ar temp s
        exit = last interPath 
        
        


pathToRect :: AdjR -> (Int,Int) -> Rectangle Int -> [(Int,Int)]
pathToRect ar (n,m) r = approach ar (n,m) closestInRect
  where closestInRect = (closestInInterval n (piX r),closestInInterval m (piY r))


approach :: AdjR -> (Int,Int) -> (Int,Int) -> [(Int,Int)]
approach EightWay (x,y) (z,w) =  (takeWhile g $ zip ([x..z] ++ repeat z) ([y..w] ++ repeat w)) ++ [(z,w)]
  where g (a,b) = a /= z || b /= w
approach FourWay (x,y) (z,w) =  zip (init [x..z]) (repeat y) ++ (zip (repeat z) [y..w])


nodeLookUp :: [Rectangle Int] -> Array Int (Rectangle Int)
nodeLookUp xs = listArray (1,length xs) xs

allEdges :: [Rectangle Int] -> [(Edge,Float)]
allEdges rs = concatMap (flip (uncurry edgesOf) rs) $ zip [0..] rs  

edgesOf :: Node -> Rectangle Int -> [Rectangle Int] -> [(Edge,Float)]
edgesOf n r rs = map f . filter (touching r . snd) $ zip nats rs
  where touching x y = x /= y && (==) 0 (rDist x y)
        nats = iterate (+1) 0
        f (m,s) = ((n,m),centersDistance r s)

mkDecomposition :: (Ord n) => [Rectangle n] -> Decomposition n
mkDecomposition xs = DD (M.fromList $ zip nats xs) (M.fromList $ zip xs nats)
  where nats = iterate (+1) 0

newEdges :: [Rectangle Int] -> [Edge]
newEdges = posOfValidPairs touching
  where touching x y = (==) 0 (rDist x y)
        
posOfValidPairs :: (a -> a -> Bool) -> [a] -> [(Int,Int)]
posOfValidPairs g = map fsts . validPairs g' . zip [0..]
  where g' x y = g (snd x) (snd y)
        fsts (x,y) = (fst x, fst y)
        
--assume g x y = g y x
validPairs :: (a -> a -> Bool) -> [a] -> [(a,a)] 
validPairs g xs = [(i,j) | i <- xs , j <- xs, g i j]

f :: Eq a => [(a,b)] -> [[(a,b)]]
f [] = [] 
f (x:xs) = as : f bs
 where (as,bs) = partition ((== (fst x)).fst) (x:xs)
       


{-type NodeLookup = [LNode (Rectangle Int)]

mapDecomp :: [[Tile]] -> [LNode (Rectangle Int)]
mapDecomp = zip (iterate (+1) 1) . maxRectDecomp   

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
1 | width r == 0 = toLine (inf.xI $ r) yI mkPointxFirst
 | otherwise = error "None trivial Rectangle"
  where toLine fixed inter fixedFirstPointCons = mkLineSegment (fixedFirstPointCons fixed (inf.inter $ r)) (fixedFirstPointCons fixed (sup.inter $ r))
 -}      
