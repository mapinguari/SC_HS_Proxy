module Proxy.PreProcessing.GraphConstructors where 
import Proxy.Math.Rectangle
import Proxy.Math.Graph
import Data.Array
import Data.List

validSuffix :: (LabelledGraph g) => g Bool b -> Path -> Path 
validSuffix g p

nodeLookUp :: [Rectangle Int] -> Array Int (Rectangle Int)
nodeLookUp xs = listArray (1,length xs) xs

edgesOf :: Node -> Rectangle Int -> [Rectangle Int] -> [Edge]
edgesOf n r = filter (touching r . snd) . zip nats
  where touching x y = (==) 0 (rDist x y)

newEdges :: [Rectangle Int] -> [Edge]
newEdges = posOfValidPairs touching
  where touching x y = (==) 0 (rDist x y)
        
posOfValidPairs :: (a -> a -> Bool) -> [a] -> [(Int,Int)]
posOfValidPairs g = map fsts . validPairs g' . zip [1..]
  where g' x y = g (snd x) (snd y)
        fsts (x,y) = (fst x, fst y)
        
--assume g x y = g y x
validPairs :: (a -> a -> Bool) -> [a] -> [(a,a)] 
validPairs g xs = filter (uncurry g) [(i,j) | i <- xs , j <- xs]

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
