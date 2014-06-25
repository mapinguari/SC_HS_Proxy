module Proxy.PreProcessing.RowBased (runLengthEncoding) where
import Data.List (mapAccumL)
import Control.Monad.State
import Data.Maybe (isJust)
import Proxy.Types.Game
import Proxy.Math.Rectangle
import Proxy.Math.Interval 
import Data.Array
import Data.List

                
batCheck n = [[Tile 1 (odd (i+j)) False | i <- [0..(n-1)]]| j <-[0..(n-1)]]
rectangles h n = [Rectangle (mkInterval i (i+1)) (mkInterval h (h+1)) | i <-[1,3..n]]

type Map = [[Tile]] 
type MapDecomposition = Map -> [Rectangle Int]
--------------------------------------------------------------------------------
--Each tile is a rectangle 
mapToRect :: MapDecomposition
mapToRect ts = foldr g [] (zip [(i,j) |j <- [0..h], i <- [0..w]] (concat ts))
  where h = length ts - 1
        w = length (head ts) - 1
        g ((x,y),tile) xs = if walkable tile then (Rectangle (mkInterval x (x+1)) (mkInterval y (y+1))) : xs else xs

--------------------------------------------------------------------------------------------
--for each row Group tiles together if they are adjacent and walkable
runLengthEncoding :: MapDecomposition
runLengthEncoding = concat . almostRLE

almostRLE :: [[Tile]] -> [[Rectangle Int]]
almostRLE = snd . mapAccumL g 0 
  where g n xs = (n+1, map f . listOfRuns . map walkable $ xs)
          where f = \(x1,x2) -> Rectangle (mkInterval x1 x2) (mkInterval n (n+1))
 
listOfRuns :: [Bool] -> [(Int,Int)]
listOfRuns xs = lR 0 xs 
  where lR _ [] = []
        lR n xs | null ts = lR (n+1) (tail fs)
                | otherwise = (n,e) : lR e fs
          where e = n + (length ts)
                (ts,fs) = span id xs


-------------------------------------------------------------------------------------------------
  
mergeX :: (Real a) => Rectangle a -> Rectangle a -> Maybe (Rectangle a)
mergeX a b = let x = piX a in 
  if x == piX b  
  then iUnion (piY a) (piY b) >>= (\y -> Just (Rectangle x y))
  else Nothing

generalizedDelta :: MapDecomposition 
generalizedDelta = rectsMerge . runLengthEncoding 

rectMerge :: (Real a) => Rectangle a -> [Rectangle a] -> (Rectangle a , [Rectangle a])
rectMerge r [] = (r,[])
rectMerge r (s:ts) = case mergeX r s of
  Nothing -> let (f,rest) = rectMerge r ts in (f,s:rest)
  Just n -> let (f,rest) = rectMerge n ts in (f,rest)
  
rectsMerge :: (Real a) => [Rectangle a] -> [Rectangle a]
rectsMerge [] = [] 
rectsMerge (r:rs) = nr : rectsMerge stillToGo
  where (nr,stillToGo) = rectMerge r rs

gd :: MapDecomposition
gd =  together . almostRLE

lgd :: MapDecomposition
lgd = together' . almostRLE

together :: (Real a) => [[Rectangle a]] -> [Rectangle a]
together ts = concat (mergeAndGather [] ts)
  where mergeAndGather as [] = [as]
        mergeAndGather as (bs:tss) = let (a,b) = listMerge bs as in 
          b : mergeAndGather a tss
          
together' :: (Real a) => [[Rectangle a]] -> [Rectangle a] 
together' ts = concat (ttt [] ts)
  where ttt as [] = [as]
        ttt as (bs:tss) = let (a,b) = linearListMerge bs as in 
          (b : ttt a tss)
        
listMerge :: (Real a) => [Rectangle a] -> [Rectangle a] -> ([Rectangle a],[Rectangle a])
listMerge [] os = ([],os)
listMerge ns [] = (ns,[])
listMerge (n:ns) os = (r:a,b)
  where (r,left) = rectMerge n os
        (a,b) = listMerge ns left
        
linearListMerge :: (Real a) => [Rectangle a] -> [Rectangle a] -> ([Rectangle a], [Rectangle a]) 
linearListMerge [] xs = ([],xs)
linearListMerge xs [] = (xs,[])
linearListMerge (x:xs) (y:ys) 
  | x `isLessAdvancedThan` y = let (ns,os) = linearListMerge xs (y:ys) in (x:ns,os)
  | y `isLessAdvancedThan` x = let (ns,os) = linearListMerge (x:xs) ys in (ns,y:os)
  | otherwise = let (ns,os) = linearListMerge xs ys in (maybeCons (mergeX x y) ns,os)
  where isLessAdvancedThan r s = (inf.piX $ r) < (inf.piX $ s)
        maybeCons Nothing xs = xs
        maybeCons (Just a) xs = a:xs
