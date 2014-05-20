module Proxy.PreProcessing.MapDecomposition (runLengthEncoding,generalisedDeltaMethod) where
import Data.List (mapAccumL)
import Control.Monad.State
import Data.Maybe (isJust)
import Proxy.Types.Game
import Proxy.Math.Rectangle
import Proxy.Math.Interval


type Map = [[Tile]] 
--------------------------------------------------------------------------------------------
--for each row Group tiles together if they are adjacent and walkable
runLengthEncoding :: Map -> [Rectangle]
runLengthEncoding = concat.mapToRect.map (map walkable)

mapToRect :: [[Bool]] -> [[Rectangle]]
mapToRect = snd.(mapAccumL (\n xs -> (n+1, rowToRect n xs)) 1)
--mutually recursive function
rowToRect :: Int -> [Bool] -> [Rectangle]
rowToRect row xs = rmObstacle 1 xs
  where rmObstacle _ [] = []
        rmObstacle n ys = 
          let blockSize = length obstacle 
              obstacle = takeWhile (not.id) ys 
              rest = dropWhile (not.id) ys
          in getRect (n+blockSize) rest
        getRect _ [] = []
        getRect n ys = 
          let rectWidth = length rect
              rest = dropWhile id ys 
              rect = takeWhile id ys 
          in Rectangle (mkInterval (row - 1) row) (mkInterval (n - 1) (n-1+rectWidth)) : rmObstacle (n+rectWidth) rest
----------------------------------------------------------------------------
----------------------If two rectangles are above one another and have the same horizontal bounds, group them into one rectangle.
generalisedDeltaMethod :: Map -> [Rectangle]
generalisedDeltaMethod = concat.gDM.mapToRect.map (map walkable)

gDM :: [[Rectangle]] -> [[Rectangle]]
gDM (xs:[]) = [xs]
gDM (xs:ys:xss) = zs: gDM (ws:xss)
  where (zs,ws) = rowMerge xs ys
        
rowMerge :: [Rectangle] -> [Rectangle] -> ([Rectangle],[Rectangle])
rowMerge xs [] = (xs,[])
rowMerge [] ys = ([],ys)
rowMerge (x:xs) (y:ys) 
  | (inf.piY) x > (sup.piY) y = (us,y:vs)
  | (inf.piY) y > (sup.piY) x = (x:zs,ws)
  | piY y == piY x = case merge y x of 
    Nothing -> (as,bs)
    Just a -> (as,a:bs)
  | otherwise = (x:as,y:bs)
  where (us,vs) = rowMerge (x:xs) ys
        (zs,ws) = rowMerge xs (y:ys)
        (as,bs) = rowMerge xs ys

--Assuming that r s are disjoint
merge :: Rectangle -> Rectangle -> Maybe Rectangle
merge r s 
  | isJust $ ry `intersection` sy = case (unionI rx sx,intersection ry sy) of
    (Just a, Just b) -> Just $ Rectangle a b
    otherwise -> Nothing
  | isJust $ rx `intersection` sx  = case (intersection rx sx,unionI ry sy) of
    (Just a, Just b) -> Just $ Rectangle a b
    otherwise -> Nothing
  | otherwise = Nothing
  where rx = piX r
        ry = piY r
        sx = piX s
        sy = piY s
