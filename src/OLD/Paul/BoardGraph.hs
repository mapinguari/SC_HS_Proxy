module Proxy.Paul.BoardGraph where

import Proxy.Game(Tile(..), Map(..), UnitId, walkable)
import Proxy.Paul.Graph 
import Data.Array
import Proxy.Paul.AStar

type Position = (Integer,Integer)
type Edge = (Position,Position)

listToArray :: Map -> Array Position Tile
listToArray (Map n w h tss) = listArray ((1,1),(h,w)) (concat tss)

mapToGraph :: RealFloat a => Array Position Tile -> Graph Position a
mapToGraph a = array (bounds a) [(i,[(j,d j i)| j <- (adjacents a i)])| i <- (range.bounds $ a)]

adjacents :: Array Position Tile -> Position -> [Position]
adjacents a p = if walkable (a!p)
          then filter (walkable.(a!)) (neighbours a p)
          else []

neighbours :: Array Position v -> Position -> [Position]
neighbours a p@(x,y) = filter (fand tests) $ range ((x-1,y-1),(x+1,y+1))
  where tests = [(/=p) ,inRange (bounds a)]

onBorder :: Array Position Tile -> Position -> [Position]
onBorder a p = filter ((fand tests).(a!)) (neighbours a p)
  where tests = [walkable, diffH (a!p)]

diffH :: Tile -> Tile -> Bool
diffH (Tile h b w) (Tile h' b' w') = h /= h'

opmap :: [a -> b] -> a -> [b]
opmap [] _ = []
opmap (x:xs) y = x y : opmap xs y

fand :: [a -> Bool] -> a -> Bool 
fand xs = and.(opmap xs)

d :: (RealFloat a) => Position -> Position -> a 
d (x,y) (z,w) = sqrt.fromInteger $ ((x-z)^2) + ((y-w)^2)

movePath :: (RealFloat a) => Graph Position a -> UnitId -> Position -> Position -> Maybe (Path Position)
movePath g id o de = aStar g (\ x -> d de x) (==de) o

