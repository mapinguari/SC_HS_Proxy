module Proxy.Paul.AStar where

import Proxy.Game(Tile(..), walkable)
import Data.Array
import Data.Set(Set(),empty,member,toList,insert)

--Find tiles adjacent to tiles in other nodes.

type Position = (Integer,Integer)

onBorder :: Array Position Tile -> Position -> [] Position
onBorder a p = filter ((fand tests).(a!)) (adjacents a p)
  where tests = [walkable, diffH (a!p)]

diffH :: Tile -> Tile -> Bool
diffH (Tile h b w) (Tile h' b' w') = h /= h'

adjacents :: Array Position v -> Position -> [Position]
adjacents a p@(x,y) = filter (fand tests) $ range ((x-1,y-1),(x+1,y+1))
  where tests = [(/=p) ,inRange (bounds a)]

opmap :: [a -> b] -> a -> [b]
opmap [] _ = []
opmap (x:xs) y = x y : opmap xs y

fand xs = and.(opmap xs)


--Abstract graph and searches

type Graph n w = Array n [(n,w)]

adjacent :: (Ix n,Num w) => (Graph n w) -> n -> [n]
adjacent g n = if n `elem` nodes g 
               then map fst (g ! n)
                    else error "node not in graph"
                         
nodes :: (Ix n, Num w) => Graph n w -> [n]
nodes  = range.bounds 

edges :: (Ix n, Num w) => Graph n w -> [(n,n,w)]
edges g = [(i,j,w)| i<- nodes g, (j,w) <- filter ((<i).fst) (g!i)]
                       
edgeIn :: (Ix n, Num w) => Graph n w -> (n,n) -> Bool
edgeIn g x = case uncurry (weight g) x of 
  Just y -> True
  Nothing -> False

weight :: (Ix n, Num w) => Graph n w -> n -> n -> Maybe w
weight g n m = case filter ((==m).fst) $ g!n of
  [x] -> Just $ snd x
  [] -> Nothing

type GoalTest n = n -> Bool

depthFirstSearch :: (Ix n, Num w) => Graph n w -> n -> [n]
depthFirstSearch g s = dfs g [s] empty

dfs :: (Ix n, Num w) => Graph n w -> [n] -> Set n -> [n]
dfs g [] xs = toList xs
dfs g (x:xs) ys = if (member x ys)
                  then dfs g xs ys
                  else dfs g ((adjacent g x)++xs) (insert x ys) 
                       
--mapToGraph :: Floating a => Array Position Tile -> Graph Position a
--mapToGraph a 

d :: (Floating a) => Position -> Position -> a 
d (x,y) (z,w) =sqrt $ fromInteger (((x-z)^2) + ((y-w)^2))

