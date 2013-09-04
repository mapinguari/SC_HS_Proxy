{-# LANGUAGE TypeFamilies #-}

module Proxy.Paul.AStar where

import Proxy.Game(Tile(..), walkable)
import Data.Array
import Data.Set(Set(),empty,member,toList,insert)
import qualified Data.PSQueue as PSQ
import qualified Data.Map as Map
import Debug.Trace(trace)

--Find tiles adjacent to tiles in other nodes.

type Position = (Integer,Integer)
type Edge = (Position,Position)

onBorder :: Array Position Tile -> Position -> [] Position
onBorder a p = filter ((fand tests).(a!)) (neighbours a p)
  where tests = [walkable, diffH (a!p)]

diffH :: Tile -> Tile -> Bool
diffH (Tile h b w) (Tile h' b' w') = h /= h'

neighbours :: Array Position v -> Position -> [Position]
neighbours a p@(x,y) = filter (fand tests) $ range ((x-1,y-1),(x+1,y+1))
  where tests = [(/=p) ,inRange (bounds a)]

adjacents :: Array Position Tile -> Position -> [Position]
adjacents a p = if walkable (a!p)
          then filter (walkable.(a!)) (neighbours a p)
          else []

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

type GoalTest a = a -> Bool
type Heuristic a w = a -> w
type Path a = [a]

depthFirstSearch :: (Ix n, Num w) => Graph n w -> n -> [n]
depthFirstSearch g s = dfs g [s] empty

dfs :: (Ix n, Num w) => Graph n w -> [n] -> Set n -> [n]
dfs g [] xs = toList xs
dfs g (x:xs) ys = if (member x ys)
                  then dfs g xs ys
                  else dfs g ((adjacent g x)++xs) (insert x ys) 
                       
mapToGraph :: Floating a => Array Position Tile -> Graph Position a
mapToGraph a = array (bounds a) [(i,[(j,d j i)| j <- (adjacents a i)])| i <- (range.bounds $ a)]

d :: (Floating a) => Position -> Position -> a 
d (x,y) (z,w) =sqrt $ fromInteger (((x-z)^2) + ((y-w)^2))

aStar :: (Ix n,Num w, Ord w) => Graph n w -> Heuristic n w -> GoalTest n -> n -> Maybe (Path n)
aStar g h goal start = as' g h goal (Map.singleton start start) (PSQ.singleton start 0)

as' :: (Ix n,Num w, Ord w) => Graph n w -> Heuristic n w -> GoalTest n -> Map.Map n n -> PSQ.PSQ n w -> Maybe (Path n)
as' g h test routeMap pq = case PSQ.minView pq of
                                         Nothing -> Nothing
                                         Just (a,pq0) -> case test (PSQ.key a) of
                                                            True -> Just $ getPath routeMap (PSQ.key a)
                                                            False -> as' g h test newMap newPSQ
                                                            where f i = [(j,(\ x -> case x of
                                                                                      Nothing -> (fromInteger 0)
                                                                                      Just a -> a )(weight g i j))| j <- adjacent g i]
                                                                  (newMap,newPSQ) = listPush g routeMap pq0 (PSQ.key a) (f (PSQ.key a)) h
                  

                                                
listPush :: (Ix n, Ord w, Num w) => Graph n w -> Map.Map n n -> PSQ.PSQ n w -> n -> [(n,w)] -> Heuristic n w -> (Map.Map n n, PSQ.PSQ n w)
listPush g m pq c [] h = (m,pq)
listPush g m pq c (x:xs) h = listPush g newM newPQ c xs h
                           where (newM,newPQ) = push g m pq c x h

push :: (Ix n, Num w, Ord w) => Graph n w -> Map.Map n n -> PSQ.PSQ n w -> n -> (n,w) -> Heuristic n w -> (Map.Map n n, PSQ.PSQ n w)
push g m pq c (n,w) h = case Map.lookup n m of
                           Nothing -> (newMap, PSQ.insert n prio pq)
                           Just a -> if pathCost g (getPath m n) > (pathCost g (getPath m c)) + w 
                                     then (newMap, PSQ.insert n prio pq)
                                     else (m,pq)
                           where prio = pathCost g (getPath newMap n) + h n
                                 newMap = Map.insert n c m

pathCost :: (Ix n, Num w) => Graph n w -> Path n -> w
pathCost g [] = 0
pathCost g [x] = 0
pathCost g [x,y] = case weight g x y of
                        Nothing -> 0
                        Just a -> a
pathCost g (x:y:xs) = case weight g x y of 
                           Nothing -> pathCost g (y:xs)
                           Just a -> a + pathCost g (y:xs)

getPath :: Ix n => Map.Map n n -> n -> Path n 
getPath routeMap end = gP routeMap end []

gP :: Ix n => Map.Map n n -> n -> Path n -> Path n
gP m n xs = if (Map.!) m n == n 
            then n:xs
            else gP m ((Map.!) m n) (n:xs)
