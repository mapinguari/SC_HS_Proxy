module Proxy.PathFinding.AStarState where
import Proxy.Math.Graph
import Proxy.PathFinding.Specification
import Data.Monoid
import Control.Monad.State
import Data.Array
import Data.List
import Data.Function


first :: (a -> Bool) -> [a] -> Maybe a
first _ [] = Nothing
first f (x:xs) = if f x then Just x else first f xs


sp :: (Num b, Ord b, WeightedGraph g) => g a b -> Node -> Node -> Maybe (WeightedPath b)
sp g n1 n2 = first atDestination (build [mkWPath 0 [n1]])
  where atDestination = maybe False (== n2) . destination
        build [] = [] 
        build (x:xs) = x : build (mergeOn wpWeight (newPaths x) xs)
        newPaths wp = map (\ (w,m) -> addToWP w m wp) (sortBy (compare `on` fst) (adjaWeights g (safeDestination wp)))


sp3 :: (Num b, Ix b, WeightedGraph g) => g a b -> Node -> Node -> Maybe (WeightedPath b)
sp3 g n1 n2 = first atDestination (evalState (statefulBuild g) (LPS ([], [mkWPath 0 [n1]])))
  where atDestination = maybe False (== n2) . destination
        
statefulBuild :: (Num b, Ix b, WeightedGraph g, PathState ps) => g a b -> State (ps b) [WeightedPath b]
statefulBuild g = do 
  ps <- get
  if noPathsLeft ps
    then return [] 
    else do
    np <- newPath g
    wps <- statefulBuild g
    return (np : wps)

newPath :: (Num b, Ix b, WeightedGraph g, PathState ps) => g a b -> State (ps b) (WeightedPath b)
newPath g =  do
  p <- gets nextPath
  modify (addToVisited (safeDestination p))
  modify (updatePathsToConsider (const 0))
  return p



spAStar :: (Num b, Ix b, WeightedGraph g) => g a b -> Heuristic b -> Node -> Node -> Maybe (WeightedPath b)
spAStar g h n1 n2 = first atDestination $ aStar g n1 h
  where atDestination = maybe False (== n2) . destination
  

aStar :: (Num b, Ix b, WeightedGraph g) => g a b -> Node -> Heuristic b -> [WeightedPath b]
aStar g n h = evalState (sb g h) (LPS ([], [mkWPath 0 [n]]))


sb :: (Num b, Ix b, WeightedGraph g, PathState ps) => g a b -> Heuristic b -> State (ps b) [WeightedPath b]
sb g h = do 
  ps <- get
  if noPathsLeft ps
    then return [] 
    else do
    np <- newPath' g h
    wps <- sb g h
    return (np : wps)
  
newPath' :: (Num b, Ix b, WeightedGraph g, PathState ps) => g a b -> Heuristic b -> State (ps b) (WeightedPath b)
newPath' g h = do
  p <- gets nextPath
  modify (addToVisited (safeDestination p))
  modify (updatePathsToConsider h)
  return p



spAAStar :: (Num b, Ix b, WeightedGraph g) => g a b -> [WeightedPath b] -> WeightedPath b -> Heuristic b -> Node -> Maybe (WeightedPath b)
spAAStar g cn p h n = first (flip isOnPath p . safeDestination) (aStar g n eH) >>= return.(\ s -> s <> pathFrom g (safeDestination s) p)
 where eH = (heurMap!)
       heurMap = accumArray second 0 (0,size g - 1) ([(i, h i) | i <- nodes g] ++ [(safeDestination q, wpWeight p - wpWeight q) | q <- cn ])       
       second _ x = x

          
floodfill :: (Node -> [Node]) -> (Node -> Bool) -> Node -> [Node]
floodfill f t n = ff [n]
  where ff visited = let nexts = filter test . concatMap f $ visited 
                         test x = (not (x `elem` visited) && t x) in
                     if null nexts 
                     then visited
                     else ff (nexts ++ visited)

