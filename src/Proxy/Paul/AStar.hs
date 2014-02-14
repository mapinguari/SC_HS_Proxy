{-# LANGUAGE TypeFamilies #-}

module Proxy.Paul.AStar where

import Data.Ix(Ix)
import qualified Data.PSQueue as PSQ
import qualified Data.Map as Map
import Control.Monad.State
import Proxy.Paul.Graph

type GoalTest a = a -> Bool
type Heuristic a w = a -> w
type Path a = [a]
data SearchResponse a = Ongoing | Stop | Returned a

--Need to remove the O(n) calls to find the cost of a path ie use data type Map n (n,w)

aStar :: (Ix n, Real w) => Graph n w -> Heuristic n w -> GoalTest n -> n -> Maybe (Path n)
aStar g h test s = case evalState (as' g h test) ((Map.singleton s (s,0)),(PSQ.singleton s 0)) of
                           Stop -> Nothing
                           Returned x -> Just x

as' :: (Ix n, Real w) => Graph n w -> Heuristic n w -> GoalTest n -> State (Map.Map n (n,w), PSQ.PSQ n w) (SearchResponse (Path n))
as' g h test = do let step = step' g h test
                  a <- step
                  case a of 
                        Stop -> return Stop
                        Ongoing -> as' g h test
                        Returned x -> return (Returned x)
         

step' :: (Ix n, Real w) => Graph n w -> Heuristic n w -> GoalTest n -> State (Map.Map n (n,w), PSQ.PSQ n w) (SearchResponse (Path n))
step' g h test  = state ( \ (routeMap,pq) -> case PSQ.minView pq of
                                                 Nothing -> (Stop, (routeMap, pq))
                                                 Just (a,pq0) -> case test (PSQ.key a) of
                                                                         True -> ( Returned completePath,(routeMap, pq))
                                                                         False -> ( Ongoing, newState)
                                                            where f i = [(j, sweight g i j)| j <- adjacent g i]
                                                                  newState = listPush g routeMap pq0 (PSQ.key a) (f (PSQ.key a)) h
                                                                  completePath = getPath routeMap (PSQ.key a))




                                                
listPush :: (Ix n, Real w) => Graph n w -> Map.Map n (n,w) -> PSQ.PSQ n w -> n -> [(n,w)] -> Heuristic n w -> (Map.Map n (n,w), PSQ.PSQ n w)
listPush g m pq c [] h = (m,pq)
listPush g m pq c (x:xs) h = listPush g newM newPQ c xs h
                           where (newM,newPQ) = push g m pq c x h

push :: (Ix n, Real w) => Graph n w -> Map.Map n (n,w) -> PSQ.PSQ n w -> n -> (n,w) -> Heuristic n w -> (Map.Map n (n,w), PSQ.PSQ n w)
push g m pq current (next,newEdgeWeight) h = case Map.lookup next m of
                           Nothing -> (newMap, newPSQ)
                           Just (a,w1) -> if oldCost > newCost
                                          then (newMap, newPSQ)
                                          else (m,pq)
                           where newMap = Map.insert next (current, newCost) m
                                 newPSQ = PSQ.insert next prio pq
                                 prio = snd((Map.!) newMap next) + h next
                                 oldCost = snd((Map.!) m next)
                                 newCost = snd((Map.!) m current) + newEdgeWeight

pathCost :: (Ix n, Real w) => Graph n w -> Path n -> w
pathCost g [] = 0
pathCost g [x] = 0
pathCost g [x,y] = sweight g x y 
pathCost g (x:y:xs) = sweight g x y 

getPath :: Ix n => Map.Map n (n,w) -> n -> Path n 
getPath routeMap end = gP routeMap end []
                       where gP m n xs = if fst((Map.!) m n) == n 
                                         then n:xs
                                         else gP m (fst((Map.!) m n)) (n:xs)
                                
                                

