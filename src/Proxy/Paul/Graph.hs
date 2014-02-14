module Proxy.Paul.Graph where

import Data.Array
import Proxy.Game
import qualified Data.Set as S

type Graph n w = Array n [(n,w)]

--Monoid w ? does that work?

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

sweight :: (Ix n, Num w) => Graph n w -> n -> n -> w
sweight g n m = case weight g n m of
   Just a -> a 
   Nothing -> error "edge not in graph, no weight"

depthFirstSearch :: (Ix n, Num w) => Graph n w -> n -> [n]
depthFirstSearch g s = dfs g [s] S.empty

dfs :: (Ix n, Num w) => Graph n w -> [n] -> S.Set n -> [n]
dfs g [] xs = S.toList xs
dfs g (x:xs) ys = if (S.member x ys)
                  then dfs g xs ys
                  else dfs g ((adjacent g x)++xs) (S.insert x ys) 