module Proxy.Paul.Graph where

import Data.Array
import qualified Data.List as List (sort)

type Node = Int
type Distance = Double

data Graph = Graph {adj :: Node -> [Node], size :: Int}

gFSAL :: [[(Node,Distance)]] -> Graph
gFSAL xs = let adjmap n = if n+1 <= length xs
                          then xs !! (n-1)
                          else error "Node not in array" in Graph {adjmap, length xs}
                  
gFAL :: [[Node]] -> Graph
gFAL xs = let ys = map List.sort xs in gFSAL ys

gFAM :: Array (Node,Node) Double -> Graph
gFAM a = let bS = bounds a
             ys = [[j|j<-[(snd.fst) bS .. (snd.snd) bS], a!(i,j) == True]| i<-[(fst.fst) bS .. (fst.snd) bS]] in gFSAL ys

g1 = gFAM $ listArray ((1,1),(3,3)) $ map f "011101110"
     where f :: Char -> Bool
           f n
             | n == '0' = False
             | otherwise = True

size :: Graph -> Int
size g 

edges :: Graph -> [(Node,Node)]
edges g = [ map ((,) i) (takeWhile (>= i) $ g i) | i <- size g]
