module Proxy.PathFinding.FloydWarshall where 
import Proxy.Math.Graph
import Data.Array
import Data.Monoid
import Control.Monad
import Proxy.PathFinding.Specification

shortestPathD :: (WeightedGraph g, Num b) => g a b -> Node -> Node -> Maybe b
shortestPathD g n1 n2 = getLeast (floWar ! (n1,n2,0))
  where floWar = array ((0,0,0),(s - 1,s - 1,s)) (initialized ++ recursive)
        s = size g
        ns = nodes g
        cost = weight g
        initialized = [((i,j,s), Least $ cost (i,j) ) | i <- ns , j <- ns]
        recursive = [((i,j,k),w) | 
                     i <- ns, 
                     j <- ns, 
                     k <- ns, 
                     let w = (floWar ! (i,j,k+1)) <> (liftM2 (+) (floWar ! (i,k,k+1)) (floWar ! (k,j,k+1)))]
                    
shortestPaths :: (WeightedGraph g) => g a b -> [(Node, Node)] -> [Maybe (WeightedPath b)]
shortestPaths g  = map (getLeast . (pathBuilder !) . f) 
  where pathBuilder = floWar g
        f (x,y) = (x,y,0)

floWar :: (WeightedGraph g, Num b) => g a b -> Array (Node,Node,Node) (Least (WeightedPath b))
floWar g = theArray
  where theArray = array ((0,0,0),(s - 1,s - 1,s)) (initialized ++ recursive)
        s = size g
        ns = nodes g
        cost = weight g
        initialized = [((i,j,s), f (i,j)) | 
                       i <- ns, 
                       j <- ns, 
                       let f (x,y) = Least $ case cost (i,j) of  
                             Nothing -> Nothing
                             Just a -> if a == 0 
                                       then Just $ WP a $ Path [i]
                                       else Just $ WP a $ Path [i,j]]
        recursive = [((i,j,k),w) | 
                     i <- ns, 
                     j <- ns, 
                     k <- ns, 
                     let w = (theArray ! (i,j,k+1)) <> (liftM2 (<>) (theArray ! (i,k,k+1)) (theArray ! (k,j,k+1)))]
                    
shortestWeightedPath :: (WeightedGraph g, Num b) => g a b -> Node -> Node -> Maybe (WeightedPath b)
shortestWeightedPath g n1 n2 = getLeast (pathBuilder ! (n1,n2,0))
  where pathBuilder = floWar g