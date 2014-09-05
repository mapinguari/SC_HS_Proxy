module Proxy.PathFinding.FloydWarshall where 
import Proxy.Math.Graph
import Data.Array
import Data.Monoid
import Control.Monad
import Proxy.PathFinding.Specification
import Proxy.Math.InfNumber

sp :: (WeightedGraph g b) => g a b -> Node -> Node -> Maybe Path
sp g o d = floWar g ! (o,d,0)

shortestPaths ::  (WeightedGraph g b) => g a b -> [(Node,Node)] -> [Maybe Path]
shortestPaths g = map ((allPaths !) . (\(x,y) -> (x,y,0))) 
  where allPaths = floWar g
        
floWar :: (WeightedGraph g b) => g a b -> Array (Node,Node,Node) (Maybe Path)
floWar g = theArray
  where theArray = array ((0,0,0),(s - 1,s - 1,s)) (initialized ++ recursive)
        s = size g
        ns = nodes g
        cost = weight g
        initialized = [((i,j,s), f (i,j)) | 
                       i <- ns, 
                       j <- ns, 
                       let f (x,y) = if arc g x y 
                                     then return . mkPath $ [x,y]
                                     else Nothing]
        recursive = [((i,j,k),p) | 
                     i <- ns, 
                     j <- ns, 
                     k <- ns, 
                     let p = minBy (liftM (pc g)) (theArray ! (i,j,k+1)) (liftM2 (+|+) (theArray ! (i,k,k+1)) (theArray ! (k,j,k+1)))]

floWarEf :: (WeightedGraph g b) => g a b -> Array (Node,Node,Node) (InfNumbers b,Maybe Node)
floWarEf g = theArray
  where theArray = array ((0,0,0),(s - 1,s - 1,s)) (initialized ++ recursive)
        s = size g
        ns = nodes g
        cost = weight g
        initialized = [((i,j,s), f (i,j)) | 
                       i <- ns, 
                       j <- ns, 
                       let f (x,y)
                             | x == y = (F 0,Nothing)
                             | arc g x y = (F (weight g (x,y)), Just y)
                             | otherwise = (Inf, Nothing)]
        recursive = [((i,j,k),p) | 
                     i <- ns, 
                     j <- ns, 
                     k <- ns, 
                     let p = if fst (theArray ! (i,k,k+1)) + fst (theArray ! (k,j,k+1)) < fst (theArray ! (i,j,k+1))
                             then (fst (theArray ! (i,k,k+1)) + fst (theArray ! (k,j,k+1)), snd (theArray ! (i,k,k+1)))
                             else theArray ! (i,j,k+1)]
                    
        

minBy :: (Ord b) => (a -> b) -> a -> a -> a 
minBy f x y = if f x <= f y
              then x
              else y
                   
sp' :: (WeightedGraph g b) => g a b -> Node -> Node -> Maybe Path
sp' g o d = liftM mkPath . path o d . floWarEf $ g
                   
path :: (Real b) => Node -> Node -> Array (Node,Node,Node) (InfNumbers b,Maybe Node) -> Maybe [Node]
path n m a 
  | n == m = Just [n] 
  | otherwise = snd (a ! (n,m,0)) >>= (\s -> path s m a) >>= return . (n :)

                   
                   
{-
shortestPathD :: (WeightedGraph g b) => g a b -> Node -> Node -> Maybe b
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
                    
shortestPaths :: (WeightedGraph g b) => g a b -> [(Node, Node)] -> [Maybe (WeightedPath b)]
shortestPaths g  = map (getLeast . (pathBuilder !) . f) 
  where pathBuilder = floWar g
        f (x,y) = (x,y,0)

floWar :: (WeightedGraph g b) => g a b -> Array (Node,Node,Node) (Least (WeightedPath b))
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
                    
shortestWeightedPath :: (WeightedGraph g b) => g a b -> Node -> Node -> Maybe (WeightedPath b)
shortestWeightedPath g n1 n2 = getLeast (pathBuilder ! (n1,n2,0))
  where pathBuilder = floWar g
-}