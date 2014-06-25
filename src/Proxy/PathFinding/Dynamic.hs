module Proxy.PathFinding.Dynamic where 
import Proxy.PathFinding.Specification
import Proxy.PathFinding.AStarState
import Proxy.Math.Graph
import Control.Monad
import Data.Ix
import Data.Foldable as Fold

dynPathF :: (WeightedGraph g, LabelledGraph g, Num b, Ix b) => [g a b] -> Heuristic b -> [Node] -> Node -> [Maybe Path]
dynPathF2 :: (WeightedGraph g, LabelledGraph g, Num b, Ix b) => [g Bool b] -> Heuristic b -> [Node] -> Node -> [Maybe Path]

dynPathF (g:gs) h (n1:ns) n2 = liftM wpPath (spAStar g h n1 n2) : dynPathF gs h ns n2

pathBlocked :: (LabelledGraph g) => g Bool a -> Maybe Path -> Bool
pathBlocked g Nothing = True
pathBlocked g (Just p) = Fold.all (not.label g) (rep p)

dynPathF2 (g:gs) h (n1:ns) n2 = firstPath : dpf gs h ns n2 firstPath
  where firstPath = liftM wpPath (spAStar g h n1 n2)
        dpf (g:gs) h (n1:ns) n2 p = let newPath = (return . wpPath) =<< spAStar g h n1 n2 in 
          if pathBlocked g p
          then newPath : dpf gs h ns n2 newPath
          else p : dpf gs h ns n2 p


{-
dynamicPathFinding gs h nc n2 = case  of
  Nothing -> []
  Just path -> path : dpf gs h  firstPath
  where (a,b) = span (not.atDestination) (spAStar (head gs) h (head nc))
        firstPath = 

dpf :: (WeightedGraph g, LabelledGraph g, Num b, Ord b) => [g a b] -> Heuristic b -> -> [WeightedPath b] -> WeightedPath b -> Path
dpf (g:gs) h (c:nc) cs p 
  | c == origin p = dpf gs h nc (remainder p)           
  | blockedPath p = case spAAStar g cs p h (destination p) of
    Nothing -> []
    Just wp ->

-}