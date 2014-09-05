module Proxy.PathFinding.Dynamic where 
import Proxy.PathFinding.Specification
import Proxy.Math.Graph
import Proxy.PathFinding.HinzAStar
import Control.Monad
import Data.Ix
import Data.List

dsp :: (WeightedGraph g b) => [g a b] -> [Node] -> Node -> Heuristic b -> [Maybe Path]
dsp [] _ _ _ = []
dsp _ [] _ _ = []
dsp (g:gs) (n:ns) d h = aStarSearch g n d h : dsp gs ns d h 

dsp' :: (WeightedGraph g b) => [g a b] -> [Node] -> Node -> Heuristic b -> [Maybe Path]
dsp' [] _ _ _ = []
dsp' _ [] _ _ = []
dsp' 

dataset :: (WeightedGraph g b) => g a b -> Node -> Map Node (InfNumbers b,InfNumbers b)
dataset g n = e

f :: Map Node (InfNumbers b,InfNumbers b) -> 

m fromList [(i,(Inf,Inf))| i <- nodes g] 

rhs :: (WeightedGraph g b) => g a b 

sameCostPath :: (WeightedGraph g b) => g a b -> (Path,b) -> Bool
sameCostPath g (p,c) = pc g p == c

{-
type GoalTest = Node -> Bool

dynPathF :: (WeightedGraph g, LabelledGraph g, Num b, Ix b) => [g a b] -> Heuristic b -> [Node] -> Node -> [Maybe Path]

dynPathF (g:gs) h (n1:ns) n2 = liftM wpPath (spAStar g h n1 n2) : dynPathF gs h ns n2

pathBlocked :: (LabelledGraph g) => g Bool a -> Path -> Bool
pathBlocked g p = Fold.all (not.label g) (rep p)

dynPathF2 :: (WeightedGraph g, LabelledGraph g, Num b, Ix b) => [g Bool b] -> Heuristic b -> [Node] -> Node -> [Maybe Path]
dynPathF2 (g:gs) h (n1:ns) n2 = firstPath : dpf gs h ns n2 firstPath
  where firstPath = liftM wpPath (spAStar g h n1 n2)
        dpf (g:gs) h (n1:ns) n2 p = let newPath = (return . wpPath) =<< spAStar g h n1 n2 in 
          if maybe True (pathBlocked g) p
          then newPath : dpf gs h ns n2 newPath
          else p : dpf gs h ns n2 p
               
takeUntilGoal :: [WeightedPath b] -> GoalTest -> Maybe ([(Node,b)], WeightedPath b)
takeUntilGoal wps gt = if null b 
                       then Nothing
                       else Just (getClosed a, head b)
  where (a,b) = partition (not.gt.safeDestination) wps
        

getClosed :: [WeightedPath b] -> [(Node,b)] 
getClosed = map (\ wp -> (safeDestination wp, wpWeight wp))

dynPathF3 :: (WeightedGraph g, LabelledGraph g, Num b, Ix b) => [g Bool b] -> Heuristic b -> [Node] -> Node -> [Maybe Path]
dynPathF3 (g:gs) h (n1:ns) n2 = dpf gs h ns n2 Nothing
  where dpf (g:gs) h (n:ns) n2 x = case x of 
          Nothing -> getPath pathData : dpf gs h ns n2 pathData
          Just (_,p) -> if pathBlocked g (wpPath p)
                        then getPath pathData : dpf gs h ns n2 pathData
                        else (return.wpPath $ p) : dpf gs h ns n2 x
          where pathData = takeUntilGoal (newPath g h n x) (== n2)
                newPath g h n cs = case cs of
                  Nothing -> aStar g n h
                  Just (cs,path) -> aAStar g n cs path h
                getPath p = p >>= (return.wpPath.snd)

  
-}