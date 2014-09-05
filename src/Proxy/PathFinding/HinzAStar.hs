module Proxy.PathFinding.HinzAStar where
import Data.List as L
import Data.PSQueue hiding (empty,singleton)
import Proxy.Math.Graph
import Control.Monad.State
import Control.Monad(liftM)
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace (trace)
import Proxy.Math.InfNumber
import qualified Data.Map.Lazy as M
import Proxy.PathFinding.Specification 

type Label = (Node , Maybe Node)
type DLabel b = (Label,b)
type Metric b = Node -> Node -> b
type LookAhead = Int
type G b = InfNumbers b
type RHS b = InfNumbers b

{-
sSastar :: (WeightedGraph g b, Real b) => g a b -> Node -> Node -> Heuristic b -> Maybe Path
sSastar g o d h = (dfs d (shortestPathTree (takeUntilIncluding ((==d).fst) (astarF' g o (h d)))))
-}

aStarSearch :: (WeightedGraph g b, Real b) => g a b -> Node -> Node -> Heuristic b -> Maybe Path
aStarSearch g o d h = pathFinderF o d succMap
  where succMap = M.fromList ((takeUntilIncluding ((==d).fst) list))
        list = astarF' g o h
        
moaStarSearch :: (WeightedGraph g b, Real b) => g a b -> [Node] -> Node -> [Heuristic b] -> [Maybe Path]
moaStarSearch g os d hs = map (\o -> pathFinderB o d parentsMap) os
  where _H x = minimum $ map ($ x) hs
        parentsMap = M.fromList $ takeUntilReachedAll fst os (astarB' g d _H)
        
takeUntilReachedAll :: (Ord a) => (b -> a) -> [a] -> [b] -> [b]
takeUntilReachedAll f gs ls = tur (S.fromList gs) ls
  where tur _ [] = []
        tur s (l:ls)  
          | S.null s = [] 
          | f l `S.member` s = l : tur (S.delete (f l) s) ls
          | otherwise = l : tur s ls

mgaStarSearch :: (WeightedGraph g b, Real b) => g a b -> Node -> [Node] -> [Heuristic b] -> [Maybe Path]
mgaStarSearch g o ds hs = map (\d -> pathFinderF o d parentsMap) ds
  where _H x = minimum $ map ($x) hs
        parentsMap = M.fromList $ astarF' g o _H
  
pathFinder :: (Path -> Node -> Path) ->  Node -> Node -> M.Map Node (Maybe Node) -> Maybe Path
pathFinder dir root leaf m 
  | root == leaf = Just $ mkPath []
  | otherwise = M.lookup leaf m >> (return (getPath dir leaf m))
          
pathFinderF :: Node -> Node -> M.Map Node (Maybe Node) -> Maybe Path
pathFinderF = pathFinder advance               

pathFinderB :: Node -> Node -> M.Map Node (Maybe Node) -> Maybe Path
pathFinderB o d m = pathFinder retreat d o m
  
        
getPath :: (Path -> Node -> Path) -> Node -> M.Map Node (Maybe Node) -> Path
getPath dir c m = case join $ M.lookup c m of
  Nothing -> mkPath [c]
  Just p -> dir (getPath dir p m) c
  
astarF' :: (WeightedGraph g b, Real b) => g a b -> Node -> Heuristic b -> [(Node,Maybe Node)]
astarF' g s h = loop (adjaWeights g) h (decrease (s,(F 0,0,Nothing)) q0)
  where q0 = fromList [v :-> (Inf,0,Nothing) | v <- nodes g]

astarB' :: (WeightedGraph g b, Real b) => g a b -> Node -> Heuristic b -> [(Node,Maybe Node)]        
astarB' g s h = loop (stneWeights g) h (decrease (s,(F 0,0,Nothing)) q0)
  where q0 = fromList [v :-> (Inf,0,Nothing) | v <- nodes g]
        
loop :: (Real b) => (Node -> [(b,Node)]) -> Heuristic b -> PSQ Node (InfNumbers b, b, Maybe Node) -> [Label]
loop suc h q = case minView q of 
  Nothing -> []
  Just (b,q1) -> (c,p) : loop suc h (decreaseList bs q1)
    where bs = [(v,(d + F ( w + h v - h c), g - w,Just c))| (w,v) <- suc c]
          c = key b 
          (d,g,p) = prio b


decrease :: (Ord p,Ord k) => (k,p) -> PSQ k p -> PSQ k p
decrease (k,p) q = adjust (min p) k q

decreaseList :: (Ord p,Ord k) => [(k,p)] -> PSQ k p -> PSQ k p
decreaseList bs q = L.foldr decrease q bs


naieve :: (WeightedGraph g b, Real b) => g a b -> [Node] -> Node -> [Heuristic b] -> [Maybe Path]
naieve g os d hs = map (\(o,h) -> aStarSearch g o d h) (zip os hs)



rtaStarSearch :: (WeightedGraph g b, Real b) => g a b -> LookAhead -> Node -> Node -> Heuristic b -> Maybe Path
rtaStarSearch g l o d h = pathFinderF o best succsMap
    where succs = take l ((takeUntilIncluding ((==d).fst) (astarF' g o h)))
          succsMap = M.fromList $ succs
          best = fst . last $ succs
 
          
takeUntilIncluding :: (a -> Bool) -> [a] -> [a]
takeUntilIncluding f xs = case bs of
  (b : bs') -> as ++ [b]
  otherwise -> as
  where (as,bs) = span (not.f) xs
                           
astarF'' :: (WeightedGraph g b, Real b) => g a b -> Node -> Heuristic b -> [(Node,Maybe Node)]
astarF'' g s h = carryLoop (adjaWeights g) h (decrease (s,(F 0, 0,Nothing)) q0)
  where q0 = fromList [v :-> (Inf,0,Nothing) | v <- nodes g]
        
carryLoop :: (Real b) => (Node -> [(b,Node)]) -> Heuristic b -> PSQ Node (InfNumbers b, b, Maybe Node) -> [(Node, Maybe Node)]
carryLoop suc h q = case minView q of 
  Nothing -> []
  Just (b,q1) -> (c,p) : carryLoop suc h (decreaseList bs q1)
    where bs = [(v,(d + F ( w + h v - h c), g - w ,Just c))| (w,v) <- suc c]
          c = key b 
          (d,g,p) = prio b

cLoop :: (Real b) => (Node -> [(b,Node)]) -> Heuristic b -> PSQ Node (InfNumbers b, b, Maybe Node) -> [(Node, InfNumbers b)]
cLoop suc h q = case minView q of 
  Nothing -> []
  Just (b,q1) -> (c,F (negate g)) : cLoop suc h (decreaseList bs q1)
    where bs = [(v,(d + F ( w + h v - h c), g - w ,Just c))| (w,v) <- suc c]
          c = key b 
          (d,g,p) = prio b
          

        
