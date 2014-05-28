module Proxy.AStar where
import Data.Graph.Inductive.Graph 
import Data.Graph.Inductive.Internal.RootPath
import Data.Graph.Inductive.PatriciaTree
import Debug.Trace
import qualified Data.Graph.Inductive.Internal.Heap as H

type Heuristic a b = a -> b 
type GoalTest = Node -> Bool

aStar :: (Graph gr, Num b, Ord b) => Heuristic a b -> H.Heap b (LPath b) -> gr a b -> LRTree b
aStar _ heap g | H.isEmpty heap || isEmpty g = []
aStar h heap g = case H.splitMin heap of
  (a,pth@(LP ((n,_):ps)),heap') -> case match n g of 
    (Nothing,g) -> aStar h heap' g
    (Just (_,_,_,sucs),g') -> pth : aStar h expandedHeap g' 
      where expandedHeap = H.mergeAll $ heap' : extend h pth sucNodes
            sucNodes = foldr (\(ac,n) xs-> case lab g' n of
                                 Nothing -> xs
                                 Just l -> (n,l,ac):xs) [] sucs
            
extend :: (Ord b, Num b) => Heuristic a b -> LPath b -> [(Node,a,b)] -> [H.Heap b (LPath b)]
extend h (LP path@((_,c):p)) = map (\(sucN,nodeLab,arcCost) -> H.unit (c+arcCost+(h nodeLab)) (LP ((sucN,c+arcCost):path)))

aStar_ter :: (Graph gr, Num b, Ord b)=> Heuristic a b -> gr a b -> Node -> LRTree b  
aStar_ter h g o = aStar h (H.unit 0 (LP [(o,0)])) g 

first :: (a-> Bool) -> [a] -> a
first f = head.filter f

strip :: LPath a -> [(Node,a)]
strip (LP x) = x

shortestPath :: (Graph gr, Num b, Ord b) => Heuristic a b -> gr a b -> Node -> GoalTest -> Path
shortestPath h graph origin goalTest = reverse.(map fst).strip.(first (\(LP ((n,_):_)) -> goalTest n)) $ aStar_ter h graph origin

