module Hope where
import Data.List as L
import Data.PSQueue hiding (empty,singleton)
import Proxy.Math.Graph
import Control.Monad.State
import Control.Monad(liftM)
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace (trace)
import Proxy.Math.InfNumber
import qualified Data.Map as M
import Proxy.PathFinding.Specification 

type Label = (Node , Maybe Node)
type DLabel b = (Label,b)
type Metric b = Node -> Node -> b

{-
sSastar :: (WeightedGraph g b, Real b) => g a b -> Node -> Node -> Heuristic b -> Maybe Path
sSastar g o d h = (dfs d (shortestPathTree (takeUntilIncluding ((==d).fst) (astarF' g o (h d)))))
-}

aStarSearch :: (WeightedGraph g b, Real b) => g a b -> Node -> Node -> Heuristic b -> Maybe Path
aStarSearch g o d h = pathFinderF o d succMap
  where succMap = M.fromList $ astarF' g o h
        
moaStarSearch :: (WeightedGraph g b, Real b) => g a b -> [Node] -> Node -> [Heuristic b] -> [Maybe Path]
moaStarSearch g os d hs = map (\o -> pathFinderB o d parentsMap) os
  where _H x = minimum $ map ($ x) hs
        parentsMap = M.fromList $ astarB' g d _H

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
astarF' g s h = loop (adjaWeights g) h (decrease (s,(F 0,Nothing)) q0)
  where q0 = fromList [v :-> (Inf,Nothing) | v <- nodes g]

astarB' :: (WeightedGraph g b, Real b) => g a b -> Node -> Heuristic b -> [(Node,Maybe Node)]        
astarB' g s h = loop (stneWeights g) h (decrease (s,(F 0,Nothing)) q0)
  where q0 = fromList [v :-> (Inf,Nothing) | v <- nodes g]
        
loop :: (Real b) => (Node -> [(b,Node)]) -> Heuristic b -> PSQ Node (InfNumbers b, Maybe Node) -> [Label]
loop suc h q = case minView q of 
  Nothing -> []
  Just (b,q1) -> (c,p) : loop suc h (decreaseList bs q1)
    where bs = [(v,(d + F ( w + h v - h c),Just c))| (w,v) <- suc c]
          c = key b 
          (d,p) = prio b


decrease :: (Ord p,Ord k) => (k,p) -> PSQ k p -> PSQ k p
decrease (k,p) q = adjust (min p) k q

decreaseList :: (Ord p,Ord k) => [(k,p)] -> PSQ k p -> PSQ k p
decreaseList bs q = L.foldr decrease q bs


naieve :: (WeightedGraph g b, Real b) => g a b -> [Node] -> Node -> [Heuristic b] -> [Maybe Path]
naieve g os d hs = map (\(o,h) -> aStarSearch g o d h) (zip os hs)
{-

sSastar :: (WeightedGraph g b, Real b) => g a b -> Node -> Node -> Heuristic b -> Maybe Path
sSastar g o d h = arrived (astarF' g o (h d)) d Empty

arrived :: [Label] -> Node -> Tree Node -> Maybe Path
arrived [] _ _ = Nothing
arrived (l:ls) n t = if fst l == n
                  then liftM mkPath $ dfs n newTree
                  else arrived ls n newTree
  where newTree = addLabel l t


rtaStar :: (WeightedGraph g b, Real b) => Int -> g a b -> Node -> Node -> Heuristic b -> Maybe Path                                  
rtaStar n g o d h = liftM mkPath $ dfs best (shortestPathTree pathlabels)
  where pathlabels = takeUntilIncluding ((==d).fst) (take n (astarF' g o (h d)))
        best = fst.last $ pathlabels


mSaStar :: (WeightedGraph g b, Real b) => g a b -> [Node] -> Node -> Heuristic b -> [Maybe Path]
mSaStar g os d h = map (liftM mkPath) $ dfsM (addLabels (takeUntilReachedAll fst os (astarB' g d (_H))) Empty) os
  where _H x = minimum $ map (h x) os
        

ALL TREE BASED F-ALGEBRA STUFF

firstLabel :: (Num b) => Node -> DLabel b
firstLabel n = ((n,Nothing),0)

shortestPathTree :: [Label] -> Tree Node
shortestPathTree ((r,Nothing) : ls) = addLabels ls (singleton r)
shortestPathTree _ = error "Not a rooted list!"

data Tree a = Node a (Forest a) | Empty
            deriving Show
type Forest a = [Tree a]

instance Functor Tree where
  fmap f (Node a ts) = Node (f a) (map (fmap f) ts)
  fmap f Empty = Empty
  
cata :: (a -> [b] -> b) -> b -> Tree a -> b
cata comb = para g
  where g a _ bs = comb a bs

        
para :: (a -> [Tree a] -> [b] -> b) -> b -> Tree a -> b
para _ base Empty = base
para comb base (Node a ts) = comb a ts (map rec ts)                
  where rec = para comb base

empty :: Tree a -> Bool
empty Empty = True
empty _ = False

tidyTree :: Tree a -> Tree a
tidyTree (Node a ts) = Node a (filter (not.empty) ts) 

singleton :: Node -> Tree Node 
singleton n = Node n []

insertT :: (a -> a -> Bool) -> a -> Tree a -> Tree a 
insertT f a = para (\ b ts bs -> if f b a 
                                then Node b (ts ++ [Node a []])
                                else Node b bs) (Node a [])
             
addLabels :: [Label] -> Tree Node -> Tree Node
addLabels xs t = L.foldl (flip addLabel) t xs 

addLabel :: Label -> Tree Node -> Tree Node
addLabel (c,Nothing) Empty = Node c []
addLabel (_,Nothing) t = t
addLabel (c,Just p) t = para (\a ts bs -> if p == a 
                                          then Node a (ts ++ [singleton c])
                                          else Node a bs) Empty t

allNodes :: Tree a -> [a]
allNodes = cata (\ a xs -> a : concat xs) []

subTreeRemoved :: Node -> Tree Node -> Tree Node 
subTreeRemoved n = para (\ a ts bs -> if a == n 
                                      then Node a ts
                                      else L.foldr notEmpty Empty bs)
                   Empty
  where notEmpty Empty x = x
        notEmpty t _ = t
        
removeSubTree :: Node -> Tree Node -> Tree Node
removeSubTree n = cata (\ a ts -> if a == n then Empty else (tidyTree (Node a ts))) Empty



starst :: Node -> Tree Node -> ([Node],Tree Node)
starst n t = (allNodes $ subTreeRemoved n t, removeSubTree n t)
                          
newOpen :: [Node] -> Tree Node -> ([Node], Tree Node) 
newOpen ns = runState (newOpen' ns)

newOpen' :: [Node] -> State (Tree Node) [Node]
newOpen' (x:xs) = do
  ns <- state (starst x)
  nss <- newOpen' xs
  return (ns ++ nss)
                                 
dfs :: (Eq a) => a -> Tree a -> Maybe [a]
dfs b = cata g Nothing
  where g a ts
          | a == b = Just [a]
          | L.null as = Nothing
          | otherwise = head as >>= return . (a :)
          where as = filter isJust ts
        
dfsM :: (Eq a) => Tree a -> [a] -> [Maybe [a]]
dfsM t = map (flip dfs t) 

takeUntilIncluding :: (a -> Bool) -> [a] -> [a]
takeUntilIncluding f xs = case bs of
  (b : bs') -> as ++ [b]
  otherwise -> as
  where (as,bs) = span (not.f) xs
  
takeUntilReachedAll :: (Ord a) => (b -> a) -> [a] -> [b] -> [b]
takeUntilReachedAll f gs ls = tur (S.fromList gs) ls
  where tur _ [] = []
        tur s (l:ls)  
          | S.null s = [] 
          | f l `S.member` s = l : tur (S.delete (f l) s) ls
          | otherwise = l : tur s ls
       
-}



















{-
daStar :: (MutableGraph g, WeightedGraph g b, Real b) => g a b -> ([GraphChanges],[Node]) -> Node -> Heuristic b -> [Maybe Path]
daStar g ss g h 

dastar' :: (WeightedGraph g b, Real b) => g a b -> Node -> ParHeur b -> [Node] -> ([Label],[Node])
dastar' 


makeTree :: [Node] -> Tree Node -> ([Label],Tree Node)
makeTree ns t = loop g h (decrease (s,(F 0,Nothing)) q0)
  where E
        
oldTree + Changes = (presentTree,[Nodes] to be recalculated)



getPathList :: [(Node,InfNumbers b)] -> (Node,InfNumbers b) -> Node -> Maybe Path
getPathList [] _ _ = Nothing
getPathList ((c,p):ls) (o,ow) d 
  | c == d = return [c]
  | c == o = path >>= return . (o :)
  | otherwise = path
  where path = do
          parent <- p
          getPathList ls parent d

parentTree :: (WeightedGraph g b, Real b) => g a b -> [(Node,InfNumbers b)] -> Tree (Node,InfNumbers b)
parentTree g = L.foldl (flip (insertT (parent g))) Empty

-}





{-
astar' :: (WeightedGraph g b, Real b) => g a b -> Node -> ParHeur b -> [(Node,InfNumbers b)]
astar' g s h = loop (decrease (s,F 0) q0)
  where q0 = fromList [v :-> Inf | v <- nodes g]
        loop q = case minView q of 
          Nothing -> []
          Just (b,q1) -> (c,d - F (h c)) : loop (decreaseList bs q1)
            where bs = [(v, d + F ( w v - h c + h v))| v <- adjacents g c]
                  c = key b 
                  d = prio b
                  w x = weight g (c,x)


h ::  g a b -> (M.Map Node (Maybe Node,b), PSQ Node b) -> (M.Map Node (Maybe Node,b), PSQ Node b)
h g (m,q) = case minView q of 
          Nothing -> (m,q)
          Just (b,q1) -> (adjustList as m, decreaseList bs q1)
  where bs = [(v, F (d + w v + h v))| v <- adjacents g c]
        c = key b
        (p,d') = m M.! c

g :: (WeightedGraph g b, Real b) => g a b -> Node -> M.Map Node (Maybe Node,b) -> M.Map Node (Maybe Node,b) 
g gr n m = decList bs m
  where bs = [(v,(Just n, F (d + w v))) | v <- adjacents g n]
        w x = weight g (c,x)
        (_,d) = m M.! n
                
dec :: (Ord k) => (k,p) -> M.Map k p -> M.Map k p
dec (k,p) m = adjust (min p) k m

decList :: (Ord k) => [(k,p)] -> M.Map k p -> M.Map k p
decList bs m = L.foldr dec m bs
-}
        


