module Proxy.PathFinding.Specification where
import Proxy.Math.Graph
import Data.Function
import Data.Monoid
import Control.Monad
import Data.Sequence as Seq hiding (filter,partition,zip,length,sortBy,null, reverse)
import Data.Foldable hiding (concatMap,concat,elem,all,foldl,any)
import Data.List (partition, sortBy)                   
import Control.Monad.State
import Data.Array
import Data.Maybe

newtype Path = Path {rep :: Seq Node}
             deriving (Eq)
                      
class PathLike p where 
  destination :: p -> Maybe Node
  origin :: p -> Maybe Node 
  completePath :: p -> [Node]
  

  edges :: p -> [Edge]
  edges p = let p' = completePath p in zip p' (tail p') 
  isOnPath :: Node -> p -> Bool
  isOnPath n = any (==n) . completePath
  safeDestination :: p -> Node
  safeDestination p = case destination p of
    Nothing -> error "No Destination"
    Just d -> d
  
instance PathLike Path where 
  destination p = case viewr.rep $ p of
    EmptyR -> Nothing 
    _ :> a -> Just a
  
  origin p = case viewl.rep $ p of
    EmptyL -> Nothing
    a :< _ -> Just a

  completePath = toList.rep

instance Show Path where
  show = show.completePath

mkPath :: [Node] -> Path
mkPath = Path . fromList

advance :: Path -> Node -> Path
advance p = Path . ((rep p) |>)

retreat :: Path -> Node -> Path
retreat p = Path . (<| (rep p))

(+|+) :: Path -> Path -> Path
(+|+) p1 p2 = Path ((rep p1) >< (rep p2))

instance Monoid Path where 
  mempty = Path empty
  mappend p1 p2 = if destination p1 == origin p2 
                              then p1 +|+ p2
                              else mempty
                                   
data WeightedPath a = WP {wpWeight :: a ,wpPath :: Path}
                     deriving (Show,Eq)
                              
mkWPath :: a -> [Node] -> WeightedPath a
mkWPath c ns = WP c (mkPath ns)

addToWP :: (Num a) => a -> Node -> WeightedPath a -> WeightedPath a
addToWP a n wp = WP (wpWeight wp + a) (advance (wpPath wp) n)
                              
instance PathLike (WeightedPath a) where 
  destination = destination . wpPath
  origin = origin . wpPath
  completePath = completePath.wpPath 
  
pathFrom :: (WeightedGraph g, Num b) => g a b -> Node -> WeightedPath b -> WeightedPath b 
pathFrom g n wp = WP {wpWeight = wpWeight wp - pc g prefix,
                      wpPath = Path suffix}
  where prefix = Path $ pre |> n
        (pre,suffix) = spanl (/= n) . rep . wpPath $ wp

instance (Ord a) => Ord (WeightedPath a) where 
  (<=) = (<=) `on` wpWeight

instance (Num a) => Monoid (WeightedPath a) where
  mempty = WP 0 mempty
  mappend p q = let np = (mappend `on` wpPath) p q in 
    if np == mempty 
    then mempty
    else WP (wpWeight p + wpWeight q) np
                     
newtype Least a = Least {getLeast :: Maybe a}

instance (Ord a) => Monoid (Least a) where 
  mempty = Least Nothing
  mappend (Least Nothing) m = m
  mappend (l@(Least (Just a))) (Least Nothing) = l 
  mappend (Least a) (Least b) = Least (liftM2 min a b)
                                
instance Monad Least where 
  return a = Least (Just a) 
  (>>=) (Least (Just a)) f = f a 
  (>>=) (Least (Nothing)) f = Least Nothing
  
  
class PathState ps where 
  mkPS :: [Node] -> [WeightedPath b] -> ps b
  nodesVisited :: ps b -> [Node]
  noPathsLeft :: ps b -> Bool
  remainingPaths :: ps b -> [WeightedPath b]
  updatePathsToConsider :: (Ix b, Num b) => Heuristic b -> ps b -> ps b
  nextPath :: ps b -> WeightedPath b
  addToVisited :: Node -> ps b -> ps b
  
instance PathState ListsPathState where
  mkPS ns wps = LPS (ns,wps)
  nodesVisited = fst.internal
  noPathsLeft = null.snd.internal
  remainingPaths = snd.internal
  updatePathsToConsider h (LPS (ms,xs)) = LPS (ms ,pigeonholeSort heuristic . strip . elems . accumArray (<>) Empty (minAndMax (map fst paths)) $ (zip ms (repeat Complete) ++ paths))
    where paths = map g xs
          g wp = (safeDestination wp, PF wp)
          heuristic p = wpWeight p + h (safeDestination p)
          
  nextPath = head . remainingPaths
  addToVisited n ps = LPS (n : nodesVisited ps,remainingPaths ps)
  
newtype ListsPathState b = LPS {internal ::([Node],[WeightedPath b])}

data PF b = Empty | Complete | PF b

instance (Ord b) => Monoid (PF b) where 
  mempty = Empty
  mappend Empty x = x
  mappend x Empty = x
  mappend Complete _ = Complete
  mappend _ Complete = Complete
  mappend (PF a) (PF b) = PF (min a b)
  

type Heuristic b = Node -> b

strip :: [PF b] -> [b]
strip [] = [] 
strip (x:xs) = case x of 
  PF b -> b : strip xs
  otherwise -> strip xs

pigeonholeSort :: (Ix b) => (a -> b) -> [a] -> [a]
pigeonholeSort f xs = concat . elems . accumArray (flip (:)) [] (minAndMax $ map f xs) $ map (\ x -> (f x, x)) xs

minAndMax :: (Ord a) => [a] -> (a,a)
minAndMax [] = error "minAndMax : empty list"
minAndMax (x:xs) = foldl f (x,x) xs
  where f (a,b) x = (min x a, max x b)
        
mergeOn :: (Ord b) => (a -> b) -> [a] -> [a] -> [a]
mergeOn _ [] xs = xs
mergeOn _ xs [] = xs
mergeOn f (x:xs) (y:ys) 
  | f x <= f y = x : mergeOn f xs (y:ys)
  | otherwise = y : mergeOn f (x:xs) ys

        


-------------------------------------------------------------------------------------------ACCURATE BUT INEFFICIENT--------------------------------------
sp'' g n1 n2 = minimumBy (compare `on` (pc g)) (filter (pathFromTo n1 n2) ( validPaths g))
                
validPaths :: (Graph g) => g a b -> [Path]
validPaths g = filter (isPath g) (allPaths g)
              
allPaths :: (Graph g) => g a b -> [Path]
allPaths = map mkPath . map reverse . allSequences . nodes 

allSequences :: [a] -> [[a]]
allSequences xs = concat $ iterate (addAll xs) [] 
  where addAll ys [] = map (: []) ys
        addAll ys xs = [(y : zs) | y <- ys , zs <- xs]

isPath :: (Graph g) => g a b -> Path -> Bool
isPath g p = all (uncurry (arc g)) $ zip xs (tail xs)
 where xs = completePath p

pc :: (WeightedGraph g, Num b) => g a b -> Path -> b
pc g = foldl f 0 . edges
  where f n = (+) n . (weight g) 
        
pathFromTo :: Node -> Node -> Path -> Bool
pathFromTo n1 n2 p = case a of 
  Nothing -> False
  Just False -> False
  otherwise -> True
  where a = do
          o <- origin p
          d <- destination p
          return (o == n1 && d == n2)
          
          
          
{-
sp :: (Num b, Ord b, WeightedGraph g) => g a b -> Node -> Node -> Maybe (WeightedPath b)
sp g n1 n2 = first atDestination allPathsInWeightOrder
  where allPathsInWeightOrder = concat (alteringIterate (allPathsLessThan g) (+1) 1 [mkWPath 0 [n1]])
        atDestination = maybe False (== n2) . destination

allPathsLessThan :: (WeightedGraph g, Ord b, Num b) => g a b -> b -> [WeightedPath b] -> [WeightedPath b]
allPathsLessThan g n = filter ((< n).wpWeight) . concatMap (appendAdjacents) 
  where appendAdjacents wp = map (\ (w,m) -> addToWP w m wp) (maybe [] (adjaWeights g) (destination wp))
        
alteringIterate :: (b -> a -> a) -> (b -> b) -> b -> a -> [a]
alteringIterate f g b a = a : alteringIterate f g (g b) (f b a)



groupBy :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupBy f [] = [] 
groupBy f (x:xs) = as : groupBy f bs
  where (as,bs) = partition ((== (f x)) . f) (x:xs)

takeIncreasingOn :: (Ord b) => (a -> b) -> [a] -> [a]
takeIncreasingOn f xs = map fst $ takeWhile ((uncurry (<)).g) $ zip xs (tail xs)
  where g (x,y) = (f x, f y)

sp' :: (Num b, Ord b, WeightedGraph g) => g a b -> Node -> Node -> Maybe (WeightedPath b)
sp' g n1 n2 = first atDestination allPathsInWeightOrder
  where allPathsInWeightOrder = concat $ takeWhile (not.null) (alteringIterate shortestCurrentPaths (+1) 1 [mkWPath 0 [n1]])
        atDestination = maybe False (== n2) . destination
        shortestCurrentPaths b = if map (minimumBy (compare `on` wpWeight)) . groupBy destination . allPathsLessThan g b
-}