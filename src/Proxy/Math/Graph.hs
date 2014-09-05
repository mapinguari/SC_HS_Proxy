{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Proxy.Math.Graph where
import Data.Array
import Data.Maybe

type Node = Int
type Edge = (Node,Node)
type Ed a = Edge
type WEdge a = (Edge,a)
type Weight = Float

completeGraph n = buildwGraph n [((i,j),1)| i<- [0..(n-1)], j <- [0..(n-1)]]

class Graph g where 
  buildGraph :: Int -> [Edge] -> g () ()
  --Minimum definition
  size :: g a b -> Int
  arc :: g a b -> Node -> Node -> Bool  
  --Optional Definition
  stnecajda :: g a b-> Node -> [Node]
  adjacents :: g a b -> Node -> [Node] 
  allEdges :: g a b -> [Edge]
  nodes :: g a b -> [Node]
  cra :: g a b -> Node -> Node -> Bool
  edgesFrom :: g a b -> Node -> [Edge]
  edgesTo :: g a b -> Node -> [Edge]
  --Default Implementation
  adjacents g n = filter (arc g n) (nodes g)
  stnecajda g n = filter (flip (arc g) n) (nodes g)
  allEdges g = filter (uncurry (arc g)) [(i,j) | i <- nodes g, j <- nodes g]
  nodes g = range (0,size g - 1)
  cra = flip . arc
  edgesTo g n = zip (stnecajda g n) (repeat n)
  edgesFrom g n = zip (repeat n) (adjacents g n)  
  
class (Real b,Graph g) => WeightedGraph g b where
  weight :: (Real b) => g a b -> Edge -> b
  --optional
  adjaWeights :: (Num b,Eq b) => g a b -> Node -> [(b,Node)]
  adjaWeights g n = [(weight g (n,i),i)| i <- adjacents g n] 
  stneWeights :: (Num b,Eq b) => g a b -> Node -> [(b,Node)]
  stneWeights g n = [(weight g (i,n),i)| i <- stnecajda g n]
  
class (Graph g) => LabelledGraph g a where 
  label :: g a b -> Node -> a
  allLabels :: g a b -> [a]
  allLabels g = map (label g) (nodes g)
  
newtype WMGraph a b = WM {wmGraph :: Array (Node,Node) (Maybe b)}
                    deriving Show

buildwGraph :: Int -> [((Node,Node),a)] -> WMGraph b a
buildwGraph n = WM . accumArray second Nothing ((0,0),(n-1,n-1)) . map (\(x,y) -> (x, Just y))
  where second _ x = x
        
newtype WLAGraph a b = WLA {wlaGraph :: Array Node [(b,Node)]}

instance Graph WLAGraph where 
  size = (+1) . snd . bounds . wlaGraph
  arc g a b = not.null.filter ((==b).snd) $ (wlaGraph g ! a)
  
instance (Real b) => WeightedGraph WLAGraph b where 
  weight g (x,y) = fst . head . filter ((==y).snd) $ wlaGraph g ! x 

class Graph g => MutableGraph g where 
  addEdge :: g a b -> Edge -> g a b
  removeEdge :: g a b -> Edge -> g a b
  addNode :: g a b -> Node -> g a b
  removeNode :: g a b -> Node -> g a b

instance Graph WMGraph where
  size = (+1) . fst . snd . bounds . wmGraph
  arc g n1 n2 = isJust (wmGraph g ! (n1,n2))
  
instance (Real a) => WeightedGraph WMGraph a where
  weight g e = case wmGraph g ! e of 
    Just w -> w
    otherwise -> error "No edge"
    

data Wrapper a b = W {list :: [a], test :: a -> a -> Bool, cost :: a -> a -> b}

instance Graph Wrapper where 
  arc g n m = test g (list g !! (n+1)) (list g !! (m+1))
  size = length . list 
  
instance (Real b) => WeightedGraph Wrapper b where
  weight g (n,m) = cost g (list g !! (n+1)) (list g !! (m+1))
  
instance LabelledGraph Wrapper a where
  label g n = list g !! (n+1)

{-
newtype MatrixGraph a b = MG {mGraph :: Array (Node,Node) Bool}

instance Graph MatrixGraph where 
  buildGraph n xs = MG (accumArray (||) False ((0,0),(n-1,n-1)) (zip xs (repeat True)))
  size = (+1) . fst . snd . bounds . mGraph
  allEdges = map fst . filter ((== True).snd)  . assocs . mGraph
  arc g n1 n2 = mGraph g ! (n1,n2)
  adjacents g n = filter (arc g n) $ nodes g 
  stnecajda g n = filter (cra g n) $ nodes g
  
newtype AdjacencyListGraph a b = ALG {lGraph :: Array Node [Node]}
 deriving Show
         


instance Graph AdjacencyListGraph  where
  buildGraph n = ALG . accumArray (flip (:)) [] (0,n-1) --O(n)
  size = (+1) . snd . bounds . lGraph -- O(1)
  allEdges = concatMap (uncurry (zip.repeat)) . assocs . lGraph
  arc = ((.) (flip elem)) . (!) . lGraph
  adjacents = (!) . lGraph
  stnecajda g n = map fst . filter ((== n).snd) . allEdges $ g
 
-}