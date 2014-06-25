module Proxy.Math.Graph where
import Data.Array

type Node = Int
type Edge = (Node,Node)

class Graph g where 
  --Minimum definition
  buildGraph :: Int -> [Edge] -> g a b 
  size :: g a b-> Int
  arc :: g a b-> Node -> Node -> Bool
  --Optional Definition
  stnecajda :: g a b -> Node -> [Node]
  adjacents :: g a b -> Node -> [Node] 
  allEdges :: g a b-> [Edge]
  nodes :: g a b-> [Node]
  cra :: g a b-> Node -> Node -> Bool
  edgesFrom :: g a b -> Node -> [Edge]
  edgesTo :: g a b-> Node -> [Edge]
  --Default Implementation
  adjacents g n = filter (arc g n) (nodes g)
  stnecajda g n = filter (flip (arc g) n) (nodes g)
  allEdges g = filter (uncurry (arc g)) [(i,j) | i <- nodes g, j <- nodes g]
  nodes g = range (0,size g - 1)
  cra = flip . arc
  edgesTo g n = zip (stnecajda g n) (repeat n)
  edgesFrom g n = zip (repeat n) (adjacents g n)  

newtype MatrixGraph a b = MG {mGraph :: Array (Node,Node) Bool}

instance Graph MatrixGraph where 
  buildGraph n xs = MG (accumArray (&&) False ((0,0),(n-1,n-1)) (zip xs (repeat True)))
  size = (+1) . fst . snd . bounds . mGraph
  allEdges = map fst . filter ((== True).snd)  . assocs . mGraph
  arc g n1 n2 = mGraph g ! (n1,n2)
  adjacents g n = filter (arc g n) $ nodes g 
  stnecajda g n = filter (cra g n) $ nodes g
  
newtype AdjacencyListGraph a b = ALG {lGraph :: Array Node [Node]}
 deriving Show

instance Graph AdjacencyListGraph where
  buildGraph n = ALG . accumArray (flip (:)) [] (0,n-1) --O(n)
  size = (+1) . snd . bounds . lGraph -- O(1)
  allEdges = concatMap (uncurry (zip.repeat)) . assocs . lGraph
  arc = ((.) (flip elem)) . (!) . lGraph
  adjacents = (!) . lGraph
  stnecajda g n = map fst . filter ((== n).snd) . allEdges $ g
  
class Graph g => WeightedGraph g where
  weight :: g a b -> Edge -> b
  adjaWeights :: g a b -> Node -> [(b,Node)]
  
class Graph g => LabelledGraph g where
  label :: g a b -> Node -> a 

class Graph g => MutableGraph g where 
  addEdge :: g a b -> Edge -> g a b
  removeEdge :: g a b -> Edge -> g a b
  addNode :: g a b -> Node -> g a b
  removeNode :: g a b -> Node -> g a b

cfr :: ((b -> a -> a) -> b -> a -> a) -> (b -> a -> a) -> a -> [b] -> a
cfr _ _ a [] = a 
cfr g f a (x:xs) = f x (cfr g (g f) a xs)

completeGraph n = buildGraph n [(i,j)| i <- [0..(n-1)], j <- [0..(n-1)]]

{-  
instance MutableGraph AdjacencyListGraph where
  addEdge g e = ALG (accum (flip (:)) (lGraph g) [e])
  removeEdge g e = ALG (accum (((flip (.)) (/=)) . (flip filter)) (lGraph g) [e])
 
instance MutableGraph MatrixGraph 
-}