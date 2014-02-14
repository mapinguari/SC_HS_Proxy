module Proxy.PreProcessing.PotentialField where
import Proxy.Helpers (distance)
import Proxy.Types.Game
import Data.IntSet hiding (filter,map,null,partition)
import Data.List (partition)
import Data.Array
import Data.Graph.Inductive.Graph hiding (empty)
import Data.Graph.Inductive.PatriciaTree
import Debug.Trace (trace)

type Position = (Int,Int)
type Potential = Double 
data Battlefield = Bat {bList :: [[Tile]],
                        bArray ::  Array Position Tile,
                        bGraph :: Gr Tile (),
                        nodeToPos :: Node -> Position,
                        posToNode :: Position -> Node}

staticObstacleInfluenceDistance :: Distance
staticObstacleInfluenceDistance = 4

obstacleMaxPotential :: Potential
obstacleMaxPotential = 80


mkBat :: [[Tile]] -> Battlefield
mkBat xss = Bat {bList = xss,
                 bArray = bListToArray xss,
                 bGraph = bListToGraph xss,
                 nodeToPos = nodeToPosition width,
                 posToNode = positionToNode width}
            where width = length.head $ xss
                   
nodeToPosition :: Width -> Node -> Position
nodeToPosition w n = case (q,r) of
  (q,0) -> (q, w)
  (q,r) -> (q+1, r)
  where r = n `rem` w
        q = n `div` w
        
positionToNode :: Width -> Position -> Node
positionToNode w (x,y) = (x-1)*w + y


bListToArray :: [[Tile]] -> Array Position Tile
bListToArray xs = array b (zip (range b) (concat xs))
  where b = ((1,1),(length xs,length.head $ xs))

bListToGraph :: [[Tile]] -> Gr Tile () 
bListToGraph xs = mkGraph a (filter legalEdge adjacencies)
  where legalEdge (x,y,_) = not $ x `member` b || y `member` b
        adjacencies = [(i,j,())| i <- range (1,c), j <- (adjacentNodes i)]
        adjacentNodes i = filter (inRange (((1,1),(h,w))).(nodeToPosition w)) (adjacent i)
        w = length.head $ xs
        h = length xs
        (a,b,c) = f (concat xs) 1 [] empty
        f [] n ys sT = (ys,sT,n-1)
        f (x:xs) n ys sT = f xs (n+1) ((n,x):ys) (if walkable x then sT else insert n sT)
        adjacent i
          | i == 1 = [2,1+w]
          | i == w = [w-1,2*w]
          | i == w*(h-1) +1 = [i-w,i+1]
          | i == w*h = [i-w,i-1]
        --Corners
          | r == 1 = [i-1,i+w,i+1]
          | r == h = [i-1,i-w,i+1]
          | c == 1 = [i-w,i+1,i+w]
          | c == w = [i-w,i-1,i+w]
        --Edges
          |otherwise = [i-1,i-w,i+1,i+w]
          where (r,c) = nodeToPosition w i
            

battlefield n m = [[Tile 1 (f i j) False | j <- [1..m]]| i<- [1..n]]
                  where f i j = i /= j

printTile :: Tile -> Char
printTile t = case walkable t of
  True -> '+'
  False -> '-'

bfMap ::  (Graph gr) => gr Tile b -> [(Node,Int)] -> [(Node,Int)]
bfMap g xs | null xs || isEmpty g = []
bfMap g ((x,n):xs) = case match x g of 
  (Nothing,g') -> bfMap g' xs
  (Just (pre,_,_,_),g') -> (x,n) : bfMap g' (xs ++ (zip (map snd pre) (repeat (n+1))))
                                      
f :: (Graph gr,Show b) => gr Tile b -> Int -> [(Node,Int)]
f g m
  | m == 0 = []
  | length xs == 4 || length xs == 0 = f g (m-1)
  | otherwise = (n,1) : f g (m-1) 
  where (~(Just (_,n,_,xs),_)) = match m g
        
staticObstacleManhattan :: (Graph gr) => gr Tile () -> [(Node,Int)]
staticObstacleManhattan g = bfMap g xs
                       where xs = f g (noNodes g)
                             
staticObstaclePotential :: Distance -> Potential 
staticObstaclePotential d = if d > soid
                            then 0
                            else omp / 2**d
  where soid = staticObstacleInfluenceDistance 
        omp = obstacleMaxPotential

bottleneck :: 