module Main where
import AIFunctions
import Proxy.Server.Server
import Proxy.Math.Graph
import Hope
import System.Random
import Data.Array
import Control.Monad.State
import Data.Graph.Inductive.Query.Monad ((><))

main :: IO ()
main = Proxy.Server.Server.run onStart potFieldAllAttack


{-
type AdjR = (Int,Int) -> [(Int,Int)]
type Dist b = (Int,Int) -> (Int,Int) -> b

main :: IO () 
main = do 
  bools <- mapM ioBool testProbabilites
  let gridstates = mapM (state.buildGridState) gridSizes
      grids = concatMap (evalState gridstates) bools
      widthGraphs = map (f eightWay euclD) grids
      paths = (pathF (0,1) (99,99))
        where 
  putStr . show $ map paths widthGraphs
  return ()
  
f :: (Real b) => AdjR -> Dist b -> Array (Int,Int) Bool -> (Int, WMGraph a b) 
f ar d a = ((+1) . fst . snd . bounds $ a, graphFromGrid ar d a)

--pathF :: (Int,Int) -> (Int,Int) -> (Int,WMGraph a Float) -> Maybe [Node]
pathF o d (w,g) =  liftM (map (nodeToPos w)) $ sSastar g (posToNode w o) (posToNode w d) (nodeEuclD w)

testProbabilites = [0.05]
gridSizes = [ (100,100) ]

euclD :: (Int,Int) -> (Int,Int) -> Float
euclD (x,y) (z,w) = sqrt . fromIntegral $ (x - z) ^ 2 + (y - w) ^ 2

nodeEuclD w n m = euclD (nodeToPos w n) (nodeToPos w m)

rows :: Int -> State [a] [[a]]
rows n = do 
  r <- state (splitAt n)
  rs <- rows n
  return (r : rs)

posToNode :: Int -> (Int,Int) -> Node
posToNode w (x,y) = w*y + x

nodeToPos :: Int -> Int -> (Int,Int)
nodeToPos w n = (n `rem` w , n `div` w)

graphFromGrid :: (Real b) => AdjR -> Dist b -> Array (Int,Int) Bool -> WMGraph a b
graphFromGrid ar d a = WM . accumArray second Nothing ((0,0),(n*m - 1, n*m -1)) . map (wrap.eToE.f) $ edges ar a
  where eToE ((x,y),d) = ((posToNode n x , posToNode n y),d)
        f (x,y) = ((x,y), d x y)
        wrap = \(x,y) -> (x, Just y)
        second _ x = x
        (n,m) = ((+1) >< (+1)) . snd . bounds $ a

randomGridGraph :: AdjR -> Dist b -> Int -> Int -> Float -> WMGraph a b 
randomGridGraph ar d n m = WM . accumArray second Nothing ((0,0),(n*m - 1, n*m -1)) . map (wrap.eToE.f) . edges ar . buildGrid n m
  where eToE ((x,y),d) = ((posToNode n x , posToNode n y),d)
        f (x,y) = ((x,y), d x y)
        wrap = \(x,y) -> (x, Just y)
        second _ x = x

buildRows :: Int -> Int -> Float -> [[Bool]]
buildRows n m = take m . evalState (rows n) . bools

buildGridState :: (Int,Int) -> [Bool] -> (Array (Int,Int) Bool, [Bool])
buildGridState (i,j) bs = (listArray ((0,0),(i-1,j-1)) as, cs)
  where (as,cs) = splitAt (i*j) bs

buildGrid :: Int -> Int -> Float -> Array (Int,Int) Bool 
buildGrid n m = listArray ((0,0),(n-1,m-1)) . bools

edges :: AdjR -> Array (Int,Int) Bool -> [((Int,Int),(Int,Int))]
edges f a = filter isValid . concatMap (potEdges f) . indices $ a
  where isValid (x,y) = inRange (bounds a) x && inRange (bounds a) y &&  a ! x && a ! y

potEdges :: AdjR -> (Int,Int) -> [((Int,Int),(Int,Int))]
potEdges f x  = zip (repeat x) (f x)

fourWay :: AdjR
fourWay (x,y) = [(x-1,y),(x+1,y), (x,y+1), (x,y-1)]

eightWay :: AdjR
eightWay (x,y) = filter (/= (x,y)) (range ((x-1,y-1),(x+1,y+1)))

randomGraph :: Int -> Float -> WMGraph b Int 
randomGraph n = buildwGraph n . es
  where es = flip zip (repeat 1) . allowed
        allowed = map fst . filter snd . zip comp . bools 
        comp = [(i,j) | i <- [0..n-1] , j <- [0..n-1]]

ioBool :: Float -> IO [Bool]
ioBool p = do 
  g <- getStdGen 
  return (map (isObstructed p) (randomRs (0,1) g))

bools :: Float -> [Bool]
bools p = map (isObstructed p) (randomRs (0,1) (mkStdGen 5))

isObstructed :: Float -> Float -> Bool
isObstructed p i
  | i > p = True
  | i <= p = False
             
             
             

-}