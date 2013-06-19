module Proxy.Paul.Terrain where
import Data.Array
import Control.Monad.State
import Proxy.Messages
import Proxy.Game

data NavTag = NT {height :: Int, node :: Int}
     deriving (Show, Eq)

impasse :: NavTag
impasse = NT (-1) 0

arraycolour :: GameInfo -> Array (Int, Int) NavTag
arraycolour g = let (Map n h w tss) = (gameMap g)  in
                listArray ((1,1), (h,w)) (concat $ mapper tss) 



g :: Tile -> State (NavTag,Int) NavTag
g (Tile h b w) = state (\ (nt,n) -> if w == False then (impasse,(impasse,n)) 
                                                  else case h == height nt of 
                                                       True -> (NT h n, (NT h n,n))
                                                       False -> (NT h (n+1), (NT h (n+1),n+1)))

rower :: [Tile] -> State (NavTag,Int) [NavTag]
rower xs = case xs of 
           [] -> return []
           y:ys -> do
                   nt <- (g y) 
                   ntt <- rower ys
                   return (nt:ntt)

topper :: [[Tile]] -> State (NavTag,Int) [[NavTag]]
topper tss = case tss of 
             [] -> return []
             t:ts -> do
                     row <- rower t 
                     s <- get 
                     put (impasse, (snd s) +1)
                     rest <- topper ts
                     return (row:rest)

mapper :: [[Tile]] -> [[NavTag]]
mapper [] = [] 
mapper ts = evalState (topper ts) (impasse,0)

nextToCheck :: Array (Int, Int) NavTag -> (Int,Int) -> [(Int,Int)]
nextToCheck b (i,j) = let ((x,y),(z,w)) = bounds b in case i of
                                                      y -> [(i+1, a) | a <- [j,j+1]]
                                                      w -> [(i+1, a) | a <- [j-1,j]]
                                                      otherwise -> [(i+1, a) | a <- [(j-1)..(j+1)]]
mapping :: [(Int, Int)] -> NavTag -> NavTag
mapping xs x = if node x = i then NT (height x) j else x



floodFillMapping :: Array (Int, Int) NavTag -> [(Int,Int)]
fmap 



{-
data Polygon = Poly{ tl :: (Int, Int), br :: (Int, Int)}
     deriving (Show)

vect :: [[Tile]] -> [[Polygon]]
vect xs = flip vectorize 0 xs

vectorize :: [[Tile]] -> Int -> [[Polygon]]
vectorize [] n = []
vectorize (x:xs) n = (vectorizeIter (0,n) 0 x) : (vectorize xs (n+8))  
                   
vectorizeIter :: (Int, Int) -> Int -> [Tile] -> [Polygon]
vectorizeIter (n,m) x [] = [Poly{tl = (n,m), br = (n + (8*x), m+8)}]
vectorizeIter (n,m) x (y:xs) = let (Tile h b w) = y in 
                               case w of 
                                      True -> vectorizeIter (n,m) (x+1) xs
                                      False -> if x == 0 
                                               then vectorizeIter (n+8,m) 0 xs
                                               else Poly{tl = (n,m), br = (n + (8*x), m+8)}:vectorizeIter (n + (8*x), m) 0 xs
                          
rowing :: [[Polygon]] -> [[Polygon]]
rowing x:y:xs = x : (rowing (y++(rowiter x y)) 

rowiter :: [Polygon] -> [Polygon] -> [Polygon]
rowiter (p:ps) (q:qs) 
        | abs(fst (tl p) - fst (tl q)) <= 8  
-}
