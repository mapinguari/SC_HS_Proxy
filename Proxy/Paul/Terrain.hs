module Proxy.Paul.Terrain where
import Data.Array
import Control.Monad.State
import Proxy.Messages
import Proxy.Game


{-
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

data NavTag = NT {row :: Int, colS :: Int, colF :: Int, height :: Int, node :: Int}

type MappingState = (Int,Int,Int,Int,Int,[(Int,Int)])
-- Row, Starting Col, Current Col, height, node, graph verticies

g :: Tile -> State MappingState (Maybe NavTag)
g (Tile h b w) = state (\ (r,s,c,h',n,xs) -> case (r,s,c,h',n,xs) of
  	       	       	  	       (r,(-1),c,_,n,xs) -> if w == False then (Nothing, (r,(-1),c+1,h,n,xs)) else (Nothing,(r,c,c+1,h,n+1,xs))
				       (r,s,c,h',n,xs) -> if w == True 
				       		          then if (h == h') then (Nothing,(r,c,c+1,h,n+1,xs)) else (Just (NT r s (c-1) h' n), (r,c,c+1,h,n+1,((n,n+1):xs)))
						     	  else (Just (NT r s c h' n),(r, (-1),c+1,h,n,xs)))

rower :: [Tile] -> State MappingState [NavTag]
rower [] = do
      	   st <- get 
	   let (r,s,c,h,n,xs) = st 
	   if s == (-1) then return [] else return [NT r s (c-1) h n]
rower (x:xs) = do 
      	     tileOut <- g x
	     tileRow <- rower xs
	     case tileOut of 
	     	  Nothing -> return tileRow
	   	  Just navtag -> return (navtag:tileRow)


comp :: [[Tile]] -> State MappingState [[NavTag]]
comp tss = case tss of
	   [] -> return []
	   (nt:ts) -> do 
	   	      row <- rower nt
		      st <- get
		      let (r,s,c,h,n,xs) = st
		      put (Nothing, (r+1,(-1), 1, 0, n,xs))
		      rest <- comp ts
		      return (row:rest)


--END OF ROW CHECKER