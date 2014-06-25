module Proxy.Paul.Ff2 where

-- import Proxy.Paul.Terrain
import Data.Array
import Proxy.Paul.BSTree
import Control.DeepSeq
import Proxy.Game

type Position = (Int, Int)
type NodeID = Int

data NavTag = NT {height :: Int, node :: NodeID}
     deriving (Show, Eq)

instance NFData NavTag where
  rnf (NT h n) = h `seq` n `seq` ()

floodfill :: Array Position NavTag -> Position -> Int -> Array Position NavTag
floodfill a p n = let on = node (a ! p) 
                      updates = [(i, NT (height (a ! i)) n) | i <- (inorder (floodfill' a p on Nil))]
                                in if inRange (bounds a) p 
                                   then a//updates
                                     else error "Bounds Error!"

floodfill' :: Array Position NavTag -> Position -> NodeID -> Tree Position -> Tree Position
floodfill' a p@(x,y) c xt = if inRange (bounds a) p && node (a ! p) == c && not( contains xt p)
                            then wt
                            else xt
  where ft = insert xt p
        nt = floodfill' a (x,y+1) c ft
        et = floodfill' a (x+1,y) c nt
        st = floodfill' a (x,y-1) c et
        wt = floodfill' a (x-1,y) c st
                                          
hello = deepseq (floodfill (listArray ((1,1), (1000,1000)) [NT 1 1 | i <- range ((1,1),(1000,1000)) ]) (5,5) 6) ()

--buildBoringArrayToMakeLifeEasy
bBATMLE :: Map -> Array Position Tile
bBATMLE (Map n w h tss) = listArray ((1,1),(h,w)) (concat tss)


nextToCheck :: Array Position Tile -> Tree Position -> Position -> [Position]
nextToCheck a t (x,y) = filter (not.(contains t)) $ filter pred $ filter (inArray a) [(x-1,y-1),(x-1,y),(x-1,y+1),(x,y-1),(x+1,y-1),(x+1,y),(x+1,y+1)]
                        where pred = tilesSame a (x,y)

tilesSame :: Array Position Tile -> Position -> Position -> Bool
tilesSame a p c = (a ! p) == (a ! c)

shouldBe :: Array Position NavTag -> Position -> NodeID
shouldBe a p = head $ map node $ adjacent a p

adjacent :: Array Position NavTag -> Position -> [NavTag]
adjacent a p = map (a !) $ adjacentPos a p

adjacentPos :: Array Position NavTag -> Position -> [Position]
adjacentPos a (x,y) = filter (inArray a) $ filter (/= (x,y)) $ range ((x-1,y-1),(x+1,y+1))

inArray :: (Ix a) => Array a b -> a -> Bool
inArray a = inRange (bounds a) 

{-
impasse :: NavTag
impasse = NT (-1) 0

arraycolour :: GameInfo -> Array (Int, Int) NavTag
arraycolour g = let (Map n h w tss) = (gameMap g)  in
                listArray ((1,1), (h,w)) (concat $ mapper tss) 



g :: Tile -> State (NavTag,NodeID) NavTag
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


build :: Array Position Tile -> NodeID -> Array Position NodeID
build a f = array (bounds a) [(i, mu i) | i <- (bounds a)]
            where 

type Seeds = [(Position, NodeID)]

goaltest :: Seeds -> Position -> Maybe NodeID
goaltest ys y = (\xs -> case xs of
                  [] -> Nothing
                  [(a,b)] -> Just b) (filter (\(z,w) -> case z of
                                                 y -> True
                                                 otherwise -> False) ys)

somthing  :: Position -> (NodeID, Seeds) -> Int

mu' xs [] = Nothing
mu' xs (y:ys) = if (elem y xs) 
                then mu' xs ys
                else case (goaltest 
-}