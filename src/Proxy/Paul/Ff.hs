module Proxy.Paul.Ff where

-- import Proxy.Paul.Terrain
import Data.Array

data NavTag = NT {height :: Int, node :: Int}
     deriving (Show, Eq)

floodfill :: Array (Int,Int) NavTag -> (Int,Int) -> Int -> Array (Int,Int) NavTag
floodfill a p n = let on = node (a ! p) 
                      updates = [(i, NT (height (a ! i)) n) | i <- (floodfill' a p on [])]
                                in if inRange (bounds a) p 
                                   then a//updates
                                     else error "Bounds Error!"

floodfill' :: Array (Int, Int) NavTag -> (Int,Int) -> Int -> [(Int,Int)] -> [(Int,Int)]
floodfill' a p@(x,y) c xs = if inRange (bounds a) p && node (a ! p) == c && not( elem p xs)
                            then ws
                            else xs
  where fs = p:xs
        ns = floodfill' a (x,y+1) c fs
        es = floodfill' a (x+1,y) c ns
        ss = floodfill' a (x,y-1) c es
        ws = floodfill' a (x-1,y) c ss
                                   
main = print $ floodfill (listArray ((1,1), (100,100)) [NT 1 1 | i <- range ((1,1),(100,100)) ]) (5,5) 6