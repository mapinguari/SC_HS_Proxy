module Proxy.Paul.Ff3 where

-- import Proxy.Paul.Terrain
import Data.Array
import Proxy.Paul.BSTree
import Control.DeepSeq

data NavTag = NT {height :: Int, node :: Int}
     deriving (Show, Eq)

instance NFData NavTag where
  rnf (NT h n) = h `seq` n `seq` ()

floodfill :: Array (Int,Int) NavTag -> (Int,Int) -> Int -> Array (Int,Int) NavTag
floodfill a p n = let on = node (a ! p) 
                      updates = [((fst i), NT (snd i) n) | i <- (inorder (floodfill' a p on Nil))]
                                in if inRange (bounds a) p 
                                   then a//updates
                                     else error "Bounds Error!"

floodfill' :: Array (Int, Int) NavTag -> (Int,Int) -> Int -> Tree ((Int,Int),Int) -> Tree ((Int,Int),Int)
floodfill' a p@(x,y) c xt = if inRange (bounds a) p && node (a!p) == c && not( contains (fmap fst xt) p)
                            then wt
                            else xt
  where 
        ft = insert xt (p,(height (a!p)))
        nt = floodfill' a (x,y+1) c ft
        et = floodfill' a (x+1,y) c nt
        st = floodfill' a (x,y-1) c et
        wt = floodfill' a (x-1,y) c st
                                   
main = deepseq (floodfill (listArray ((1,1), (100,100)) [NT 1 1 | i <- range ((1,1),(100,100)) ]) (5,5) 6) ()
