module TileList where

import Data.Array
import Proxy.Game
import qualified Data.Set as Set

type Position = (Integer, Integer)
type NodeID = Int

fall :: Array Position Tile -> NodeID

f :: Array Position Tile -> Position -> Set.Set Position
f a p = if inArray a p
        then f' a Set.empty [p]
        else error "Position not in Array"
             
f' :: Array Position Tile -> Set.Set Position -> [Position] -> Set.Set Position
f' a xs [] = xs
f' a xs (y:ys) = let adjacents z = filter (tilesSame a z) (adjacentPos a z) 
                      in if (Set.member y xs) 
                         then f' a xs ys
                         else f' a (Set.insert y xs) $ (adjacents y) ++ ys

tilesSame :: Array Position Tile -> Position -> Position -> Bool
tilesSame a p c = (a ! p) == (a ! c)

adjacentPos :: Array Position Tile -> Position -> [Position]
adjacentPos a (x,y) = filter (inArray a) $ filter (/= (x,y)) $ range ((x-1,y-1),(x+1,y+1))

inArray :: (Ix a) => Array a b -> a -> Bool
inArray a = inRange (bounds a) 

g i 
  | even (fst i) || even (snd i) = Tile 1 True True
  | otherwise = Tile 2 True True
                