module Proxy.Paul.SimplePolygon where

import Data.Array
import Proxy.Game

data Direction = North | West | South | East
               deriving (Eq,Show)
instance Enum Direction where
  toEnum x = case x of
    0 -> North
    1 -> West
    2 -> South 
    3 -> East
    otherwise -> toEnum (x `mod` 4)
  fromEnum x  = case x of
    North -> 0
    West -> 1
    South -> 2
    East -> 3
  
type Position = (Int, Int)
type SPolygon = [GridRef]
type NodeId = Position
type NicheList = [Position]
type GridRef = (Double,Double)
type Factor = GridRef

move :: Direction -> Position -> Position
move x = let n (x,y) = (x-1, y)
             e (x,y) = (x, y+1)
             s (x,y) = (x+1,y)
             w (x,y) = (x,y-1) in 
         case x of
           North -> n
           South -> s
           East -> e
           West -> w
           
--rotateCounterClockwise
rCC :: Direction -> Direction
rCC = succ
--rotateClockwise
rC :: Direction -> Direction
rC = pred
--opposite direction
opp :: Direction -> Direction
opp = rCC.rCC

($>) :: (a->b) -> (a,a) -> (b,b)
f $> (x,y) = (f x, f y)

--first Corner, upon entering a tile from a direction fC returns the first corner on your right
fC :: Direction -> Factor
fC x = case x of 
  North -> fromInteger $> (0,0)
  West -> fromInteger $> (1,0)
  South -> fromInteger $> (1,1)
  East -> fromInteger $> (0,1)

--antiClockwise turn to a factor that will return the next corner when used with functions below.
nC :: Factor -> Factor
nC x = case x of 
  (0.0,0.0) -> (1.0,0.0)
  (1.0,0.0) -> (1.0,1.0)
  (1.0,1.0) -> (0.0,1.0)
  (0.0,1.0) -> (0.0,0.0)
               
argfor :: a -> (a->b) -> b 
x `argfor` f = f x

---------------------------------------------------------------------
--Functions for mapping an abstract grid to pixels and walk tiles and back
--The following four functions are not inverses at the moment. Not injective at all.
--The following two functions generally place on bottom and right border.

gridToPix :: GridRef -> Position
gridToPix (x,y) = (\x -> if x == 0 then 1 else ceiling x) $> (x,y)

gridToTile :: GridRef -> Position
gridToTile p = let g = \x -> if x `mod` 8 == 0 then (x `div` 8) else (+1) $ x `div` 8 in 
  case gridToPix p of
    (x,y) -> (g x, g y)

pixToGrid :: Position -> Factor -> GridRef
pixToGrid (x,y) (a,b) = let g = \x -> x > 1 || x < 0 
                            x' = (fromInteger.toInteger) x
                            y' = (fromInteger.toInteger) y in 
  if g a || g b 
  then error "Factor invalid, must satisfy 0<=x<=1"
  else ((+x')(a-1),(+y')(b-1))

tileToGrid :: Position -> Factor -> GridRef
tileToGrid (x,y) (a,b) = let g = \x -> x > 1 || x < 0 
                             x' = (fromInteger.toInteger) x
                             y' = (fromInteger.toInteger) y in 
  if g a || g b 
  then error "Factor invalid, must satisfy 0<=x<=1"
  else ((*8).(+x')$(a-1),(*8).(+y')$(b-1))

-------------------------------------------------------------------------
--Functions for checking tile information 

obstruction :: Array Position Tile -> Position -> Bool
obstruction a p = not (inRange (bounds a) p && (walkable $ a!p)) 

terrainChange :: Array Position Tile -> Position -> Position -> Bool
terrainChange a p q = case (a!p,a!q) of
  (Tile h w _,Tile h' w' _) -> not ( h==h' && w==w')

nodeChange :: Array Position Tile-> Position -> Position -> Bool
nodeChange a q p = obstruction a p || terrainChange a p q

---------------------------------------------------------------------------------------
--get to a "Niche" as quickly as possible
--final condition assuring we don't cross the polygon and maybe hit a complexification
fastNiche :: Array Position Tile -> Position -> Position
fastNiche a p = case map ((nodeChange a p).(p `argfor`).move) (iterate rCC North) of
  True:True:_ -> p 
  True:_ -> move West p
  False:False:True:_ -> move West p 
  otherwise -> move North p

--Move round the boundary of the polygon until you find the next niche and return its position
nextNiche :: Array Position Tile -> Position -> Position 
nextNiche a p = nN a p East
                where nN a q x = let d = aCB a q x in case d of
                        Nothing -> q 
                        Just x -> if nicheCheck x $ move x q
                                  then move x q
                                  else nN a (move x q) (opp x)   
                      nicheCheck x q = case x of 
                        West  -> and $ take 2 $ map ((nodeChange a q).(q `argfor`).move) (iterate rCC North)
                        North -> and $ take 3 $ map ((nodeChange a q).(q `argfor`).move) (iterate rCC East)
                        otherwise -> False
--
poly :: Array Position Tile -> Position -> SPolygon
poly a p = polybuild (tileToGrid p (0,0)) p East []
           where polybuild q x d xs 
                   | length xs > 1 && head xs == q = xs
                   | otherwise = case b of
                          Nothing -> singleTilePolygon
                          Just z -> if z == opp d 
                                    then polybuild q (move z x) d xs
                                    else polybuild q (move z x) (opp z) $ (vertAdder x z d) ++ xs
                   where b = aCB a x d 
                         singleTilePolygon = [(tileToGrid x (fC d)),(tileToGrid x (nC.nC.nC $ fC d)),(tileToGrid x (nC.nC $ fC d)),(tileToGrid x (nC $ fC d)),(tileToGrid x (fC d))]
                 vertAdder x z d
                   | z == rCC d = [tileToGrid x (fC d)]
                   | z == rC d = [tileToGrid x $ nC (fC d)]
                   | z == d = [tileToGrid x (nC.nC $ fC d),tileToGrid x (nC $ fC d)]
                 

--anti Clockwise Boundary 
-- Array, at position, from direciton, next direction
aCB :: Array Position Tile -> Position -> Direction -> Maybe Direction
aCB a p x = let d = rCC x in case map ((nodeChange a p).(p `argfor`).move) (iterate rCC d) of 
  False:_ -> Just d 
  True:False:_ -> Just (rCC d) 
  True:True:False:_ -> Just (opp d) 
  True:True:True:False:_-> Just x
  otherwise -> Nothing

b= listArray ((1::Int,1::Int),(1::Int,1::Int)) [Tile 1 True True]

a = array ((1,1),(100,100)) $ [((1,1),Tile 1 False False)] ++ [(i,Tile 2 True True)| i <- (tail.range $ ((1,1),(100,100)))]

node :: Array Position (SPolygon,NicheList)
node = array (bounds a) [(i, (polyPlot i, nichePlot i))| i <- range.bounds $ a]
       where polyPlot p = case walkable (a!p) of
               False -> [tileToGrid p (0,0)]
               otherwise -> case p == fastNiche a p of 
                 True -> if p == nodeID p then poly a p else fst (node ! nodeID p)
                 False -> fst (node ! fastNiche a p)
             nichePlot p = case walkable (a!p) of
               False -> []
               otherwise -> case p == fastNiche a p of
                 True -> nicheList p
                 otherwise -> []
             nicheList p = p: snd (node ! (nextNiche a p))
             nodeID p = minimum(p: (takeWhile (/=p) $ tail (snd (node ! p ))))





