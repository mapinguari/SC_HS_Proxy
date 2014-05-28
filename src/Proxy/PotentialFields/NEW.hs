module NEW where 
import Data.Ix
import Data.Array
import Control.Arrow

type Position = (Int,Int)

data Direction = X | Y
               deriving (Show,Eq)

h :: (Ix a,Num a) => (a,a) -> (a,a) -> [(a,a)]
h (v,w) = filter (inbounds) . mk
  where inbounds = inRange ((0,0),(v-1,w-1))
        mk (x,y) = [(x,y+1),(x-1,y),(x+1,y),(x,y-1)]
f' :: (Integral a) => (a,a) -> a -> (a,a)
f' (_,w) n = (n `rem` w, n `div` w)

f :: (Num a) => (a,a) -> (a,a) -> a
f (_,w) (x,y) = x + y * w

g :: (Ix a,Integral a) => (a,a) -> a -> [a]
g b = map (f b) . h b . f' b

adjacents :: (Ix a,Integral a) => (a,a) -> (a -> Bool) -> a -> [a]
adjacents b m = filter m . g b 

adjacents' :: (Ix a,Integral a) => (a,a) -> (a -> Bool) -> a -> [a]
adjacents' b m = map (f b) . filter ((m . f b) ^&& (ib b)) . mk . f' b
  where ib (x,y) = inRange ((0,0),(x-1,y-1))
        mk (x,y) = [(x,y+1),(x-1,y),(x+1,y),(x,y-1)]
        

(^&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(^&&) = ((.) ((.) (uncurry (&&)))) . (&&&)

tileLookup :: [[a]] -> Position -> a
tileLookup xs = (!) (array ((0,0),(w-1,h-1)) (zip pos (concat xs)))
  where pos = [ (x,y) | (y,x) <- range ((0,0),(h-1,w-1))]
        w = length.head $ xs
        h = length xs


verticies :: (Num a,Eq a) => [(a,a)] -> [(a,a)]
verticies xs = map g $ filter (not.colinear) (zip3 xsL1 xs xsR1)
  where colinear ((ax,ay),(bx,by),(cx,cy)) = (threequal ax bx cx) || (threequal ay by cy)
        xsR1 = transposeR 1 xs
        xsL1 = transposeL 1 xs
        g (x,y,z) = y

threequal :: (Eq a) => a -> a -> a -> Bool
threequal a b c = a == b && b == c

transposeR :: Int -> [b] -> [b]
transposeR _ [] = []
transposeR n xs = let m = n `mod` length xs 
                      (ts,ds) = splitAt (length xs - m) xs in
                  reverse ds ++ ts
                  
transposeL :: Int -> [b] -> [b] 
transposeL _ [] = []
transposeL n xs = let m = n `mod` length xs 
                      (ts,ds) = splitAt (length xs - m) xs in
                  ds ++ reverse ts

type Bounds = ((Int,Int),(Int,Int))

width :: Bounds -> Int
width ((x,y),(z,w)) = z - x

height :: Bounds -> Int
height ((x,y),(z,w)) = w - y

allSame :: (Eq b) => (a-> b) -> [a] -> Bool
allSame f [] = True
allSame f (x:xs) = all (== f x) (map f xs)

split ::  (Integral a) => a -> a -> a
split a b = (a + ((b - a) `div` 2))
                
interToDiags :: (Int,Int) -> (Int,Int) -> Bounds
interToDiags (x,y) (z,w) = ((x,z),(y,w))

cross :: [(Int,Int)] -> [(Int,Int)] -> [Bounds]
cross [] _ = []
cross (x:xs) ys = foldr f [] ys ++ cross xs ys
  where f y xs = interToDiags x y : xs
        
data QuadTree a = Empty | Leaf a | Branch {bl,br,tl,tr ::(QuadTree a)} 

quadtree :: ((Int,Int) -> Bool) -> Bounds -> QuadTree Bounds
quadtree f b = case same b of
  True -> Leaf b 
  False -> mkTree b 
  where same b = all (== f h) (map f bs)
        (h:bs) = range b
        mkTree :: Bounds -> QuadTree Bounds
        mkTree ((a,c),(b,d)) 
          | b - a == 0 = Branch (quadtree f ((a,c),(b,my))) Empty (quadtree f ((a,my+1),(b,d))) Empty 
          | d - c == 0 = Branch (quadtree f ((a,c),(mx,d))) (quadtree f ((mx+1,c),(b,d))) Empty Empty
          | otherwise = Branch (quadtree f ((a,c),(mx,my))) (quadtree f ((mx+1,c),(b,my))) (quadtree f ((a,my+1),(mx,d))) (quadtree f ((mx+1,my+1),(b,d))) 
          where mx = split a b
                my = split c d
                      