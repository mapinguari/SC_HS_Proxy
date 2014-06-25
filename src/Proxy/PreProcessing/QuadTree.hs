module Proxy.PreProcessing.QuadTree where
import Data.Ix
import Proxy.Types.Game

data QuadTree a = EMP | Leaf {colour :: Bool,value :: a} | Branch {bl,br,tl,tr :: (QuadTree a)}
                deriving (Show,Eq)

quadtree' :: (Position -> Bool) -> Positions -> QuadTree Positions
quadtree' test xs = case xs of
  [] -> EMP
  [x] -> Leaf (test x) [x]
  otherwise -> if same . map test $ xs 
               then Leaf (test.head $ xs) xs
               else fourUnCurry Branch . fourTupMap (quadtree' test) . safeSplit $ xs

same :: (Eq a) => [a] -> Bool
same [] = True
same (x:xs) = null $ filter (== x) xs

type Positions = [Position]

type Rep a = Maybe (a,a)

rep :: (Ix a) => [a] -> Rep a
rep [] = Nothing
rep xs = Just (minimum xs,maximum xs)

raRep :: (Ix a) => Rep a -> [a]
raRep Nothing = []
raRep (Just (a,b)) = range (a,b)

mkRep :: (Ix a) => a -> a -> Rep a
mkRep = (.) (repCheck . Just) . (,)  

repCheck :: (Ix a) => Rep a -> Rep a
repCheck Nothing = Nothing
repCheck (r@(Just (a,b))) 
  | a <= b = r
  | otherwise = Nothing

fourTupMap :: (a -> b) -> (a,a,a,a) -> (b,b,b,b)
fourTupMap f (x,y,w,v) = (f x,f y,f w,f v)

fourUnCurry :: (a -> a -> a -> a -> b) -> (a,a,a,a) -> b
fourUnCurry f (a,b,c,d) = f a b c d

quadMap :: (a->b) -> QuadTree a -> QuadTree b
quadMap f EMP = EMP
quadMap f (Leaf x a) = Leaf x (f a)
quadMap f (Branch a b c d) = Branch (quadMap f a) (quadMap f b) (quadMap f c) (quadMap f d) 

leaf :: QuadTree a -> Bool
leaf (Leaf _ _ ) = True
leaf _ = False

qt :: (Position -> Bool) -> Rep Position -> QuadTree (Rep Position)
qt f rp = case rp of
  Nothing -> EMP
  Just (a,b) -> if a == b
                then Leaf (f a) (Just (a,a))
                else buildTree .fourTupMap (qt f) . saReSplit $ rp
  where allEq a b c d = a == b && a == c && a == d
        allApp (Just (a,_)) _ _ (Just (_,b)) = Just (a,b) 
        first (x,y,z,w) = x
        buildTree ts = if (fourUnCurry allEq . fourTupMap leaf $ ts) && (fourUnCurry allEq . fourTupMap colour $ ts)
                         then Leaf {colour = colour.first $ ts,
                                    value = fourUnCurry allApp . fourTupMap value $ ts}
                         else fourUnCurry Branch ts

quadtree :: (Position -> Bool) -> Positions -> QuadTree Positions
quadtree f xs = case xs of 
  [] -> EMP
  [x] -> Leaf (f x) [x]
  xs -> buildTree . fourTupMap (quadtree f) . safeSplit $ xs
    where allEq a b c d = a == b && a == c && a == d
          allApp a b c d = (a ++ (b ++ (c ++ d)))
          first (x,y,z,w) = x
          buildTree ts = if (fourUnCurry allEq . fourTupMap leaf $ ts) && (fourUnCurry allEq . fourTupMap colour $ ts)
                         then Leaf {colour = colour.first $ ts,
                                    value = fourUnCurry allApp . fourTupMap value $ ts}
                         else fourUnCurry Branch ts
                          
quadTree' :: (Position -> Bool) -> Rep Position -> QuadTree (Rep Position)
quadTree' f = quadMap rep . quadtree f . raRep 

          
          
safeSplit :: Positions -> (Positions,Positions,Positions,Positions)
safeSplit xs = split xs (g x1 x2) (g y1 y2) 
  where (x1,y1) = head xs
        (x2,y2) = last xs
        g a b 
          | b == a = b + 1
          | otherwise = a + ((b - a) `div` 2)

safeRepSplit :: Rep Position -> (Rep Position,Rep Position,Rep Position,Rep Position)
safeRepSplit rp = z rep $ safeSplit (raRep rp)
  where z f (x,y,w,v) = (f x,f y,f w,f v)

saReSplit :: Rep Position ->  (Rep Position,Rep Position, Rep Position,Rep Position)
saReSplit (r@(Just ((x,y),(z,w)))) = reSplit r (g x z) (g y w)
 where g a b 
         | b == a = b + 1
         | otherwise = a + ((b - a) `div` 2)
               
reSplit :: Rep Position -> Int -> Int -> (Rep Position,Rep Position, Rep Position,Rep Position)
reSplit Nothing _ _ = (Nothing,Nothing,Nothing,Nothing)
reSplit (Just ((x,y),(v,w))) a b = (mkRep (x,y) (a,b), mkRep (a+1,y) (v,b), mkRep (x,b+1) (a,w), mkRep (a+1,b+1) (v,w) )
             


split :: Positions -> Int -> Int -> (Positions,Positions,Positions,Positions)
split xs a b = foldr g ([],[],[],[]) xs
  where g (p@(x,y)) (br,bl,tr,tl) 
          | x <= a && y <= b = (p:br,bl,tr,tl)
          | x > a && y <= b = (br,p:bl,tr,tl)
          | x <= a && y > b = (br,bl,p:tr,tl)
          | x > a && y > b = (br,bl,tr,p:tl)
                             

                             
split' :: Positions -> Int -> Int -> (Positions,Positions,Positions,Positions)
split' xs a b = (z,w,n,m)
  where z = filter bl xs
        w = filter br xs
        n = filter tl xs
        m = filter tr xs
        bl (x,y) = x <= a && y <= b 
        br (x,y) = x > a && y <= b 
        tl (x,y) = x <= a && y > b 
        tr (x,y) = x > a && y > b 

