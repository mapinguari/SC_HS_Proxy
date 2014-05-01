module Proxy.Math.Rectangle where
import Proxy.Math.Interval
import Proxy.Math.Line

data Rectangle a = Rectangle {xI,yI :: (Interval a)} 
                 deriving (Show,Eq)


height :: (Num a) => Rectangle a -> a
height = ilength.yI 

width :: (Num a) => Rectangle a -> a
width = ilength.xI

area :: (Num a) => Rectangle a -> a
area = (\r -> width r * height r)

vertex :: (Num a,Integral b) => b -> Rectangle a -> Point a
vertex n r = case n `mod` 4 of
  0 -> mkPointxFirst (inf.xI $ r) (inf.yI $ r)
  1 -> mkPointxFirst (inf.xI $ r) (sup.yI $ r)
  2 -> mkPointxFirst (sup.xI $ r) (sup.yI $ r)
  3 -> mkPointxFirst (sup.xI $ r) (inf.yI $ r)
  
edge :: (Num a,Integral b) => b -> Rectangle a -> LineSeg 
edge n r = mkLineSeg (vertex n r) (vertex (n+1) r)

isInRect :: Point a -> Rectangle a -> Bool
isInRect p r = x p `isIn` xI r && y p `isIn` yI r







