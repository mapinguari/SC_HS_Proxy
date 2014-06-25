module Proxy.Math.Rectangle where
import Data.Ord
import Proxy.Math.Interval
import Proxy.Math.Line
import Data.List
import Data.Maybe
import Data.Function

data Rectangle a = Rectangle {xI,yI :: (Interval a)} 
                 deriving (Show,Eq)

piX :: Rectangle a -> Interval a
piX = xI

piY :: Rectangle a -> Interval a
piY = yI

height :: (Real a) => Rectangle a -> a
height =  ilength.yI 

width :: (Real a) => Rectangle a -> a
width = ilength.xI

area :: (Real a) => Rectangle a -> a
area = (\r -> width r * height r)

center :: (Real a,Fractional b) => Rectangle a -> Point b
center r = mkPoint (midpoint.xI $ r) (midpoint.yI $ r)

vertex :: (Real a,Integral b) => b -> Rectangle a -> Point a
vertex n r = case n `mod` 4 of
  0 -> mkPointxFirst (inf.xI $ r) (inf.yI $ r)
  1 -> mkPointxFirst (inf.xI $ r) (sup.yI $ r)
  2 -> mkPointxFirst (sup.xI $ r) (sup.yI $ r)
  3 -> mkPointxFirst (sup.xI $ r) (inf.yI $ r)
  
edge :: (Real a,Integral b) => b -> Rectangle a -> LineSeg 
edge n r = mkLineSegment (vertex n r) (vertex (n+1) r)

isInRect :: (Real a, Real b) => Point a -> Rectangle b -> Bool
isInRect p r = x p `isIn` xI r && y p `isIn` yI r

areaCompare :: (Real a) => Rectangle a -> Rectangle a -> Ordering
areaCompare = comparing area 

perimeterCompare :: (Real a) => Rectangle a -> Rectangle a -> Ordering
perimeterCompare = comparing f 
  where f = (\r -> height r * 2 + width r * 2)

squareCompare :: (Real a) => Rectangle a -> Rectangle a -> Ordering
squareCompare = comparing f 
  where f = (\r -> let h = fromRational.toRational $ (height r)
                       w = fromRational.toRational $ (width r) in 
                   min h w / max h w)

largerOrSquarer :: (Real a) => Rectangle a -> Rectangle a -> Ordering
largerOrSquarer r s = let areaC = areaCompare r s in 
  case areaC of
    EQ -> squareCompare r s 
    otherwise -> areaC

largestSquarest :: (Real a) => Rectangle a -> Rectangle a -> Rectangle a
largestSquarest r s = case largerOrSquarer r s of
  LT -> s
  GT -> r
  otherwise -> r
  
rectangleI :: (Real a) => Rectangle a -> Rectangle a -> Maybe (Rectangle a)
rectangleI r s = do 
  xInterval <- trivialIntersection (xI r) (xI s) 
  yInterval <- trivialIntersection (yI r) (yI s)
  return (Rectangle xInterval yInterval)
  
isLine :: (Eq a) => Rectangle a -> Bool
isLine r = trivial (piX r) || trivial (piY r)
  
commonEdge :: (Real a) => Rectangle a -> Rectangle a -> Maybe LineSeg
commonEdge r s = do 
  inter <- rectangleI r s
  if isLine inter 
    then return (mkLineSegment (vertex 0 r) (vertex 3 r))
    else Nothing
  
rDist :: (Real a) => Rectangle a -> Rectangle a -> a
rDist r s =  iDist (xI r) (xI s) + iDist (piY r) (piY s) 

centersDistance :: (Real a,Floating b) => Rectangle a -> Rectangle a -> b
centersDistance r s = case cedge >>= (lineSegIntersect direct) of
  Nothing -> distance closestPoint (center r) + distance closestPoint (center s)
  Just (Left a) -> lineSegLength direct
  otherwise -> error "Intersection of rectangles error"
  where direct = mkLineSegment (center r) (center s)
        cedge = commonEdge r s
        closestPoint = minimumBy (compare `on` (pointToLineDistance (lineEq direct))) (catMaybes [cedge >>= (return.mapPoint fromRational.start) , cedge >>= (return.mapPoint fromRational.end)])
        
