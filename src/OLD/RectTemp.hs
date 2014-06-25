module Proxy.Rectangle where
import Interval

newType Rectangle a = Rectangle (Interval a) (Interval a)


p2P :: Point a -> (a,a)
p2P p = (x p, y p)

class PointSet a where 
  isIn :: (Ord b) => b -> a b -> Bool
  intersection :: (Ord b,PointSet c) =>  a b -> a b -> Maybe (c b)
  
instance PointSet Value where 
  isIn v (V w) = v == w
  intersection (V a) (V b) = if a == b then Just (V a) else Nothing
  
instance PointSet Point where 
  isIn p (P x y) = p == (x,y)
  intersection (P x y) (P z w) = if (x,y) == (z,w) then Just (P x y) else Nothing
  
instance PointSet Interval where 
  isIn p i = p < sup i && p > inf i
  intersection (I (a,b)) (I (c,d)) 
     | d < a || b < c || max a c == min b d = Nothing
     | a == d = Just (V a)
     | b == d = Just (V b)
     | otherwise = Just $ I (max a c,min b d)

instance PointSet Rectangle where
 isIn (x,y) r = x 'isIn' (iX r) && y 'isIn' (iY r)

class Rectangle rect where
  --Top left of rectangle
  rOrg :: rect b -> Point b
  rHeight :: a -> Int
  rWidth :: a -> Int
  rArea :: a -> Int
  rPerimeterL :: a -> Int
  rVerticies :: a -> (Point b,Point b,Point b,Point b)
  isSquare :: a -> Bool
  isIn :: Point b -> a -> Bool
  isIn p r = (x p) >= x (rOrg r) && x p <= x (rOrg r) + rWidth r - 1 && y p >= y (rOrg r) && y p <= y (rOrg r) + (rHeight r) - 1
  rArea r = rWidth r * rHeight r
  rPerimeterL r = 2*(rWidth r) + 2*(rHeight r)
  isSquare r = rHeight r == rWidth r
  rVerticies r = (org, P ((x org)+ ((rWidth r)-1)) (y org),P (x org) ((y org)+(rHeight r - 1)),P ((x org)+rWidth r - 1) ((y org)+(rHeight r - 1 )))
                  where org = rOrg r
  closestVerticies :: (Rectangle b) => a -> b -> (Point b,Point b)
  closestVerticies x y = (closestInX,minimumBy (comparing (mpDistance closestInX)) [f,g,h,i])
    where (a,b,c,d) = rVerticies x
          (f,g,h,i) = rVerticies y
          closestInX = minimumBy (comparing (mpDistance (rOrg y))) [a,b,c,d]

instance Rectangle Point b where 
  rOrg = id
  rHeight _ = 1
  rWidth _ = 1
  
instance Rectangle WTile where
  rOrg (WT (x,y)) = P (8*x) (8*y)
  rHeight _ = 8
  rWidth _ = 8
  
instance Rectangle BTile where 
  rOrg (BT (x,y)) = P (32*x) (32*y)
  rHeight _ = 32
  rWidth _ = 32

instance Rectangle UnitData where
  rOrg = locationToPoint . unitLocation
  rHeight = (8*) . tileHeight . unitType
  rWidth = (8*) . tileWidth . unitType