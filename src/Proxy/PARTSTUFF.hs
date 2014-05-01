
class Rectangle a where
  --Top left of rectangle
  rOrg :: a -> Pixel
  rHeight :: a -> Int
  rWidth :: a -> Int
  rArea :: a -> Int
  rPerimeterL :: a -> Int
  rVerticies :: a -> (Pixel,Pixel,Pixel,Pixel)
  isSquare :: a -> Bool
  isIn :: Pixel -> a -> Bool
  isIn p r = (x p) >= x (rOrg r) && x p <= x (rOrg r) + rWidth r - 1 && y p >= y (rOrg r) && y p <= y (rOrg r) + (rHeight r) - 1
  rArea r = rWidth r * rHeight r
  rPerimeterL r = 2*(rWidth r) + 2*(rHeight r)
  isSquare r = rHeight r == rWidth r
  rVerticies r = (org, P ((x org)+ ((rWidth r)-1)) (y org),P (x org) ((y org)+(rHeight r - 1)),P ((x org)+rWidth r - 1) ((y org)+(rHeight r - 1 )))
                  where org = rOrg r
  closestVerticies :: (Rectangle b) => a -> b -> (Pixel,Pixel)
  closestVerticies x y = (closestInX,minimumBy (comparing (mpDistance closestInX)) [f,g,h,i])
    where (a,b,c,d) = rVerticies x
          (f,g,h,i) = rVerticies y
          closestInX = minimumBy (comparing (mpDistance (rOrg y))) [a,b,c,d]

instance Rectangle Pixel where 
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
  rOrg = locationToPixel . unitLocation
  rHeight = (8*) . tileHeight . unitType
  rWidth = (8*) . tileWidth . unitType
  
data Priority a = Priority {priority :: PriorityVal, action :: a}

instance (Ord a) => Ord (Priority a) where 
  compare (Priority n a) (Priority m b) = compare n m
  
instance (Eq a) => Eq (Priority a) where 
  (==) (Priority n a) (Priority m b) = n == m && a == b
  
instance Functor Priority where
  fmap f (Priority n a) = Priority n (f a)
  
instance Comonad Priority where 
  extract (Priority n a) = a
  duplicate (Priority n a) = Priority n (Priority n a)

moreUrgent :: Priority a -> Priority a
moreUrgent (Priority n a) = Priority (n+1) a 

lessUrgent :: Priority a -> Priority a
lessUrgent (Priority n a) = Priority (n-1) a