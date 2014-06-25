module Proxy.Basics where
import Data.List
import Data.Ord

data Pixel = P {x,y::Int}
           deriving (Show,Eq)
newtype WTile = WT Position
newtype BTile = BT Position
type Position = (Int,Int)

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
  closestVerticies x y = (closestInX,minimumBy (comparing (mDistance closestInX)) [f,g,h,i])
    where (a,b,c,d) = rVerticies x
          (f,g,h,i) = rVerticies y
          closestInX = minimumBy (comparing (mDistance (rOrg y))) [a,b,c,d]

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
  
class BwapiLoc a where
  closestPixels :: a -> a -> (Pixel,Pixel)
  fromPixel :: Pixel -> a
  toPixel :: a -> Pixel
  isAt :: Pixel -> a -> Bool
  mDistance :: (Integral numOfPixels) => a -> a -> numOfPixels
  mDistance x y = uncurry mpDistance $ closestPixels x y
  eDistance :: (Floating euclideanD) => a -> a -> euclideanD
  eDistance x y = uncurry epDistance $ closestPixels x y
  
instance BwapiLoc Pixel where
  closestPixels p q = (p,q)
  fromPixel = id
  toPixel = id
  isAt p q = p == q
  mDistance = mpDistance
  eDistance = epDistance
  
mpDistance :: (Integral numOfPixels) => Pixel -> Pixel -> numOfPixels
mpDistance (P x y) (P z w) = fromIntegral $ abs (z - x) + abs (w - y) 

epDistance :: (Floating euclideanD) => Pixel -> Pixel -> euclideanD
epDistance (P x y) (P z w) = sqrt $ (fromIntegral (z - x))**2 + (fromIntegral(w-y))**2
  
instance BwapiLoc WTile where 
  toPixel = rOrg
  fromPixel (P x y) = WT (x `quot` 8, y `quot` 8)
  closestPixels = closestVerticies
  isAt = isIn

instance BwapiLoc BTile where
  toPixel = rOrg
  fromPixel (P x y) = BT (x `quot` 32, y `quot` 32)
  closestPixels = closestVerticies
  isAt = isIn
          
class (Rectangle a) => BwapiRegLoc a where
  closestPixelsReg :: (BwapiRegLoc b) => a -> b -> (Pixel,Pixel)
  closestPixelsReg = closestVerticies
  mDistanceReg :: (Integral numOfPixels) => a -> a -> numOfPixels
  mDistanceReg x y = uncurry mpDistance $ closestVerticies x y
  eDistanceReg :: (Floating euclideanD) => a -> a -> euclideanD
  eDistanceReg x y = uncurry epDistance $ closestVerticies x y
  isAtReg :: Pixel -> a -> Bool
  isAtReg = isIn
  
          
instance BwapiRegLoc Pixel 
instance BwapiRegLoc WTile
instance BwapiRegLoc BTile

class BWAPIBat a where 
  bHeight :: a -> Int
  bWidth :: a -> Int 
  