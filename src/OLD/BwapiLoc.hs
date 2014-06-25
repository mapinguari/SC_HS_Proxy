module Proxy.BwapiLoc (BwapiLoc, closestPixels,fromPixel,toPixel,isAt,mDistance,eDistance) where
import Proxy.Rectangle 
import Proxy.Types.Game

class (Eq a) => BwapiLoc a where
  closestPixels :: a -> a -> (Pixel,Pixel)
  fromPixel :: Pixel -> a
  toPixel :: a -> Pixel
  isAt :: Pixel -> a -> Bool
  isAt p x = fromPixel p == x
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
  
instance BwapiLoc WTile where 
  toPixel = rOrg
  fromPixel (P x y) = WT (x `quot` 8, y `quot` 8)
  closestPixels = closestVerticies

instance BwapiLoc BTile where
  toPixel = rOrg
  fromPixel (P x y) = BT (x `quot` 32, y `quot` 32)
  closestPixels = closestVerticies