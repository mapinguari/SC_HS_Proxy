module Proxy.BwapiRegLoc where
import Proxy.Rectangle
import Proxy.Types.Game
import Data.List
import Data.Ord
import Proxy.Server.Messages
import Proxy.Types.UnitTypes (UnitType(NoneUnitType))
import Proxy.Types.Orders (Order(UnknownOrder))
          
data AdjacencyType = FourWay | EightWay

class (Rectangle a,Eq a) => BwapiRegLoc a where
  closestPixels :: (BwapiRegLoc b) => a -> b -> (Pixel,Pixel)
  closestPixels = closestVerticies
  fromPixel :: Pixel -> a
  toPixel :: a -> Pixel
  toPixel = rOrg
  mDistance :: (Integral numOfPixels) => a -> a -> numOfPixels
  mDistance x y = uncurry mpDistance $ closestVerticies x y
  eDistance :: (Floating euclideanD) => a -> a -> euclideanD
  eDistance x y = uncurry epDistance $ closestVerticies x y
  isAt :: Pixel -> a -> Bool
  isAt p x = fromPixel p == x
  possAdjacencies :: AdjacencyType -> a -> [a]

instance BwapiRegLoc Pixel where
  closestPixels p q = (p,minimumBy (comparing (mpDistance p)) [a,b,c,d])
    where (a,b,c,d) = rVerticies q
  fromPixel = id
  toPixel = id
  isAt p q = p == q
  mDistance = mpDistance
  eDistance = epDistance
  
instance BwapiRegLoc WTile where
  fromPixel (P x y) = WT (x `quot` 8, y `quot` 8)

instance BwapiRegLoc BTile where
  fromPixel (P x y) = BT (x `quot` 32, y `quot` 32)
  
instance BwapiRegLoc UnitData where
  fromPixel _ = UnitData (-1) (-1) (NoneUnitType) (-1,-1) 0 0 0 (TimeData 0 0 0 0 0) UnknownOrder 0 0 0

---I DONT KNOW HOW TO ARRANGE ALL OF THESE IDEAS.
class BwapiLocInfo a 
instance BwapiLocInfo Tile
instance BwapiLocInfo Bool

class Distanceable a where
  pixelCover :: a -> [Pixel]
  closestPixels :: (Distanceable b) => a -> b -> (Pixel,Pixel)
  closestPixels a b = concat $ foldr (zip (pixelCover b).repeat) [] (pixelCover a)
  mDistance :: (Integral c, Distanceable b) => a -> b -> c
  eDistance :: (Floating c, Distanceable b) => a -> b -> c
  mDistance x = uncurry mpDistance.closestPixels x 
  eDistance x = uncurry epDistance.closestPixels x 
  
class (Rectangle a,BwapiGridLoc a) => GridRectDistanceable a where
  closestPixels :: (Distanceable b) => a -> b -> (Pixel,Pixel)
  closestPixels = closestVerticies
  mDistance :: (Integral c, Distanceable b) => a -> b -> c
  eDistance :: (Floating c, Distanceable b) => a -> b -> c
  mDistance x = uncurry mpDistance.closestPixels x 
  eDistance x = uncurry epDistance.closestPixels x 