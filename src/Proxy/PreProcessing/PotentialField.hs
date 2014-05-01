module Proxy.PreProcessing.PotentialField where
import Proxy.Helpers (distance)
import Proxy.Types.Game
import Data.Array
import Proxy.Server.Messages
import Proxy.Query.Unit
import Data.List(mapAccumL,maximumBy)
import Data.Maybe (catMaybes)
import Control.Applicative hiding (empty)
import Data.Complex

nodeToPosition :: Width -> Int -> Position
nodeToPosition w n = (q,r)
  where r = n `rem` w
        q = n `quot` w

type Potential = Double 
--type PotCalc = Location -> Potential

type PotCalc a = a -> UnitData -> Vector 
type Vector = (Double,Double)

moveTo :: Location -> Vector -> Location 
moveTo l v 
  | magnitude cv == 0 = l
  | otherwise = l `add` xi 8 phi  
  where phi = phase cv
        cv = uncurry (:+) v
        
xi :: (RealFloat a,Integral b) => b -> a -> (b,b) 
xi b theta 
  | negate ((1/4)*pi) < theta && (1/4)*pi >= theta = (b,chi $ theta)
  | (1/4)*pi < theta && (3/4)*pi >= theta = (chi.ref $ theta ,b)
  | negate ((3/4)*pi) < theta && negate ((1/4)*pi) >= theta = (chi $ theta + ((1/2)*pi),negate b)
  | otherwise = (negate b, chi.prep $ theta)
  where chi = round.(f.fromIntegral $ b)
        prep a = if a > 0
                 then pi - a
                 else negate (a + pi)
        ref a = (1/2)*pi - a
  
mapData :: PotCalc [Location]
mapData xs = sum . (([passAv] <*> xs) <*>) . (flip (:) [])
  where sum = foldl add nullVector
        
f :: (Floating a) => a -> a -> a
f a theta = (4 * a * theta)  / pi 

nullVector :: Vector
nullVector = (0,0)

nofield :: PotCalc a 
nofield _ _ = nullVector

peaApp :: PotCalc Location
peaApp h fu = grad h f
 where f = unitLocation fu
       
agApp :: PotCalc UnitData
agApp hu fu = (2*di*(di - far)*(2*di - far)) `mult` grad h f
  where di = d h f
        h = unitLocation hu
        f = unitLocation fu
        far = fromIntegral $ if isFlyer.unitType $ hu
                             then airRange.unitType $ fu
                             else groundMaxRange.unitType $ fu
        
passAv :: PotCalc Location
passAv h fu = ((1/ d h f) ^ 2) `mult` grad h f
  where f = unitLocation fu
        
actAv :: PotCalc UnitData
actAv hu fu = (2*(di+har)*(di - hsr)*(di - har - hsr)) `mult` grad h f
  where di = d h f
        h = unitLocation hu
        f = unitLocation fu
        hsr = fromIntegral.(+1).sightRange.unitType $ hu 
        har = fromIntegral.(+1) $ if isFlyer.unitType $ fu
                                  then airRange.unitType $ hu
                                  else groundMaxRange.unitType $ hu

grad :: Location -> Location -> Vector
grad h@(hx,hy) f@(fx,fy) = (1/d f h) `mult` (fromIntegral $ fx-hx,fromIntegral $ fy-hy)


mult :: (Num a) => a -> (a,a) -> (a,a)  
mult x (v,w) = (x*v,x*w)

add :: (Num a) => (a,a) -> (a,a) -> (a,a) 
add (x,y) (z,v) = (z+x,y+v)

d ::(Floating a) => Location -> Location -> a
d (x,y) (z,w) = sqrt.fromIntegral $ ((x-z) ^ 2) + ((y-w) ^ 2)
{-
       
------------------Total Potential of a Location  --------------------------

totalPotential :: PotCalc -> PotCalc -> PotCalc -> PotCalc -> PotCalc
totalPotential static dynamic objective historical = foldl1 (*) . (potentialFunctions <*>) . (flip (:)) []
  where potentialFunctions = [static,dynamic,objective,historical]

-----------------Basic Potential field stuff starts here ------------------
------sort out picking next location 
------sort out dangerous and mobile obstacle

--------------Locations selection and decision making----------------

---all locations occupied by a walk tile
tileToLocations :: Position -> [Location]
tileToLocations (x,y) = [(i,j) | i <- [(8*x)..(8*(x+1))], j <- [(8*y)..(8*(y+1))]]

---all locations occupied by unit
locationsOfUnit :: UnitData -> [Location]
locationsOfUnit uD = allLocations $ unitLocation uD
  where allLocations (z,w) = [(i,j) | i<-[z..z+(uW -1)], j<- [w..w+(uH -1)]]
        uH = (*8).tileHeight.unitType $ uD 
        uW = (*8).tileWidth.unitType $ uD

--all new totally new locations adjacent to current unit space
tilesToCheck :: UnitData -> [[Location]]
tilesToCheck uD = map allLocations corners
  where uH = (*8).tileHeight.unitType $ uD 
        uW = (*8).tileWidth.unitType $ uD
        (a,b) = unitLocation $ uD
        corners = [(i,j) | i <- [a..a+2*uW], j <- if i == a || i == a +2*uW
                                                  then [b..b+2*uH]
                                                  else [b,b+2*uH]]
        allLocations (z,w) = [(i,j) | i<-[z..z+(uW -1)], j<- [w..w+(uH -1)]]
        
-- list of locations adjacent to an arbitary rectangle given in height and width as number of locations
rectanglePerimeter :: Height -> Width -> Location -> [Location]
rectanglePerimeter h w (x,y) = [(i,j)| i<-[(x-1)..(x+h+1)], j <- if i == (x-1) || i == (x+w-1)
                                                                 then [(y-1)..(y+h+1)]
                                                                 else [(y-1),(y+h+1)]]
        
nullarySum :: (Num a,Ord a) => [a] -> a
nullarySum = foldr f 0 
  where f x y = if x < 0 then x else x + y

---------------Decision Making Potential ---------------
        
-- check all locations and find the one with the highest new potential
bestnewLocation :: UnitData -> Array Location Potential -> Maybe Location
bestnewLocation uD a = g . maximumBy f $ zip (map (nullarySum.(map (a!))) checkPositions) (map head checkPositions)
  where checkPositions = tilesToCheck uD
        f (n,p) (m,q) = compare n m 
        g (n,p) = if n == 0 then Nothing else Just p
        
-------------------Potential around units and static obstacles --------------

dangerousAir :: UnitData -> [(Location,Potential)]
dangerousAir uD = [(l,p j)| j <- [0..range] , l <- rectanglePerimeter (uH + j) (uW + j) (unitLocation uD) ]
  where range = airRange . unitType $ uD 
        p j = (fromIntegral j) * (1 / fromIntegral range)
        uH = (*8).tileHeight.unitType $ uD 
        uW = (*8).tileWidth.unitType $ uD

dangerousGround :: UnitData -> [(Location,Potential)]
dangerousGround uD = [(l,p j)| j <- [0..range] , l <- rectanglePerimeter (uH + j) (uW + j) (unitLocation uD) ]
  where range = groundMaxRange . unitType $ uD 
        p j = (fromIntegral j) * (1 / fromIntegral range)
        uH = (*8).tileHeight.unitType $ uD 
        uW = (*8).tileWidth.unitType $ uD
          
dynamicObstruction :: UnitData -> [(Location, Potential)]
dynamicObstruction uD = (concat [[(i,0.8)] |j <- [0..10], i <- (rectanglePerimeter (uH+j) (uW+j) (unitLocation uD))]) ++ staticObstruction uD
  where uH = (*8).tileHeight.unitType $ uD 
        uW = (*8).tileWidth.unitType $ uD

staticObstruction :: UnitData  -> [(Location,Potential)]
staticObstruction uD = zip (locationsOfUnit uD) (repeat 0)
        

staticMapPotential :: [[Tile]] -> [(Location,Potential)]
staticMapPotential xss = zip (concat $ map tileToLocations unwalkable) (repeat 0)
  where unwalkable = catMaybes . snd  $ mapAccumL f 0 (concat xss)
        f n t = if walkable t 
                then (n+1,Nothing)
                else (n+1,Just $ nodeToPosition (length.head $ xss) n)
        
---------------------------------------------
--------Simplest Example---------------------
        
airPotential :: Int -> Int -> [(Location,Potential)] -> Array Location Potential
airPotential h w xs = accumArray (*) 1 ((0,0),(8*h,8*w)) xs


airPotentialPairings :: [UnitData] -> [(Location,Potential)]
airPotentialPairings = concatMap f 
  where f uD 
          | canAttackAir.unitType $ uD = dangerousAir uD
          | isFlyer.unitType $ uD =  dynamicObstruction uD
          | otherwise = []


----------complete - O(n)--------------------
--swap the order of arguements for the append operation
airAndGroundPotential :: [[Tile]] -> [UnitData] -> (Array Location Potential,Array Location Potential)
airAndGroundPotential tss us = (accumArray (*) 1 ((0,0),(mH,mW)) affectsGround,
                                accumArray (*) 1 ((0,0),(mH,mW)) affectsAir)
  where mH = length tss
        mW = length.head $ tss
        affectsGround = staticMapPotential tss ++ gs
        affectsAir = as
        (gs,as) = foldl f ([],[]) us
        
        f (gs',as') uD = let uT = unitType uD in case (isFlyer uT, canMove uT, canAttackGround uT, canAttackGround uT) of
          (_,_,True,True) -> (dangerousGround uD ++ gs',dangerousAir uD ++ as')
          (False,False,False,False) -> (staticObstruction uD ++ gs', as')
          (False,True,False,False) -> (dynamicObstruction uD ++ gs',as')
          (False,_,False,True) -> (dangerousGround uD ++ gs',as')
          (False,False,True,False) -> (staticObstruction uD ++ gs', dangerousAir uD ++ as')
          (False,True,True,False) -> (dynamicObstruction uD ++ gs', dangerousAir uD ++ as')
          (True,_,False,False) -> (gs',dynamicObstruction uD ++ as')
          (True,_,False,True) -> (dangerousGround uD ++ gs', dynamicObstruction uD ++ as')
          (True,_,True,False) -> (gs',dangerousAir uD ++ as')
          
-}