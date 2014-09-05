module Proxy.PotentialFields.Internal.Fields where
import Proxy.Types.Game
import Proxy.Server.Messages
import Proxy.Query.Unit
import Data.Complex
import Data.List (foldl')
import Control.Applicative ((<*>))
import System.Random
import Control.DeepSeq
import Data.Array

--REALISTICALLY IT WOULD BE NICE TO GO IN AND CHANGE ALL OF THESE DEFINITIONS TO BE MUCH MORE ABSTRACT -IE REMOVE REFERENCES TO THE HSBWAPI

type PotCalc a = a -> UnitData -> Vector 
type Vector = (Double,Double)

lOfTrap :: Int
lOfTrap = 5

sOfTrap :: Double
sOfTrap = 3

trapped :: [Location] -> [(Int,Location)]
trapped [] = [] 
trapped (x:xs) = if l > lOfTrap
                 then (l,centroid as) : trapped bs
                 else trapped bs
  where (as,bs) = span ((<3).(d x)) (x:xs)
        l = length as
        
passAvSize :: PotCalc (Int,Location)
passAvSize (i,l) u = mult (fromIntegral i) (passAv l u)
        
avoidTrappedOld :: PotCalc [Location]
avoidTrappedOld hs u =  foldr g nullVector (trapped hs)
  where g l v = passAvSize l u `add` v

avoidAllTrapped :: PotCalc [[Location]]
avoidAllTrapped lss u = vSum (map (flip avoidTrappedOld u) lss)


randMag :: Double
randMag = 0.5

nullVector :: Num a => (a,a)
nullVector = (0,0)

randomField :: RandomGen g => PotCalc g
randomField g _ = randMag `mult` (x,y)
  where x:y:_ = randomRs (-1,1) g 
        
avoidAllOld :: PotCalc [Location]
avoidAllOld hs u = foldr g nullVector hs 
  where g l v = passAv l u `add` v

nofield :: PotCalc a 
nofield _ _ = nullVector

peaApp :: PotCalc Location
peaApp h fu = mult (-1) (grad h f)
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
passAv h fu = ((recip (d h f)) ^ 2) `mult` grad h f
  where f = unitLocation fu
        
passAvU :: PotCalc UnitData 
passAvU = passAv.unitLocation
        
actAv :: PotCalc UnitData
actAv hu fu = (2*(di+har)*(di - hsr)*(di - har - hsr)) `mult` grad h f
  where di = d h f
        h = unitLocation hu
        f = unitLocation fu
        hsr = fromIntegral.(+1).sightRange.unitType $ hu 
        har = fromIntegral.(+1) $ if isFlyer.unitType $ fu
                                  then airRange.unitType $ hu
                                  else groundMaxRange.unitType $ hu


mapData :: PotCalc [Location]
mapData xs = vSum . (([passAv] <*> xs) <*>) . (flip (:) [])

grad :: Location -> Location -> Vector
grad h@(hx,hy) f@(fx,fy) = (recip (d f h)) `mult` (fromIntegral $ fx-hx,fromIntegral $ fy-hy)

centroid :: [Location] -> Location
centroid xs = vmap round $ (recip . fromIntegral . length) xs `mult` (vmap fromIntegral . vSum) xs

mult :: (Num a) => a -> (a,a) -> (a,a)  
mult x (v,w) = (x*v,x*w)

add :: (Num a) => (a,a) -> (a,a) -> (a,a) 
add (x,y) (z,v) = (z+x,y+v)

vSum :: (Num a) => [(a,a)] -> (a,a) 
vSum = foldl' add (0,0)

vmap :: (a -> b) -> (a,a) -> (b,b)
vmap f (x,y) = (f x, f y) 

d ::(Floating a) => Location -> Location -> a
d (x,y) (z,w) = sqrt.fromIntegral $ ((x-z) ^ 2) + ((y-w) ^ 2)

{-
f :: (Ix a) => (b -> c -> b) -> b -> (a,a) -> [(a,c)] -> [[(a,c)]] -> [Array a b]
f g base bounds baselist xs = foldr h [] xs   
  where arr y = accumArray g base bounds (baselist ++ y)
        h a b = arr a : b
        
test = f (\x y -> y) 0 (1,100000) (zip [1..100000] (repeat 1)) [[(100000,2)],[(1,1)],[],[],[(3,10)]]
-}