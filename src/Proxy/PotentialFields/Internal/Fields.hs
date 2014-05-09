module Proxy.PotentialFields.Internal.Fields where
import Proxy.Types.Game
import Proxy.Server.Messages
import Proxy.Query.Unit
import Data.Complex
import Data.List (foldl')
import Control.Applicative ((<*>))

--REALISTICALLY IT WOULD BE NICE TO GO IN AND CHANGE ALL OF THESE DEFINITIONS TO BE MUCH MORE ABSTRACT -IE REMOVE REFERENCES TO THE HSBWAPI

type PotCalc a = a -> UnitData -> Vector 
type Vector = (Double,Double)

nullVector :: Vector
nullVector = (0,0)

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
passAv h fu = ((1/ d h f) ^ 2) `mult` grad h f
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
grad h@(hx,hy) f@(fx,fy) = (1/d f h) `mult` (fromIntegral $ fx-hx,fromIntegral $ fy-hy)


mult :: (Num a) => a -> (a,a) -> (a,a)  
mult x (v,w) = (x*v,x*w)

add :: (Num a) => (a,a) -> (a,a) -> (a,a) 
add (x,y) (z,v) = (z+x,y+v)

vSum :: (Num a) => [(a,a)] -> (a,a) 
vSum = foldl' add (0,0)

d ::(Floating a) => Location -> Location -> a
d (x,y) (z,w) = sqrt.fromIntegral $ ((x-z) ^ 2) + ((y-w) ^ 2)
