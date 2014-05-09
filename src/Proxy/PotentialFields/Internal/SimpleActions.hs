module Proxy.PotentialFields.Internal.SimpleActions where
import Proxy.PotentialFields.Internal.Fields
import Proxy.Commands
import Control.Monad.Reader
import Data.Complex
import Proxy.Types.Game
import Proxy.Server.Messages


type Map = [Location]
type FieldEnvironment = (Map,[UnitData],[UnitData])
type UnitAction = Reader FieldEnvironment Command

---------------------------------------------------------------------------------------------------------------------
--Simple UnitActions for a unit to perform. These are a single step that a unit might take at any given moment, they rely on the information from from potential field being generated.

toward :: UnitData -> Location -> UnitAction
toward ud l = mapReader (rightClickAt uid) field
  where uid = unitId ud
        field = zeta passAvU actAv (Just l) peaApp ud


-------------------------------------------------------------------------------------------------
--field accumulation zeta returns a computation which will return the resultant location for a unit to move to.

first (a,_,_) = a
second (_,a,_) = a
third (_,_,a) = a

zeta :: PotCalc UnitData -> PotCalc UnitData -> Maybe Location -> PotCalc Location -> UnitData -> Reader FieldEnvironment Location
zeta f h l lc u = do 
  mapd <- asks (flip mapData u . first)
  friend <- asks (map (flip f u) . second) 
  host <- asks (map (flip h u) . third) 
  loc <- case l of 
    Just a -> return [lc a u]
    otherwise -> return []
  return ( moveTo (unitLocation u) $ add mapd (vSum.concat $ [friend,host,loc]))
  
ro :: PotCalc UnitData -> PotCalc UnitData -> UnitData -> Reader FieldEnvironment Location
ro f h = zeta f h Nothing nofield

-----------------------------------------------------------------------------------------------------------
--Conversion from a float vector to a location for the unit to move to.
--NEED TO CONSIDER SPEED AND THE SIZE OF THE VECTOR RETURNED.

moveTo :: Location -> Vector -> Location 
moveTo l v 
  | magnitude cv == 0 = l
  | otherwise = l `add` xi 1 phi  
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
  
f :: (Floating a) => a -> a -> a
f a theta = (4 * a * theta)  / pi 

---------------------------------------------------------------------------------------------------------------------