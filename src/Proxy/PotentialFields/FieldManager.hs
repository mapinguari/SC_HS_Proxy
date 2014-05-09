module Proxy.PotentialFields.FieldManager where
import Proxy.PotentialFields.MetaCommands
import Proxy.PotentialFields.Internal.SimpleActions
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad.Reader
import Proxy.Types.Game
import Proxy.Server.Messages
import Control.Comonad
import Network
import GHC.IO.Handle
import Data.Ord
import Data.Array
import Data.List
import Proxy.Server.Server
import Proxy.Helpers

data Priority a = Priority {priority :: Int, action :: a}

instance Functor Priority where 
  fmap f (Priority x a) = Priority x (f a) 

instance Comonad Priority where 
  extract (Priority _ a) = a
  duplicate (Priority n a)  = Priority n (Priority n a)
  
instance Eq (Priority a) where 
  (==) p q = priority p == priority q

instance Ord (Priority a) where 
  compare = comparing priority


initializeFieldManager :: (Handle, HostName,PortNumber) -> GameState -> GameInfo -> MVar [UnitData] -> MVar [(UnitId,Priority MetaCommand)] -> IO ThreadId
initializeFieldManager conn gs gi ud pc = forkIO $ fieldManager conn pid battlefield ud pc []
  where battlefield = tilesToLocations.tiles.gameMap $ gi
        pid = playerId . getMyInfo . gamePlayers $ gi
        f xs = let {h = length xs ; w = (length.head) xs} in 
          [swap x| t <- concat xs, x <- range ((0,0),(h,w)), not.walkable $ t]
        swap (a,b) = (b,a) 
        g (z,w) = range ((8*z,8*w),(8*(z+1),8*(w+1))) 
        tilesToLocations = concatMap g . f
        
fieldManager :: (Handle, HostName, PortNumber) -> PlayerId -> [Location] -> MVar [UnitData] -> MVar [(UnitId,Priority MetaCommand)] -> [(UnitId,Priority MetaCommand)] -> IO ()
fieldManager conn pid m ud ps sPS = let fM = fieldManager conn pid m ud ps in 
  do 
    units <- takeMVar ud
    todoM <- tryTakeMVar ps 
    let xs = unitActions todoM sPS
        todo = (catActions. map snd) xs
        (a,b) = unitSeperate pid units
        env = (m,a,b)
        commands = runReader (sequence todo) env
    send conn (Commands commands)
    fM xs

unitSeperate :: PlayerId -> [UnitData] -> ([UnitData],[UnitData])
unitSeperate n = partition ((==n).unitOwnerId)

unitActions :: Maybe ([(UnitId,Priority MetaCommand)]) -> [(UnitId,Priority MetaCommand)] -> [(UnitId, Priority MetaCommand)]
unitActions Nothing oc = oc
unitActions (Just nc) oc = assocs $ accumArray (max) (Priority 0 STOP) (minimum (map fst tc), maximum (map fst tc)) tc
  where tc = nc ++ oc

catActions :: [Priority MetaCommand] -> [UnitAction]
catActions = foldl f [] 
 where f xs x = case x of 
         Priority _ STOP -> xs
         Priority _ (MC a c) -> a : xs
         



{-
---- chi is the basis of all potential fields based on unitData :
--- chi id f h us u = vSum $ ((map f (filter (== id) us)) ++ (map h (filter (/= id) us)) <*> [u])
chi :: PlayerId -> PotCalc UnitData -> PotCalc UnitData -> PotCalc [UnitData]
chi id f h = (flip (.) (flip (:) [])) . (((.) vSum) . nu)
  where nu = (<*>).((uncurry (++)).((map f) >< (map h)).(unitSeperate id))


-}
  