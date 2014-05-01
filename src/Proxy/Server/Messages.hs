module Proxy.Server.Messages where
import Proxy.Types.Game
import Proxy.Types.ImportAllTypes
import qualified Proxy.Types.Orders as Orders

-- | I am not sure, a list of players
type Ack = [Player]

-- | List of starting locations, unsure if used for possible starting locations or for starting locations of players this match
type StartingLocations = [Location]

-- | List of Chokepoints on the map
type ChokeData = [Choke]
-- | List of Base locations
type BaseData = [Location]

-- | Time data. Unclear what is used for at this stage
data TimeData = TimeData { buildTime    :: Time
                         , trainTime    :: Time
                         , researchTime :: Time
                         , upgradeTime  :: Time 
                         , orderTimer   :: OrderTimer
                         } deriving Show

-- | Used to carry data about an individual unit
data UnitData = UnitData { unitId        :: UnitId
                         , unitOwnerId   :: PlayerId
                         , unitType      :: UnitType
                         , unitLocation  :: Location
                         , unitHealth    :: Health
                         , unitShields   :: Shields
                         , unitEnergy    :: Energy
                         , unitTimeData  :: TimeData
                         , unitOrder     :: Orders.Order
                         , unitResources :: Int
                         , unitAddonId   :: AddonId
                         , unitMineCount :: MineCount
                         } deriving Show

instance Eq UnitData where
  (==) u1 u2 = unitId u1 == unitId u2

-- | Used to contain game state, probably of an individual player
data GameState = GameState { gameResources     :: Resources
                           , gameSupply        :: Supply
                           , gameTechStatus    :: [(TechType, TechStatus)]
                           , gameUpgradeStatus :: [(UpgradeType, UpgradeStatus)]
                           , gameUnits         :: [UnitData]
                           } deriving Show

-- | Information about the current game
data GameInfo = GameInfo { gamePlayers           :: [Player]
                         , gameStartingLocations :: [Location]
                         , gameMap               :: MapData
                         , gameTerrain           :: (Maybe (ChokeData, BaseData))
                         }
                
instance Show GameInfo where 
  show (GameInfo gP gSL gM gT) = "Players \n" ++ playersAndLocations 
    where playersAndLocations = unlines $ map f $ zip gP gSL
          f (p,l) = show p ++ " at " ++ show l
        
-- | List of selected options about the current game
data Options = Options [Bool] deriving Show
-- | String of arguments for a command
type CommandArgs = String
-- | Data structure for a command
data Command = Command {commandType :: CommandType, subject :: UnitId, commandArgs :: CommandArgs} deriving (Show, Eq)
-- | A list of commands
data Commands = Commands [Command]

class Serialize a where
    serializes :: a -> ShowS
    serialize  :: a -> String
    serialize x = serializes x ""

instance Serialize Bool where
    serializes = shows . fromEnum

instance Serialize Options where
    serializes (Options bools) = serializesBools bools where
        serializesBools = foldr ((.) . serializes) (""++)

instance Serialize CommandType where
    serializes = shows . fromEnum

instance Serialize UnitType where
    serializes = shows . fromEnum

instance Serialize UpgradeType where
    serializes = shows . fromEnum

instance Serialize Int where
    serializes = shows

instance Serialize TechType where
    serializes = shows . fromEnum

instance Serialize Command where
    serializes (Command t s args) = serializes t . (';':) . (args ++)

instance Serialize Commands where
    serializes (Commands []) = ("" ++)
    serializes (Commands cs) = ("commands" ++) . serializesCommands cs where
        serializesCommands [] = (""++)
        serializesCommands (c:cs) = (':' :) . serializes c . serializesCommands cs


