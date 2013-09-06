module Proxy.Game where
import Proxy.Orders
import Proxy.TechTypes
import Proxy.UpgradeTypes

-- | Identifier
type Id = Int

-- | Unique Player identifier
type PlayerId = Id

-- | Unique Unit identifier (not sure if this is unique per frame or per game)
type UnitId = Id

-- | Name
type Name = String

-- | Location on map
type Location = (Int, Int)

-- | Width of object
type Width = Integer

-- | Height of object
type Height = Integer

-- | Not sure
type Slot = Int

-- | Speed of unit
type Speed = Int

-- | Whether a unit is buildable
type Buildable = Bool

-- | Not sure
type Walkable = Bool

-- | Contains information about a tile (consider changing to field name based)

data Tile = Tile Height Buildable Walkable deriving Show

walkable :: Tile -> Walkable
walkable (Tile g b w) = w

instance Eq Tile where
  t1 == t2 = let (Tile h1 b1 w1) = t1
                 (Tile h2 b2 w2) = t2 in
             h1 == h2 && b1 == b2 && w1 == w2

getWalkable :: Tile -> Int
getWalkable (Tile h b w) 
            | w == True = 1
            | otherwise = 0

-- | All the species in SC

data Race = Zerg
          | Terran
          | Protoss
          | OtherRace
          | UnknownRace
          | SelectRace
          | RandomRace
          | NoneRace deriving (Show, Enum, Eq)

-- | All player types
data PlayerType = NotUsed
                | Computer
                | Human
                | Rescuable
                | UnknownPlayerType0
                | ComputerSlot
                | OpenSlot
                | Neutral
                | ClosedSlot
                | UnknownPlayerType1
                | HumanDefeated
                | ComputerDefeated deriving (Show, Enum)

-- | Information about a given player 
data PlayerInfo = PlayerInfo { playerId :: PlayerId
                             , playerRace :: Race
                             , playerName :: Name
                             , playerType :: PlayerType
                             } deriving Show

-- | DataTypes for each of the types of other players
data Player = Me PlayerInfo
            | Ally PlayerInfo
            | Enemy PlayerInfo deriving Show

-- | Map information, name, dimensions and a list of areas,
data Map = Map Name Width Height [[Tile]]
     deriving Show

-- | Information about map regions which restrict movement, location and width
data Choke = Choke Location Width deriving Show

-- | Number of minerals ?
type Minerals = Int

-- | Quanitity of Gas ?
type Gas = Int

-- | Resources of a player ?
data Resources = Resources Minerals Gas deriving Show
-- | Unit Health
type Health = Int
-- | Unit Shields
type Shields = Int
-- | Unit Energy
type Energy = Int
-- | Time
type Time = Int
-- | Time until order completed?
type OrderTimer = Int
-- | Number of Addons? 
type AddonId = Id
-- | Don't know
type MineCount = Int
-- | Used to track total number of used unit supply
type Used = Int
-- | Used to track current total of unit supply
type Total = Int
-- | Player unit supply, used and total
data Supply = Supply Used Total deriving Show


-- | The current status of a specific tech upgrade
data TechStatus = TechNotResearched
                | ResearchingTech
                | TechResearched deriving Show

instance Enum TechStatus where
    toEnum 0 = TechNotResearched
    toEnum 1 = ResearchingTech
    toEnum 4 = TechResearched
    fromEnum (TechNotResearched) = 0
    fromEnum (ResearchingTech) = 1
    fromEnum (TechResearched)  = 4
-- | Probably for current level of tech upgrade
type Level = Int

-- | Probably for unit special upgrades current status
data UpgradeStatus = UpgradeNotResearched
                   | ResearchingUpgrade
                   | UpgradeResearched Level deriving Show

instance Enum UpgradeStatus where
    toEnum 0 = UpgradeNotResearched
    toEnum 4 = ResearchingUpgrade
    toEnum n = UpgradeResearched n
    fromEnum (UpgradeNotResearched) = 0
    fromEnum (ResearchingUpgrade) = 4
    fromEnum (UpgradeResearched level) = level

