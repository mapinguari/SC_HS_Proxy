module Proxy.Commands where
import Proxy.Game
import Proxy.Messages
import Proxy.UnitTypes
import Proxy.TechTypes
import Proxy.UpgradeTypes
import qualified Proxy.CommandTypes as CT

---------------------------------------------------------------
-- * Commands
---------------------------------------------------------------
-- | These are all the standard commands that you use

attackMove :: UnitId -> Location -> Command
attackMove          = atLocation CT.AttackMove
-- ^ @ attackMove unit place @ This would make the unit @unit@ move to @place@ attacking enemy units enroute

-- | @ rightClickAt unit place@ will perform the action associated to right clicking at @place@ on the map with unit @unit@.
rightClickAt :: UnitId -> Location -> Command
rightClickAt        = atLocation CT.RightClickAt

-- | @ patrol unit place@ will command @unit@ to begin patrolling from its current location to that of @place@.
patrol :: UnitId -> Location -> Command
patrol              = atLocation CT.Patrol

-- | @ setRallyPosition unit place@ will set the rally point of @unit@ to place.
setRallyPosition :: UnitId -> Location -> Command
setRallyPosition    = atLocation CT.SetRallyPosition

-- | @land unit place@ will cause @unit@ to land at @place@
land :: UnitId -> Location -> Command
land                = atLocation CT.Land

-- | @unloadAllAt unit place@ will fully unload @unit@ at @place@
unloadAllAt :: UnitId -> Location -> Command
unloadAllAt         = atLocation CT.UnloadAllAt

-- | First arg will attack second 
-- | ALL OF THE FOLLLOWING UNIT UNIT ARGUMENTS NEED TO BE CHECKED FOR ORDER
attackUnit :: UnitId -> UnitId -> Command
attackUnit          = atUnit CT.AttackUnit

-- | Simulate a right click for arg1 on arg2
rightClickOn :: UnitId -> UnitId -> Command
rightClickOn        = atUnit CT.RightClickOn

-- | arg1 follow arg2
follow  :: UnitId -> UnitId -> Command
follow              = atUnit CT.Follow

-- | I am not sure
setRallyUnit  :: UnitId -> UnitId -> Command
setRallyUnit        = atUnit CT.SetRallyUnit

-- | arg1 repair arg2
repair  :: UnitId -> UnitId -> Command
repair              = atUnit CT.Repair

-- | load arg2 into arg1
load  :: UnitId -> UnitId -> Command
load                = atUnit CT.Load

-- | unload arg2 from arg1
unload  :: UnitId -> UnitId -> Command
unload              = atUnit CT.Unload

-- | arg1 use techType arg2 on unit arg3
useTechOn :: UnitId -> TechType -> UnitId -> Command
useTechOn           = withTechTypeAtUnit CT.UseTechOn

-- | arg1 train an instance of 'UnitType' arg2
train :: UnitId -> UnitType -> Command
train               = atUnitType CT.Train

-- | arg1 build addon structure arg2
buildAddon :: UnitId -> UnitType -> Command
buildAddon          = atUnitType CT.BuildAddon

-- | arg1 change into UnitType arg2
morph :: UnitId -> UnitType -> Command
morph               = atUnitType CT.Morph

-- | Use arg1 to build at arg2 unit arg3
build :: UnitId -> Location -> UnitType -> Command
build               = atLocationWithUnitType CT.Build

-- | Use arg1 to perform tech arg2 at arg3
useTechAt :: UnitId -> TechType -> Location -> Command
useTechAt           = withTechTypeAtLocation CT.UseTechAt

-- | Arg1 research tech arg2
research :: UnitId -> TechType -> Command
research            = atTechType CT.Research

-- | Arg1 use tech arg2
useTech :: UnitId -> TechType -> Command
useTech             = atTechType CT.UseTech

-- | Arg1 research upgrade arg2
upgrade :: UnitId -> UpgradeType -> Command
upgrade             = atUpgradeType CT.Upgrade

-- | Arg1 stop your current action
stop :: UnitId -> Command
stop                = atNothing CT.Stop

-- | Arg1 hold position. Different to stop, unit will not perform any action.
holdPosition :: UnitId -> Command
holdPosition        = atNothing CT.HoldPosition

-- | Arg1 burrow
burrow :: UnitId -> Command
burrow              = atNothing CT.Burrow

-- | Arg1 unburrow
unburrow :: UnitId -> Command
unburrow            = atNothing CT.Unburrow

-- | Arg1 enter seige mode
seige :: UnitId -> Command
seige               = atNothing CT.Seige

-- | Arg1 exit seige mode
unseige :: UnitId -> Command
unseige             = atNothing CT.Unseige

-- | Arg1 cloak
cloak :: UnitId -> Command
cloak               = atNothing CT.Cloak

-- | Arg1 decloak
decloak :: UnitId -> Command
decloak             = atNothing CT.Decloak

-- | I am not sure
lift :: UnitId -> Command
lift                = atNothing CT.Lift

-- | Arg1 unload all units being carried 
unloadAll :: UnitId -> Command
unloadAll           = atNothing CT.UnloadAll

-- | Arg1 Cancel construction. I assume that this destroys the building and returns its resources
cancelConstruction :: UnitId -> Command
cancelConstruction  = atNothing CT.CancelConstruction

-- | Arg1 pause constructing the building
haltConstruction :: UnitId -> Command
haltConstruction    = atNothing CT.HaltConstruction

-- | Arg1 stop the morphing process
cancelMorph :: UnitId -> Command
cancelMorph         = atNothing CT.CancelMorph

-- | Arg1 stop training the unit you are currently training
cancelTrain :: UnitId -> Command
cancelTrain         = atNothing CT.CancelTrain

-- | Arg1 cancel building the addon you are building
cancelAddon :: UnitId -> Command
cancelAddon         = atNothing CT.CancelAddon

-- | Arg1 cancel the research you are currently performing
cancelResearch :: UnitId -> Command
cancelResearch      = atNothing CT.CancelResearch

-- | Arg1 cancel the upgrade you are currently researching
cancelUpgrade :: UnitId -> Command
cancelUpgrade       = atNothing CT.CancelUpgrade

-- | I am not sure
cancelTrainOn :: UnitId -> Slot -> Command
cancelTrainOn       = atSlot CT.CancelTrainOn

-- | Set the game speed to be higher
setGameSpeed :: Int -> Command
setGameSpeed        = setValue CT.SetGameSpeed

-- * Command preparation
-- ** Arguments are prepended to a string which contains 0's in the other arguement spaces delimited by ';' 

-- | Seperator for arguments
s :: [Char] -> CommandArgs
s = (';':)

-- | Function to append a single argument 
args1 :: (Serialize a) => a -> CommandArgs
args1 a = serializes a ";0;0;0"

-- | Function to append 2 argument 
args2 :: (Serialize a, Serialize b) => a -> b -> CommandArgs
args2 a b = serializes a . s $ serializes b ";0;0" 

-- | Function to append 3 arguments
args3 :: (Serialize a, Serialize b, Serialize c) => a -> b -> c -> CommandArgs
args3 a b c = serializes a . s . serializes b . s $ serializes c ";0"

-- | Function to specify all 4 arguments
args4 :: (Serialize a, Serialize b, Serialize c ,Serialize d) => a -> b -> c -> d -> CommandArgs
args4 a b c d = serializes a . s . serializes b . s . serializes c . s $ serializes d ""

---------------------------------------------------------------
-- * Command Signatures
---------------------------------------------------------------

atLocation                                          :: CT.CommandType -> UnitId -> Location -> Command
atLocation ct unitId (x, y)                         = Command ct $ args3 unitId x y

atUnit                                              :: CT.CommandType -> UnitId -> UnitId -> Command
atUnit ct unitId                                    = Command ct . args2 unitId

withTechTypeAtUnit                                  :: CT.CommandType -> UnitId -> TechType -> UnitId -> Command
withTechTypeAtUnit ct unitId techType               = Command ct . args3 unitId techType

atUnitType                                          :: CT.CommandType -> UnitId -> UnitType -> Command
atUnitType ct unitId                                = Command ct . args2 unitId

atLocationWithUnitType                              :: CT.CommandType -> UnitId -> Location -> UnitType -> Command
atLocationWithUnitType ct unitId (x, y)             = Command ct . args4 unitId x y

withTechTypeAtLocation                              :: CT.CommandType -> UnitId -> TechType -> Location -> Command
withTechTypeAtLocation ct unitId techType (x, y)    = Command ct $ args4 unitId techType x y

atTechType                                          :: CT.CommandType -> UnitId -> TechType -> Command
atTechType ct unitId                                = Command ct . args2 unitId

atUpgradeType                                       :: CT.CommandType -> UnitId -> UpgradeType -> Command
atUpgradeType ct unitId                             = Command ct . args2 unitId

atNothing                                           :: CT.CommandType -> UnitId -> Command
atNothing ct                                        = Command ct . args1

atSlot                                              :: CT.CommandType -> UnitId -> Slot -> Command
atSlot ct unitId                                    = Command ct . args2 unitId

setValue                                            :: CT.CommandType -> Int -> Command
setValue ct                                         = Command ct . args1
