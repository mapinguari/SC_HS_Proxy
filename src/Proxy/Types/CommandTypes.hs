-- | This is very similar to 'Orders' this datatype is for issuing commands using commands in 'Commands' where as 'Orders' is used to populate the 'Messages.UnitOrder' field of UnitData in 'Messages'

module Proxy.Types.CommandTypes where

-- | All the commands it is possible for the AI to issue to units
data CommandType = NoneCommand
                 | AttackMove
                 | AttackUnit
                 | RightClickAt
                 | RightClickOn
                 | Train
                 | Build
                 | BuildAddon
                 | Research
                 | Upgrade
                 | Stop
                 | HoldPosition
                 | Patrol
                 | Follow
                 | SetRallyPosition
                 | SetRallyUnit
                 | Repair
                 | Morph
                 | Burrow
                 | Unburrow
                 | Seige
                 | Unseige
                 | Cloak
                 | Decloak
                 | Lift
                 | Land
                 | Load
                 | Unload
                 | UnloadAll
                 | UnloadAllAt
                 | CancelConstruction
                 | HaltConstruction
                 | CancelMorph
                 | CancelTrain
                 | CancelTrainOn
                 | CancelAddon
                 | CancelResearch
                 | CancelUpgrade
                 | UseTech
                 | UseTechAt
                 | UseTechOn
                 | SetGameSpeed deriving (Enum, Show, Eq)
