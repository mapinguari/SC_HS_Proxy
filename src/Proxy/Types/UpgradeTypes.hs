module Proxy.Types.UpgradeTypes where
-- ^ Module contains all upgradetypes for all species

data UpgradeType = TerranInfantryArmor
                 | TerranVehiclePlating
                 | TerranShipPlating
                 | ZergCarapace
                 | ZergFlyerCaparace
                 | ProtossArmor
                 | ProtossPlating
                 | TerranInfantryWeapons
                 | TerranVehicleWeapons
                 | TerranShipWeapons
                 | ZergMeleeAttacks
                 | ZergMissileAttacks
                 | ZergFlyerAttacks
                 | ProtossGroundWeapons
                 | ProtossAirWeapons
                 | ProtossPlasmaShields
                 | U238Shells
                 | IonThrusters
                 | BurstLasers
                 | TitanReactor
                 | OcularImplants
                 | MoebiusReactor
                 | ApolloReactor
                 | ColossusReactor
                 | VentralSacs
                 | Antennae
                 | PneumatizedCarapace
                 | MetabolicBoost
                 | AdrenalGlands
                 | MuscularAugments
                 | GroovedSpines
                 | GameteMeiosis
                 | MetasynapticNode
                 | SingularityCharge
                 | LegEnhancements
                 | ScarabDamage
                 | ReaverCapacity
                 | GraviticDrive
                 | SensorArray
                 | GraviticBoosters
                 | KhaydarinAmulet
                 | ApialSensors
                 | GraviticThrusters
                 | CarrierCapacity
                 | KhaydarinCore
                 | UnusedUpgrade45
                 | UnusedUpgrade46
                 | ArgusJewel
                 | UnusedUpgrade48
                 | ArgusTalisman
                 | UnusedUpgrade50
                 | CaduceusReactor
                 | ChitinousPlating
                 | AnabolicSynthesis
                 | CharonBooster
                 | UnusedUpgrade55
                 | UnusedUpgrade56
                 | UnusedUpgrade57
                 | UnusedUpgrade58
                 | UnusedUpgrade59
                 | GlobalUpgrade60
                 | NoneUpgradeType
                 | UnknownUpgradeType deriving (Enum, Show)
