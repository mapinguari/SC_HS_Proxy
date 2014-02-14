module Proxy.Query.Tech where
import Proxy.Types.UnitTypes
import Proxy.Types.TechTypes

whatResearches :: TechType -> UnitType
-- ^ Returns the unit which researches the arguement Tech
whatResearches Stimpacks = TerranAcademy
whatResearches Lockdown = TerranCovertOps
whatResearches EMPShockwave = TerranScienceFacility
whatResearches SpiderMines = TerranMachineShop
whatResearches ScannerSweep = NoneUnitType
whatResearches TankSiegeMode = TerranMachineShop
whatResearches DefensiveMatrix = NoneUnitType
whatResearches Irradiate = TerranScienceFacility
whatResearches YamatoGun = TerranPhysicsLab
whatResearches CloakingField = TerranControlTower
whatResearches PersonnelCloaking = TerranCovertOps
whatResearches Burrowing = ZergHatchery
whatResearches Infestation = NoneUnitType
whatResearches SpawnBroodlings = ZergQueensNest
whatResearches DarkSwarm = NoneUnitType
whatResearches Plague = ZergDefilerMound
whatResearches Consume = ZergDefilerMound
whatResearches Ensnare = ZergQueensNest
whatResearches Parasite = NoneUnitType
whatResearches PsionicStorm = ProtossTemplarArchives
whatResearches Hallucination = ProtossTemplarArchives
whatResearches Recall = ProtossArbiterTribunal
whatResearches StasisField = ProtossArbiterTribunal
whatResearches ArchonWarp = NoneUnitType
whatResearches Restoration = NoneUnitType
whatResearches DisruptionWeb = ProtossFleetBeacon
whatResearches MindControl = ProtossTemplarArchives
whatResearches DarkArchonMeld = NoneUnitType
whatResearches Feedback = NoneUnitType
whatResearches OpticalFlare = TerranAcademy
whatResearches Maelstorm = ProtossTemplarArchives
whatResearches LurkerAspect = ZergHydraliskDen
whatResearches Healing = NoneUnitType
whatResearches NoneTechType = NoneUnitType
whatResearches UnknownTechType = NoneUnitType
whatResearches NuclearStrike = NoneUnitType

mineralsCost :: (Num a) => TechType -> a
-- ^ The mineral cost to research a tech upgrade
mineralsCost Stimpacks = 100
mineralsCost Lockdown = 200
mineralsCost EMPShockwave = 200
mineralsCost SpiderMines = 100
mineralsCost ScannerSweep = 0
mineralsCost TankSiegeMode = 150
mineralsCost DefensiveMatrix = 0
mineralsCost Irradiate = 200
mineralsCost YamatoGun = 100
mineralsCost CloakingField = 150
mineralsCost PersonnelCloaking = 100
mineralsCost Burrowing = 100
mineralsCost Infestation = 0
mineralsCost SpawnBroodlings = 100
mineralsCost DarkSwarm = 0
mineralsCost Plague = 200
mineralsCost Consume = 100
mineralsCost Ensnare = 100
mineralsCost Parasite = 0
mineralsCost PsionicStorm = 200
mineralsCost Hallucination = 150
mineralsCost Recall = 150
mineralsCost StasisField = 150
mineralsCost ArchonWarp = 0
mineralsCost Restoration = 100
mineralsCost DisruptionWeb = 200
mineralsCost MindControl = 200
mineralsCost DarkArchonMeld = 0
mineralsCost Feedback = 0
mineralsCost OpticalFlare = 100
mineralsCost Maelstorm = 100
mineralsCost LurkerAspect = 200
mineralsCost Healing = 0
mineralsCost NoneTechType = 0
mineralsCost UnknownTechType = 0
mineralsCost NuclearStrike = 0

gasCost :: (Num a) => TechType -> a
-- ^ The gas cost to research a tech upgrade

gasCost Stimpacks = 100
gasCost Lockdown = 200
gasCost EMPShockwave = 200
gasCost SpiderMines = 100
gasCost ScannerSweep = 0
gasCost TankSiegeMode = 150
gasCost DefensiveMatrix = 0
gasCost Irradiate = 200
gasCost YamatoGun = 100
gasCost CloakingField = 150
gasCost PersonnelCloaking = 100
gasCost Burrowing = 100
gasCost Infestation = 0
gasCost SpawnBroodlings = 100
gasCost DarkSwarm = 0
gasCost Plague = 200
gasCost Consume = 100
gasCost Ensnare = 100
gasCost Parasite = 0
gasCost PsionicStorm = 200
gasCost Hallucination = 150
gasCost Recall = 150
gasCost StasisField = 150
gasCost ArchonWarp = 0
gasCost Restoration = 100
gasCost DisruptionWeb = 200
gasCost MindControl = 200
gasCost DarkArchonMeld = 0
gasCost Feedback = 0
gasCost OpticalFlare = 100
gasCost Maelstorm = 100
gasCost LurkerAspect = 200
gasCost Healing = 0
gasCost NoneTechType = 0
gasCost UnknownTechType = 0
gasCost NuclearStrike = 0

