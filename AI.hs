module Main where
import Helpers
import Proxy.Server
import Proxy.Game
import Proxy.Messages
import Proxy.Commands
import Proxy.Orders
import Proxy.Unit
import Proxy.UnitTypes

onStart = id

onFrame onStartData gameState history myState = let
                                                players = gamePlayers onStartData
                                                myInfo  = getMyInfo players
                                                units   = gameUnits gameState
                                                myUnits = getUnitsOf (playerId myInfo) units
                                                myWorkers = filter (isWorker . unitType) myUnits
                                                myWorker = unitType.head $ myWorkers
                                                myBoredUnits = getUnitsWithOrder PlayerGuard myWorkers
                                                myCommandCenter = filter (isBuilding.unitType) myUnits
                                                probing = train (unitId.head $ myCommandCenter) myWorker
                                                
                                                minerals = getMinerals units
                                                pairs = matchWith closest myBoredUnits  minerals
                                                pairIds = map (\(a,b) -> (unitId a, unitId b))
                                                commands =probing : (map (uncurry rightClickOn) (pairIds pairs))
                                             in (commands, myState)

main = Proxy.Server.run onStart onFrame
