module Main where
import Proxy.Helpers
import Proxy.Server.Server
import Proxy.Types.Game
import Proxy.Server.Messages
import Proxy.Commands
import Proxy.Types.Orders
import Proxy.Query.Unit
import Debug.Trace (trace)

onStart = id

-- Utilize some concurrent shit on units currently with actions

onFrame onStartData gameState history myState = let
                                                players = gamePlayers onStartData
                                                myInfo  = getMyInfo players
                                                units   = gameUnits gameState
                                                myUnits = getUnitsOf (playerId myInfo) units
                                                myWorkers = filter (isWorker . unitType) myUnits
                                                myWorker = unitType.head $ myWorkers
                                                myBoredUnits = getUnitsWithOrder PlayerGuard myWorkers
                                                minerals = getMinerals units
                                                pairs = matchWith closest myBoredUnits  minerals
                                                pairIds = map (\(a,b) -> (unitId a, unitId b))
                                                commands = (map (uncurry rightClickOn) (pairIds pairs))
                                             in (commands, myState)

main = Proxy.Server.Server.run onStart onFrame
