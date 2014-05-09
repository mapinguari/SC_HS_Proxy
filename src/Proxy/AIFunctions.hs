module Proxy.AIFunctions (onStart, onFrame, AnalysedGameInfo) where
import Proxy.Server.Messages (GameState,GameInfo)
import Proxy.Helpers
import Proxy.Types.Orders
import Proxy.Server.Messages
import Proxy.Types.Game
import Proxy.Query.Unit
import Proxy.Commands
import Proxy.PreProcessing.MapDecomposition
import Control.Monad.State
import Proxy.Types.CommandTypes
import Data.Ord (comparing)
import Data.Array
import Control.Comonad
import Control.DeepSeq
import Data.Graph.Inductive.Graph
import Proxy.Math.Rectangle

--larger Number most urgent.
type PriorityVal = Int

instance NFData (Rectangle a) where 
  rnf r = ()

data AnalysedGameInfo = AGI { players :: [(Player,Position)], battlefield :: MapData , terrain :: Maybe (ChokeData,BaseData)}

data AIState = No_Thing | AI {step :: Int, interval :: (Int -> Int),toDo :: (GameState -> [Command])}

onStart :: GameInfo -> AnalysedGameInfo
onStart (GameInfo p l m t) = deepseq (labNodes.mapToGraph.tiles $ m)  ( AGI (zip p l) m t)

onFrame :: AnalysedGameInfo -> GameState -> [GameState] -> Maybe b -> ([Command], Maybe b)
onFrame onStartData gameState history aiState = ([],Nothing)
  {-
  let
  bigCalc = deepseq (airAndGroundPotential (tiles.battlefield $ onStartData) (gameUnits gameState)) 1
  pInfo = map fst . players $ onStartData
  myId = playerId.getMyInfo $ pInfo
  enemyLocation = last $ map snd (players onStartData)
  myUnits = filter (isOwnedBy myId) $ gameUnits gameState
  commands = map (uncurry attackMove) $ zip (map unitId myUnits) (repeat enemyLocation)
            
{-
                                                --players = getPlayers onStartData
                                                players' = players onStartData
                                                enemyLocations = map snd.filter (not.isMe.fst) $ players'
                                                firstEnemyLocation = head enemyLocations
                                                myUnits = getUnitsOf (playerId myInfo) units
                                                units   = gameUnits gameState
                                                
                                                myInfo = map fst .head.filter (isMe.fst) $ players'
                                                
                                                myInfo  = getMyInfo players
                                       
                                        
                                                myWorkers = filter (isWorker . unitType) myUnits
                                                myWorker = unitType.head $ myWorkers
                                                myBoredUnits = getUnitsWithOrder PlayerGuard myWorkers
                                                minerals = getMinerals units
                                                pairs = matchWith closest myBoredUnits  minerals
                                                pairIds = map (\(a,b) -> (unitId a, unitId b))
                                                --commands = (map (uncurry rightClickOn) (pairIds pairs))
-}
                                                
                                             in (commands, aiState)

-}