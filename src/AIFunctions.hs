module AIFunctions (onStart, onFrame) where
import Proxy.AI.AIControl

onStart :: StartCalculation
onStart = id

onFrame :: FrameCalculation a
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