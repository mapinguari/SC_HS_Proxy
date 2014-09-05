module AIFunctions (onStart, onFrame,potFieldAllAttack) where
import Proxy.AI.AIControl
import Proxy.PotentialFields.Internal.SimpleActions
import Proxy.AI.Helpers
import Proxy.Server.Messages
import Proxy.Types.Game
import Proxy.Types.Orders
import Data.List
import Debug.Trace
import qualified Data.Map as M

onStart :: StartCalculation
onStart = id


potFieldAllAttack :: FrameCalculation (M.Map UnitId [Location])
potFieldAllAttack gi gs his state = (allAttack, newState)
  where allAttack = allActions unitActions fieldE
        unitActions = map (flip attackTowards (unitLocation . head $ enemyUnits)) myFreeUnits
        myFreeUnits = getUnitsWithOrder PlayerGuard myUnits
        obstacles = getMap . tiles . gameMap $ gi
        me = getMe . gamePlayers $ gi
        myId = playerId . playerInfo . getMe . gamePlayers $ gi
        allUnits = gameUnits gs
        (myUnits,enemyUnits) = partition ((==myId).unitOwnerId) allUnits
        fieldE = (obstacles, myUnits, enemyUnits)
        newState = addLocations gs state
        
getMap :: [[Tile]] -> [Location]
getMap = (\x -> mapPerimeter x ++ getObstacles x)
        
mapPerimeter :: [[Tile]] -> [Location]
mapPerimeter xss = zip (repeat (negate 1)) [-1..h] ++ zip (repeat w) [-1..h] ++ zip [0..(w-1)] (repeat (negate 1)) ++ zip [0..(w-1)] (repeat h)
  where h = length xss
        w = length.head $ xss
        
getObstacles :: [[Tile]] -> [Location]
getObstacles xss = [(i,j)| (j,xs) <- zip [0..] xss, (i,x) <- zip [0..] xs, walkable x]


addLocations :: GameState -> Maybe (M.Map UnitId [Location]) -> Maybe (M.Map UnitId [Location])
addLocations gs Nothing = Just $ M.fromList luids
  where luids = map (\ud -> (unitId ud,[unitLocation ud])) . gameUnits $ gs
addLocations gs (Just m) = Just $ foldr (\(uid,l) m0 -> M.adjust (l :) uid m0) m luids
  where luids = map (\ud -> (unitId ud,unitLocation ud)) . gameUnits $ gs

  

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