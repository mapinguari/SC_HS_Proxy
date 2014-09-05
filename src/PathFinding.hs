{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module PathFinding where
import Proxy.Math.Graph
import Proxy.Math.InfNumber as I
import Proxy.Server.Messages
import Proxy.Command.Commands
import Proxy.PathFinding.Specification
import Proxy.Types.Game
import Proxy.Query.Unit
import Data.Maybe
import Data.List
import qualified Data.Map as M
import Data.Array
import Proxy.AI.AIControl
import Proxy.PathFinding.HinzAStar
import Proxy.AI.Helpers

data GridGraph a b = GG {ggraph :: WLAGraph a b, for :: a -> Node, back :: Node -> a}  

instance Graph GridGraph where
  size = size . ggraph
  arc = arc . ggraph
  
instance (Real b) => WeightedGraph GridGraph b where
  weight = weight . ggraph
  
instance LabelledGraph GridGraph a where 
  label (GG g f a) = a

boundedEightWay :: (Position,Position) -> Position -> [Position]
boundedEightWay b = filter (inRange b) . eightWay 

eightWay :: (Num a,Eq a,Ix a) => (a,a) -> [(a,a)]
eightWay (x,y) = filter (/= (x,y)) (range ((x-1,y-1),(x+1,y+1)))

posToNode :: Int -> (Int,Int) -> Node
posToNode w (x,y) = w*y + x

nodeToPos :: Int -> Int -> (Int,Int)
nodeToPos w n = (n `rem` w , n `div` w)

euclD :: (Int,Int) -> (Int,Int) -> Float
euclD (x,y) (z,w) = sqrt . fromIntegral $ (x - z) ^ 2 + (y - w) ^ 2

buildGG :: [[Tile]] -> [(Location,Location)] -> GridGraph Location (InfNumbers Float)
buildGG tss le = GG graph (posToNode w) (nodeToPos w)
  where w = length.head $ tss
        h = length tss
        e = map (\(x,y) -> (posToNode w x, posToNode w y)) $ filter (\(x,y) -> inRange gridBounds x && inRange gridBounds y) le
        gridBounds = ((0,0),(w-1,h-1))
        graph = WLA $ accumArray (flip (:)) [] (0,posToNode w (w-1,h-1)) (struct ++ changes)
        grid = listArray gridBounds (map walkable . concat $ tss)
        struct = map dit $ filter (\(x,y) -> grid ! x && grid ! y) $ concatMap getEdges (range gridBounds)
        getEdges x = zip (repeat x) (boundedEightWay gridBounds x)
        dit (x,y) = (posToNode w x,(F (euclD x y), posToNode w y)) 
        changes = map (\(n,m) -> (n,(Inf,m))) e
        


getChanges :: [UnitData] -> [(Location,Location)]
getChanges = concatMap (\u -> let (x,y) = unitLocation u
                                  w = tileWidth . unitType $ u
                                  h = tileHeight . unitType $ u in   
                              zip (repeat (x,y)) (range ((x,y), (x + w - 1, y + h - 1))))

data AIState = AI {mapbuilder ::[(Location,Location)] -> GridGraph Location (InfNumbers Float), currentPaths :: OldPaths}

forAnyPathFinder :: (GridGraph Location (InfNumbers Float) -> Node -> Node -> Maybe Path) ->  FrameCalculation AIState
forAnyPathFinder pathfinder onStartData gameState history aiState = (pathCommands ++ newPathCommands ,Just $AI mapBuilder (continuingPaths ++ newContinuingPaths))
  where (pathCommands,continuingPaths,rePlanningRequired) = bFrame graph gameState oldPaths
        (newPathCommands, newContinuingPaths) = getPaths graph pathfinder (assign myId unitData ++ rePlanningRequired)
        oldPaths = maybe [] (currentPaths . id) aiState
        graph = mapBuilder (getChanges . gameUnits $ gameState)
        mapBuilder = maybe (buildGG mapData) mapbuilder aiState
        mapData = tiles . gameMap $ onStartData
        unitData = gameUnits gameState
        myId = playerId . playerInfo . getMe . gamePlayers $ onStartData
        
onFrame :: FrameCalculation AIState
onFrame = forAnyPathFinder realTime
  where realTime g n1 n2 = rtaStarSearch g 100 n1 n2 (euclD n2)
        
assign :: PlayerId -> [UnitData] -> [(UnitId,Node,Node)]
assign myId uds = map (\ud -> (unitId ud, unitLocation ud, 700)) uds
  where (myUnits,enemyUnits) = partition ((==myId).unitOwnerId) uds
        

getPaths :: GridGraph Location (InfNumbers Float) -> (GridGraph Location (InfNumbers Float) -> Node -> Node -> Maybe Path) -> [(UnitId,Node,Node)] -> (NewCommands, PathsContinuing)
getPaths graph pathfinder uds = filter (isJust.snd)
  where unitPathPair = map (\(uid,n,m) -> (uid,pathFinder graph n m)) uds
        end = map (\(uid,maybePath) -> case maybePath of
                      Nothing -> Nothing
                      Just p -> Just (rightClickAt uid (back graph (safeOrigin (dropOrigin p))), (uid,dropOrigin p))) unitPathPair

type OldPaths = [(UnitId,Path)]
type NewCommands = [Command]
type PathsContinuing = [(UnitId,Path)]
type UnitsRequiringRePlanning = [(UnitId,Node,Node)]


bFrame :: GridGraph Position (InfNumbers Float) -> GameState -> OldPaths -> (NewCommands, PathsContinuing,UnitsRequiringRePlanning)
bFrame g gS xs = (catMaybes maybeCommands, continueOnPath, map (\(uid,p) -> (uid,safeOrigin p, safeDestination p)) toReplan)
  where unitMap = M.fromList (zip (map unitId (gameUnits gS)) (gameUnits gS))
        stillConsider = filter (\(uid,p) -> origin p /= destination p) xs
        (toReplan,noReplanYet) = partition ((not . I.isInfinite) . (pc g) . snd) $ stillConsider
        (maybeCommands,continueOnPath) = unzip (map helper noReplanYet)        
        helper (uid,p) = if unitLocation (unitMap M.! uid) == label g (safeOrigin p)
                           then (Just (rightClickAt uid (label g (safeOrigin (dropOrigin p)))), (uid,dropOrigin p))
                           else (Nothing, (uid,p)) 