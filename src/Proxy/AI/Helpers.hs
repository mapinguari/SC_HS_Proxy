module Proxy.Helpers where
import Proxy.Types.Game
import Proxy.Server.Messages
import qualified Proxy.Types.Orders as Orders
import qualified Proxy.Query.Unit as Unit
import Proxy.Types.UnitTypes
import Proxy.Math.Graph
import Data.Array
import Control.Arrow

isMe :: Player -> Bool
isMe (Me _) = True
isMe _      = False

getMe :: [Player] -> Player
getMe players = head $ filter isMe players

getMyInfo :: [Player] -> PlayerInfo
getMyInfo = (\(Me i) -> i) . getMe

isOwnedBy :: PlayerId -> UnitData -> Bool
isOwnedBy playerId = (playerId ==) . unitOwnerId

hasOrder :: Orders.Order -> UnitData -> Bool
hasOrder order = (order ==) . unitOrder

isType :: UnitType -> UnitData -> Bool
isType uType = (uType ==) . unitType

distance :: (Floating a) => (Int, Int) -> (Int, Int) -> a
distance (x1, y1) (x2, y2) = (sqrt (dx**2 + dy**2)) where
    dx = fromIntegral $ x1 - x2
    dy = fromIntegral $ y1 - y2

distanceFrom :: (Floating a) => UnitData -> UnitData -> a
distanceFrom u1 u2 = distance (unitLocation u1) (unitLocation u2)

closer :: UnitData -> UnitData -> UnitData -> UnitData
closer u u1 u2 = if (distanceFrom u u1) < (distanceFrom u u2)
                 then u1
                 else u2

closest :: UnitData -> [UnitData] -> UnitData
closest = foldl1 . closer

matchWith :: (a -> b -> c) -> [a] -> b -> [(a,c)]
matchWith f a b = zip a $ map (\u -> f u b) a

getUnitsOf :: PlayerId -> [UnitData] -> [UnitData]
getUnitsOf = filter . isOwnedBy

getUnitsWithOrder :: Orders.Order -> [UnitData] -> [UnitData]
getUnitsWithOrder = filter . hasOrder

getMinerals :: [UnitData] -> [UnitData]
getMinerals = filter (isType ResourceMineralPatch1)

myUnit :: [Player] -> UnitData -> Bool
myUnit xs uD = unitId uD == myId
  where myId = playerId . playerInfo . head $  filter g xs
        g x = case x of
          Me _ -> True
          otherwise -> False
          
posOfUnit :: UnitData -> [Position]
posOfUnit ud = range (bL ud, tR . bL $ ud) 
  where bL = locToPos . unitLocation
        tR (x,y) = (x + (Unit.tileWidth . unitType $ ud),  y + (Unit.tileHeight . unitType $ ud))
        
locToPos :: Location -> Position 
locToPos (x,y) = (x `div` 8, y `div` 8)

mapToGraph :: (Graph g) => [UnitData] -> [[Tile]] -> g
mapToGraph ud tss = buildGraph (h*w) . map (posToNode *** posToNode) . filter validEdge $ allEdges
  where h = length tss - 1
        w = subtract 1 (length.head $ tss)
        allEdges = concatMap positionEdges . range $ ((0,0),(w,h))
        posToNode (x,y) = x + y * w
        positionEdges p = zip (repeat p) (adjacentTilesInBounds (w,h) p)
        adjacentTilesInBounds (v,w) = filter (inbounds) . mk
        
          where inbounds = inRange ((0,0),(v-1,w-1))
                mk (x,y) = [(x,y+1),(x-1,y),(x+1,y),(x,y-1)]
        validEdge (x,y) = trav ! x && trav ! y
        trav = accumArray (&&) True ((0,0),(w,h)) (zip (concatMap posOfUnit ud) (repeat False) ++ zip [(i,j)| j <- [0..h], i <- [0..w]] (map (walkable) . concat $ tss))
        