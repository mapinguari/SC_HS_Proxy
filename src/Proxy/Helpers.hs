
-- | Some useful functions
module Proxy.Helpers where
import Proxy.Types.Game
import Proxy.Server.Messages
import qualified Proxy.Types.Orders as Orders
import qualified Proxy.Query.Unit as Unit
import Proxy.Types.UnitTypes

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

distanceFrom :: (Floating a) =>UnitData -> UnitData -> a
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
