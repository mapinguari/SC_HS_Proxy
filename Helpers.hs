-- | Some useful functions
module Helpers where
import Proxy.Game
import Proxy.Messages
import qualified Proxy.Orders as Orders
import qualified Proxy.Unit as Unit
import Proxy.UnitTypes

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

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = ceiling (sqrt (dx**2 + dy**2)) where
    dx = fromIntegral $ x1 - x2
    dy = fromIntegral $ y1 - y2

distanceFrom :: UnitData -> UnitData -> Int
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
