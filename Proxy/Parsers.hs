module Proxy.Parsers where
import Prelude hiding (id)
import Text.ParserCombinators.Parsec
import Proxy.Game
import Proxy.TechTypes
import Proxy.UnitTypes (UnitType)
import Proxy.UpgradeTypes
import Proxy.Messages
import Proxy.Orders (Order)

id :: Parser Int
id = int

stringToRace :: String -> Race -> Parser Race
stringToRace s r = do
                   string s
                   return r

race :: Parser Race
race = foldl1 (<|>) $ map (uncurry stringToRace)
                          [("Zerg", Zerg)
                          ,("Terran", Terran)
                          ,("Protoss", Protoss)
                          ,("Random", RandomRace)
                          ,("Other", OtherRace)
                          ,("None", NoneRace)
                          ,("Unknown", UnknownRace)
                          ]
name :: Parser Name
name = many1 (noneOf ";:")

bool :: Parser Bool
bool = do
       i <- digit
       return $ toEnum (read [i])

pType :: Parser PlayerType
pType = do
        i <- id
        return $ toEnum i

player :: PlayerId -> Parser Player
player myId = do
              (i, r, n, t, a) <- args5 id race name pType bool
              let p = if myId == i then Me
                      else if a then Ally
                           else Enemy
              return . p $ PlayerInfo i r n t

ackHeader :: Parser Int
ackHeader = do
            (_, i) <- args2 (string "NewGame") id
            groupDelim
            return i

ack :: Parser Ack
ack = do
      myId <- ackHeader
      player myId `sepBy1` groupDelim

locationHeader :: Parser Char
locationHeader = string "Locations" >> groupDelim

location :: Parser Location
location = args2 int int

startingLocations :: Parser [Location]
startingLocations = locationHeader >> location `sepBy1` groupDelim

mapHeader :: Parser ([[Tile]] -> Map)
mapHeader = groupCon3 Map name int int

tile :: Parser Tile
tile = do
       d <- digit
       b <- bool
       w <- bool
       return $ Tile (read [d]) b w

tileline :: Int -> Parser [Tile]
tileline n = do 
             line <- count n tile
             return line

mapData :: Parser Map
mapData = do
            mapName <- name
            groupDelim
            mapWidth <- int
            groupDelim
            mapHeight <- int
            groupDelim
            let m = Map mapName mapWidth mapHeight
            tiles <- many (tileline mapWidth)
            return $ m tiles

chokeHeader :: Parser Char
chokeHeader = string "Chokes" >> groupDelim

choke :: Parser Choke
choke = argCon2 Choke (args2 int int) int

chokeData :: Parser [Choke]
chokeData = chokeHeader >> choke `sepBy` groupDelim

baseData :: Parser [Location]
baseData = string "Bases" >> groupDelim >> location `sepBy` groupDelim

resources :: Parser Resources
resources = argCon2 Resources int int

supply :: Parser Supply
supply = argCon2 Supply int int

research :: Parser [(TechType, TechStatus)]
research = do
           digits <- many digit
           let researchTypes = map toEnum (take (length digits) [0..])
           let researchStatuses = map (\x -> toEnum (read [x])) digits
           return $ zip researchTypes researchStatuses

upgrade :: Parser [(UpgradeType, UpgradeStatus)]
upgrade = do
          digits <- many digit
          let upgradeTypes = map toEnum (take (length digits) [0..])
          let upgradeStatuses = map (\x -> toEnum (read [x])) digits
          return $ zip upgradeTypes upgradeStatuses

time :: Parser TimeData
time = argCon5 TimeData int int int int int

order :: Parser Order
order = do
        oId <- id
        return $ toEnum oId

uType :: Parser UnitType
uType = do
        typeId <- id
        return $ toEnum typeId


unitIds :: Parser (Int, Int)
unitIds = args2 id id

stats :: Parser (Int, Int, Int)
stats = args3 int int int

unit :: Parser UnitData
unit = do
       (uId, playerId)           <- unitIds
       argDelim
       uType                     <- uType
       argDelim
       loc                       <- location
       argDelim
       (health, shields, energy) <- stats
       argDelim
       timeData                  <- time
       argDelim
       order                     <- order
       argDelim
       resources                 <- int
       argDelim
       addonId                   <- int
       argDelim
       mines                     <- int
       (return $ UnitData uId playerId uType loc health shields energy timeData order resources addonId mines) 
        
gameState :: Parser GameState
gameState = do
            char 's'
            argDelim
            (rs, s, rh, u) <- args4 resources supply research upgrade
            groupDelim
            unitData <- unit `sepBy` groupDelim
            return $ GameState rs s rh u unitData

-- ** Running a parser 

parseMessage p m = case (parse p "" m) of
                   Left err -> error $ show err
                   Right x  -> x

-- * Parser Internals

groupDelim :: Parser Char
groupDelim = char ':'

argDelim :: Parser Char
argDelim = char ';'

tupleSepBy2 :: Parser Char -> Parser a -> Parser b -> Parser (a,b)
tupleSepBy2 d pA pB = do {vA <- pA; d; vB <- pB; return (vA, vB)}
tupleSepBy3 :: Parser Char -> Parser a -> Parser b -> Parser c -> Parser (a,b,c)
tupleSepBy3 d pA pB pC = do {vA <- pA; d; vB <- pB; d; vC <- pC; return (vA, vB, vC)}
tupleSepBy4 :: Parser Char -> Parser a -> Parser b -> Parser c -> Parser d -> Parser (a,b,c,d)
tupleSepBy4 d pA pB pC pD = do {vA <- pA; d; vB <- pB; d; vC <- pC; d; vD <- pD; return (vA, vB, vC, vD)}
tupleSepBy5 :: Parser Char -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser (a,b,c,d,e)
tupleSepBy5 d pA pB pC pD pE = do {vA <- pA; d; vB <- pB; d; vC <- pC; d; vD <- pD; d; vE <- pE; return (vA, vB, vC, vD, vE)}

args2 :: Parser a -> Parser b -> Parser (a,b)
args2 = tupleSepBy2 argDelim
args3 :: Parser a -> Parser b -> Parser c -> Parser (a,b,c)
args3 = tupleSepBy3 argDelim
args4 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser (a,b,c,d)
args4 = tupleSepBy4 argDelim
args5 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser (a,b,c,d,e)
args5 = tupleSepBy5 argDelim

groups2 :: Parser a -> Parser b -> Parser (a,b)
groups2 = tupleSepBy2 groupDelim
groups3 :: Parser a -> Parser b -> Parser c -> Parser (a,b,c)
groups3 = tupleSepBy3 groupDelim
groups4 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser (a,b,c,d)
groups4 = tupleSepBy4 groupDelim
groups5 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser (a,b,c,d,e)
groups5 = tupleSepBy5 groupDelim

tupleCon2 :: (Parser a -> Parser b -> Parser (a,b)) -> (a -> b -> c) -> Parser a -> Parser b -> Parser c
tupleCon2 sep c a1 a2 = do{(v1, v2) <- sep a1 a2; return $ c v1 v2}
tupleCon3 :: (Parser a -> Parser b -> Parser c -> Parser (a,b,c)) -> (a -> b -> c ->d) -> Parser a -> Parser b -> Parser c -> Parser d
tupleCon3 sep c a1 a2 a3 = do{(v1, v2, v3) <- sep a1 a2 a3; return $ c v1 v2 v3}
tupleCon4 :: (Parser a -> Parser b -> Parser c -> Parser d -> Parser (a,b,c,d)) -> (a -> b -> c ->d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
tupleCon4 sep c a1 a2 a3 a4 = do{(v1, v2, v3, v4) <- sep a1 a2 a3 a4; return $ c v1 v2 v3 v4}
tupleCon5 :: (Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser (a,b,c,d,e)) -> (a -> b -> c ->d -> e -> f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f
tupleCon5 sep c a1 a2 a3 a4 a5 = do{(v1, v2, v3, v4, v5) <- sep a1 a2 a3 a4 a5; return $ c v1 v2 v3 v4 v5}

argCon2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
argCon2 = tupleCon2 args2
argCon3 :: (a -> b -> c ->d) -> Parser a -> Parser b -> Parser c -> Parser d
argCon3 = tupleCon3 args3
argCon4 :: (a -> b -> c ->d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
argCon4 = tupleCon4 args4
argCon5 :: (a -> b -> c ->d -> e -> f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f
argCon5 = tupleCon5 args5

groupCon2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
groupCon2 = tupleCon2 groups2
groupCon3 :: (a -> b -> c ->d) -> Parser a -> Parser b -> Parser c -> Parser d
groupCon3 = tupleCon3 groups3 

int :: Parser Int
int = do
      digits <- many1 digit
      return $ read digits
