 -- | some AI setup options including server port and history length
module Proxy.Settings where
import Network

-------REMOTE SETTINGS

-- | Allow the user to interact with the game alongside the AI
allowUserControl :: Bool
allowUserControl = True

-- | Currently this gives the bot access to all unit data and upgrade data, opponent supply and resource data not available yet.
allowCompleteInformation :: Bool
allowCompleteInformation = False

-- | Print commands to console running the proxy
printCommandsToConsole :: Bool
printCommandsToConsole = True

-- | Perform terrain analysis at start
performTerrainAnalysis :: Bool
performTerrainAnalysis = False

------LOCAL SETTINGS-----------

--absolute path only atm
logDir :: String
logDir = "/home/mapinguari/Project/Logs/"

--absolute path atm 
mapDataBaseDir :: String
mapDataBaseDir = "/home/mapinguari/Project/MapDB/"


-- | Number of previous game states to be passed to the next gameState
historyLength :: Int
historyLength = 1

-- | Port to listen on for BWAPI connection
port :: PortID
port = PortNumber 12345