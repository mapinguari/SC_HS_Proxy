-- | some AI setup options including server port and history length
module Proxy.Settings where
import Network

-- | Number of previous game states to be passed to the next gameState
historyLength :: Int
historyLength = 1

-- | Port to listen on for BWAPI connection
port :: PortID
port = PortNumber 12345

-- | Allow the user to interact with the game alongside the AI
allowUserControl :: Bool
allowUserControl = True

-- | Allow the AI access to all game data
allowCompleteInformation :: Bool
allowCompleteInformation = False

-- | Print commands to console running the proxy
printCommandsToConsole :: Bool
printCommandsToConsole = True

-- | Perform terrain analysis at start
performTerrainAnalysis :: Bool
performTerrainAnalysis = False
