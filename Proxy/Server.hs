module Proxy.Server
  (run)
where
import Network
import System.IO
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import qualified Proxy.Settings as Settings
import Proxy.Messages
import Proxy.Commands
import Proxy.Parsers
import Proxy.Game
import Proxy.Paul.Terrain


botOptions = Options [Settings.allowUserControl,
                      Settings.allowCompleteInformation,
                      Settings.printCommandsToConsole,
                      Settings.performTerrainAnalysis]

---------------------------
-- Network functions
---------------------------

send (handle, _, _) message = do
                              hPutStrLn handle (serialize message)
                              hFlush handle

receive (handle, _, _) m = do
                           line <- hGetLine handle
                           return $ parseMessage m line

---------------------------
-- Startup operations
---------------------------

receiveTerrain connection = if Settings.performTerrainAnalysis
                            then do
                                 chokeData <- receive connection chokeData
                                 baseData  <- receive connection baseData
                                 return $ Just (chokeData, baseData)
                            else return Nothing

startup connection = do
                     players <- receive connection ack
                     print players
                     send connection botOptions
                     print botOptions
                     startingLocations <- receive connection startingLocations
                     maping <- receive connection mapData
                     terrainData <- receiveTerrain connection
                     return $ GameInfo players startingLocations maping terrainData


---------------------------
-- AI Synchronization
---------------------------

aiThread stateVar commVar onStartData onFrame = aiLoop [] Nothing where
    aiLoop history aiState = do
                             gameState <- takeMVar stateVar
                             let (commands, newAIState) = onFrame onStartData gameState history aiState
                             putMVar commVar commands
                             let newHistory = history `seq` (take Settings.historyLength (gameState : history))
                             aiLoop newHistory newAIState

checkAI commVar = do
                  commVal <- tryTakeMVar commVar
                  makeCommands commVal where
                      makeCommands Nothing   = return (False, Commands [])
                      makeCommands (Just []) = return (True, Commands [])
                      makeCommands (Just cs) = print cs >> return (True, Commands cs)

---------------------------
-- Server loop
---------------------------

loop conn stateVar commVar = do                                       
                             state              <- receive conn gameState
                             (aiDone, commands) <- checkAI commVar
                             send conn commands
                             when aiDone $ putMVar stateVar state
                             loop conn stateVar commVar

---------------------------
-- Server init
---------------------------

firstFrame conn stateVar commVar = do
                                   state  <- receive conn gameState
                                   send conn $ Commands []
                                   putMVar stateVar state

server onStart onFrame socket = do
                                connection <- accept socket
                                gameInfo   <- startup connection
                                print $ makeMap gameInfo
                                stateVar   <- newEmptyMVar
                                commVar    <- newEmptyMVar
                                forkIO $ aiThread stateVar commVar (onStart gameInfo) onFrame
                                firstFrame connection stateVar commVar
                                loop connection stateVar commVar

---------------------------
-- Server entry-point
---------------------------

run :: (GameInfo -> a) -> (a -> GameState -> [GameState] -> Maybe b -> ([Command], Maybe b)) -> IO ()
run onStart onFrame = withSocketsDo $ bracket (listenOn Settings.port) (sClose) (server onStart onFrame)
