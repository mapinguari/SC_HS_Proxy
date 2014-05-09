module Proxy.Server.Server (run,send) where
import Network
import System.IO
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import qualified Proxy.Settings as Settings
import Proxy.Server.Messages
import Proxy.Commands
import Proxy.Server.Parsers
import System.Time
import Proxy.Server.Log
import Text.Parsec.String (Parser)
import Proxy.AIFunctions (AnalysedGameInfo)
import Proxy.Types.Game
import Data.Char

botOptions :: Options
botOptions = Options [Settings.allowUserControl,
                      Settings.allowCompleteInformation,
                      Settings.printCommandsToConsole,
                      Settings.performTerrainAnalysis]
showBotOptions = [ "User Control - " ++ show Settings.allowUserControl,
                   "Complete Information - " ++ show Settings.allowCompleteInformation,
                   "Print Commands Remotely - " ++ show Settings.printCommandsToConsole,
                   "Perform Terrain Analysis Remotely - " ++ show Settings.performTerrainAnalysis]                   

---------------------------
-- Network functions
---------------------------
send :: (Serialize a) => (Handle,HostName,PortNumber) -> a -> IO ()
send (handle, _, _) message = do
                              hPutStrLn handle (serialize message)
                              hFlush handle
                              
receive :: (Handle,HostName,PortNumber) -> Parser a -> IO a
receive (handle, _, _) parser = do
                           line <- hGetLine handle
                           return $ parseMessage parser line

---------------------------
-- Startup operations
---------------------------
receiveTerrain :: (Handle,HostName,PortNumber) -> IO (Maybe (ChokeData,BaseData))
receiveTerrain connection = if Settings.performTerrainAnalysis
                            then do
                                 chokeData <- receive connection chokeData
                                 baseData  <- receive connection baseData
                                 return $ Just (chokeData, baseData)
                            else return Nothing
                                 

startup :: (Handle,HostName,PortNumber) -> IO (GameInfo,Handle)
startup connection@(_,host,_) = do 
                     putStrLn $ "Connection Established - " ++ host
                     putStrLn $ unlines showBotOptions
                     players <- receive connection ack
                     send connection botOptions
                     startingLocations <- receive connection startingLocations
                     maping <- receive connection mapData
                     terrainData <- receiveTerrain connection
                     let gameInfo = GameInfo players startingLocations maping terrainData
                     time <- toCalendarTime =<< getClockTime
                     logHandle <- startLog gameInfo botOptions time
                     return (gameInfo,logHandle)
                     
---------------------------
-- AI Synchronization
---------------------------

aiThread :: MVar GameState -> MVar [Command] -> AnalysedGameInfo -> (AnalysedGameInfo -> GameState -> [GameState] -> Maybe a -> ([Command],Maybe a)) -> IO ()
aiThread stateVar commVar onStartData onFrame = aiLoop [] Nothing where
    aiLoop history aiState = do
                             gameState <- takeMVar stateVar
                             let (commands, newAIState) = onFrame onStartData gameState history aiState
                             putMVar commVar commands
                             let newHistory = history `seq` (take Settings.historyLength (gameState : history))
                             aiLoop newHistory newAIState
                             
checkAI :: MVar [Command] -> IO (Bool,Commands)
checkAI commVar = do
                  commVal <- tryTakeMVar commVar
                  makeCommands commVal where
                      makeCommands Nothing   = return (False, Commands [])
                      makeCommands (Just []) = return (True, Commands [])
                      makeCommands (Just cs) = print cs >> return (True, Commands cs)

---------------------------
-- Server loop
---------------------------
                      
loop :: (Handle, HostName,PortNumber) -> MVar GameState -> MVar [Command] -> IO ()
loop conn stateVar commVar = do                                       
                             state              <- receive conn gameState
                             print state
                             (aiDone, commands) <- checkAI commVar
                             send conn commands
                             when aiDone $ putMVar stateVar state
                             loop conn stateVar commVar

---------------------------
-- Server init
---------------------------

firstFrame :: (Handle, HostName,PortNumber) -> MVar GameState -> MVar [Command] -> IO ()
firstFrame conn stateVar commVar = do
                                   state  <- receive conn gameState
                                   send conn $ Commands []
                                   putMVar stateVar state

server :: (GameInfo -> AnalysedGameInfo) -> (AnalysedGameInfo -> GameState -> [GameState] -> Maybe b -> ([Command], Maybe b)) -> Socket -> IO ()
server onStart onFrame socket = do
                                connection <- accept socket
                                (gameInfo,logHandle) <- startup connection
                                stateVar   <- newEmptyMVar
                                commVar    <- newEmptyMVar
                                forkIO $ aiThread stateVar commVar (onStart gameInfo) onFrame
                                firstFrame connection stateVar commVar
                                loop connection stateVar commVar

---------------------------
-- Server entry-point
---------------------------

run ::  (GameInfo -> AnalysedGameInfo) -> (AnalysedGameInfo -> GameState -> [GameState] -> Maybe b -> ([Command], Maybe b)) -> IO ()
run onStart onFrame = withSocketsDo $ bracket (listenOn Settings.port) (sClose) (server onStart onFrame)
