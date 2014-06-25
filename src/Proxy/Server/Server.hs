module Proxy.Server.Server (run,send) where
import Network
import System.IO
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import qualified Settings 
import Proxy.Server.Messages
import Proxy.Server.Parsers
import System.Time
import Proxy.Server.Log
import Proxy.AI.AIControl

import Text.Parsec.String (Parser)
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

server :: StartCalculation -> FrameCalculation a -> Socket -> IO ()
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

run ::  StartCalculation -> FrameCalculation -> IO ()
run onStart onFrame = withSocketsDo $ bracket (listenOn Settings.port) (sClose) (server onStart onFrame)
