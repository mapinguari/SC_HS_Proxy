module Proxy.Server.Log where
import System.Time
import System.IO
import Proxy.Types.Game
import Proxy.Server.Messages
import Proxy.Settings

startLog :: GameInfo -> Options -> CalendarTime -> IO Handle
startLog gI bs t = do 
    if null lDir 
      then do logh <- return stdout 
              hPutStr logh startInfo
              return logh
      else do logh <- openFile (lDir ++ logName) WriteMode 
              hPutStr logh startInfo
              return logh
    where lDir = Proxy.Settings.logDir
          startInfo = (mapName.gameMap) gI ++ "-" ++ (show.toClockTime) t ++ "\n" ++ show gI 
          logName = (map spaceToUnderScore .mapName.gameMap) gI ++ "_" ++(show.secondsInDay $ t) ++ "-" ++ show (ctYDay t) ++ "-" ++ show (ctYear t)
          
secondsInDay :: CalendarTime -> Int
secondsInDay ct = ((ctHour ct)*60 + (ctMin ct)) * 60 + ctSec ct

spaceToUnderScore :: Char -> Char
spaceToUnderScore x = if x == ' ' then '_' else x