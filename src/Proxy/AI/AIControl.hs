module Proxy.AI.AIControl where
import Control.Concurrent.MVar
import Proxy.Server.Messages
import Settings

type FrameCalculation aiState = (GameInfo -> GameState -> [GameState] -> Maybe aiState -> ([Command],Maybe aiState))
type StartCalculation = GameInfo -> GameInfo


aiThread :: MVar GameState -> MVar [Command] -> GameInfo -> FrameCalculation a -> IO ()
aiThread stateVar commVar onStartData onFrame = aiLoop [] Nothing 
  where aiLoop history aiState = do
          gameState <- takeMVar stateVar
          let (commands, newAIState) = onFrame onStartData gameState history aiState
          putMVar commVar commands
          let newHistory = history `seq` (take Settings.historyLength (gameState : history))
          aiLoop newHistory newAIState
