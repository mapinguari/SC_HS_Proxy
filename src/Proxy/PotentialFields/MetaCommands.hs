module Proxy.PotentialFields.MetaCommands where
import Proxy.PotentialFields.Internal.SimpleActions
import Control.Monad.Reader
import Data.Ord (comparing)
import Proxy.Types.Game
import Proxy.Server.Messages

data MetaCommand = STOP | MC UnitAction MetaCommand

stop :: UnitData -> MetaCommand
stop _ = STOP

followPath :: UnitData -> [Location] -> MetaCommand
followPath _ [] = STOP
followPath ud (x:xs) = case goTo ud x of
  STOP -> followPath ud xs
  MC c _ -> MC c (followPath ud (x:xs))

goTo :: UnitData -> Location -> MetaCommand
goTo ud l = if ud `isAt` l 
            then STOP
            else MC (toward ud l) (goTo ud l)
                 
patrol :: UnitData -> Location -> Location -> MetaCommand
patrol ud l1 l2 = followPath ud (concat (repeat [l1,l2])) 

isAt :: UnitData -> Location -> Bool
isAt ud l = (unitLocation ud) == l

