module Proxy.BwapiRegBat where

import Proxy.BwapiRegLoc
  
class BWAPIBat a where 
  bHeight :: a -> Int
  bWidth :: a -> Int 
  