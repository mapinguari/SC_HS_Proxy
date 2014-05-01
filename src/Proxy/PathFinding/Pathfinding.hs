module Proxy.Pathfinding where
import Proxy.AStar 
import Proxy.PreProcessing.PotentialField
type CommandSeq a = [a -> Maybe Command]
type Prepared = Bool 

-- track closest reached.


-- static potentials
-- dynamic potentials
-- objective potential
-- historial potential

potentialFieldPathfinding :: 

nodeInternalPathfinding :: LEdge LineSeg -> UnitData -> 
nodeInternalPathfinding le uD = e

continueInternal :: Frame -> Frame -> Int -> Int -> Bool
continueInternal estimate currentF best currentD 
  | currentF >= estimate =  