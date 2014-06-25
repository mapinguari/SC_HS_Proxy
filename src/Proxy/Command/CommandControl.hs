module Proxy.CommandControl where
import Proxy.Types.Game
import Proxy.Server.Messages
import Control.Monad.Trans.State

data Conditional b a = Cond {test :: b -> Bool , action :: a}

newtype CommandSequence b a = CS [Conditional b a]

next :: CommandSequence b a -> Maybe (Conditional b a)
next (CS []) = Nothing
next (CS (x:xs)) = Just x

ignore :: CommandSequence b a -> CommandSequence b a
ignore (CS (x:xs)) = CS xs
ignore (CS []) = CS []

perform :: Conditional b a -> b -> Maybe a
perform c b = if test c b 
              then Just (action c)
              else Nothing
                   
step :: CommandSequence b a -> b -> (Maybe a, CommandSequence b a)
step cs b = case next cs >>= flip perform b of
  Nothing -> (Nothing, cs)
  Just a -> (Just a, ignore cs)
  
step' b = state (flip step b)
      
unitIsAt :: UnitId -> Location -> GameState -> Bool
unitIsAt _ _ _ = True

performPath :: UnitId -> [Location] -> CommandSequence GameState Command 
performPath uid xs = CS $ map f (zip xs (tail xs))
  where f (x,y) = Cond {test = unitIsAt uid x,
                        action = rightClickAt uid y}
                  

pickActions :: [(UnitId, Priority a)] -> [(UnitId, Priority a)]
pickActions = map (minimumBy urgency) . f 

urgency :: Priority a -> Priority a -> Ordering
urgency p1 p2 = compare (prio p1) (prio p2)

f :: Eq a => [(a,b)] -> [[(a,b)]]
f [] = [] 
f (x:xs) = as : f bs
 where (as,bs) = partition ((== (fst x)).fst) (x:xs)
