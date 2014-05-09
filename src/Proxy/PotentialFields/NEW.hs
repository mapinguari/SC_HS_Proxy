module NEW where 
import Data.Ix

h (v,w) (x,y) = filter (inbounds) [(x,y+1),(x-1,y),(x+1,y),(x,y-1)]
  where inbounds = inRange ((0,0),(v-1,w-1))

f' (_,w) n = (n `rem` w, n `div` w)
f (_,w) (x,y) = x + y * w

g b v = (map (f b) (h b (f' b v)))
g b = map (f b) . ((h b) . (f' b))