module GraphBuild where
import Proxy.Types.Game
import Data.Array
import Proxy.Math.Graph


eightWay :: (Num a,Eq a,Ix a) => (a,a) -> [(a,a)]
eightWay (x,y) = filter (/= (x,y)) (range ((x-1,y-1),(x+1,y+1)))

euclD :: (Int,Int) -> (Int,Int) -> Float
euclD (x,y) (z,w) = sqrt . fromIntegral $ (x - z) ^ 2 + (y - w) ^ 2

posToNode :: Int -> (Int,Int) -> Node
posToNode w (x,y) = w*y + x

nodeToPos :: Int -> Int -> (Int,Int)
nodeToPos w n = (n `rem` w , n `div` w)

buildGraph :: [[Tile]] -> WLAGraph a Float
buildGraph xss = WLA $ array graphBounds [(posToNode w i, adjs i) | i <- indices grid]
  where w = length.head $ xss
        h = length xss
        adjs x = map (\y -> (euclD x y,posToNode w y)) . filter boundsLookup . eightWay $ x
        graphBounds = (0,posToNode w (w-1,h-1))
        boundsLookup x = inRange (bounds grid) x && grid ! x
        grid = listArray ((0,0),(w-1,h-1)) (map walkable (concat xss))