module Main where

import Proxy.PathFinding.HinzAStar
import Proxy.PathFinding.FloydWarshall
import Proxy.Types.Game
import System.Time
import GraphBuild
import Proxy.Math.Graph (arc)

main :: IO ()
main = do 
  putStrLn "Please enter first node"
  n <- readLn 
  putStrLn "Please enter second node"
  m <- readLn
  mapRaw <- readFile "../../MapData/Map-Crystallis"
  t1 <- getClockTime
  let mapD = read mapRaw :: [[Tile]]
      graph = buildGraph mapD
      width = length . head $ mapD
      path = aStarSearch graph n m (dist m)
      dist n m = euclD (nodeToPos width n) (nodeToPos width m)
  putStrLn (show path)
  t2 <- getClockTime
  putStrLn (show (diffClockTimes t2 t1))
