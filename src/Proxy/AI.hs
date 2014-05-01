module Main where
import Proxy.AIFunctions
import Proxy.Server.Server

main :: IO ()
main = Proxy.Server.Server.run onStart onFrame
