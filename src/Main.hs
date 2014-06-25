module Main where
import AIFunctions
import Proxy.Server.Server

main :: IO ()
main = Proxy.Server.Server.run onStart onFrame
