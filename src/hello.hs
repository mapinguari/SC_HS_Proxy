module Main where
import Graphics.Rendering.Cairo
import Proxy.PreProcessing.RowBased
import Proxy.Types.Game

myDraw :: Render ()
myDraw = do
  setSourceRGB 0 0 0 
  arc 200 200 100 0 (2*pi)
  stroke

main :: IO ()
main = do
  mapRaw <- readFile "../../MapData/Map-Midnight Lagoon"
  let mapD = map (map walkable) $ mapA
      mapA = read mapRaw :: [[Tile]]
      pdw = 5000
      pdh = 5000
  withPDFSurface "myDraw.pdf" pdw pdh (\s -> renderWith s $ do mapToSquare mapD 0 0
                                                               showdecomposition (map helper (runLengthEncoding mapA))
                                                               showPage)
    
battlefieldTest = [[f i | i <- [j..(50+j)]] | j <- [1..50]]
                   where f i = True
    
showdecomposition :: [((Double,Double),(Double,Double))] -> Render ()
showdecomposition [] = return ()
showdecomposition (((x,y),(z,w)):xs) = do
  rectangle ((y-1)*10) ((x-1)*10) ((w - y)*10) ((z - x)*10)
  setLineWidth 1
  setSourceRGBA 0 0.1 1 0.5
  strokePreserve
  setSourceRGBA 0 0 1 0.4
  fill
  showdecomposition xs


                         
square :: Bool -> Double -> Double -> Double -> Render ()
square b x y h = do
  if b then setSourceRGB 1 1 1 else setSourceRGB 0 0 0
  setLineWidth 5
  moveTo x y
  lineTo (x+h) y
  lineTo (x+h) (y+h) 
  lineTo x (y+h)
  closePath
  fill
  stroke
  

  
mapToSquare :: [[Bool]] -> Double -> Double -> Render ()
mapToSquare [] _ _ = return ()
mapToSquare (xs:xss) x y = do 
  listToSquare xs x y 
  mapToSquare xss x (y+10)
  
  
listToSquare :: [Bool] -> Double -> Double -> Render ()
listToSquare [] _ _ = return ()
listToSquare (b:xs) x y = do
  let h = 10
  square b x y h
  listToSquare xs (x+h) y
  
lineFromTo :: Double -> Double -> Double -> Double -> Render ()
lineFromTo x y z w = do
  setSourceRGB 0 0 0 
  setLineWidth 0.25
  moveTo x y 
  lineTo z w 
  stroke
{-
horLines :: Double -> Double 
horLines x y = do
  let length = (0.25+10)*x + 0.25
      noOfLines = y + 1
      horLine z w = do 
        if z == w 
          then return ()
          else lineFromTo 1 z (1+length) z
               horLine 
  
  
grid :: Double -> Double -> Double -> Double -> Render ()
grid n m = do
  horLines 
-}