module Proxy.Server.MapExtractor where
import Proxy.Types.Game
import Proxy.Server.Messages
import System.Directory
import Data.Word

extractMapTo :: FilePath -> GameInfo -> IO ()
extractMapTo fp gI = do
  createDirectoryIfMissing True fp
  let fp' = fp ++ "Map-" ++ mapN gI
  existAlready <- doesFileExist fp'
  if existAlready 
    then return ()
    else writeFile fp' (show $ mapD gI)
  where mapN = mapName . gameMap
        mapD = tiles . gameMap

isMapData :: FilePath -> Bool
isMapData fp = take 4 fp == "Map-"
        
allGrids :: FilePath -> IO [[[Tile]]]
allGrids fp = do
  ls <- getDirectoryContents fp 
  let mapDataFiles = filter isMapData ls
  mapM readFile mapDataFiles >>= return . map (read :: String -> [[Tile]])
 
tileToWord8 :: Tile -> Word8
tileToWord8 (Tile h w b) = toEnum $ fromEnum h + 2 * fromEnum w + 4 * fromEnum b

word8ToTile :: Word8 -> Tile
word8ToTile w
  | w == 0 = Tile 0 False False
  | w == 1 = Tile 1 False False
  | w == 2 = Tile 0 True False
  | w == 3 = Tile 1 True False
  | w == 4 = Tile 0 False True
  | w == 5 = Tile 1 False True
  | w == 6 = Tile 0 True True
  | w == 7 = Tile 1 True True