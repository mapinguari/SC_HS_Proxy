module Paths_bwapi_hsproxy (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/mapinguari/.cabal/bin"
libdir     = "/home/mapinguari/.cabal/lib/bwapi-hsproxy-0.0/ghc-7.4.2"
datadir    = "/home/mapinguari/.cabal/share/bwapi-hsproxy-0.0"
libexecdir = "/home/mapinguari/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "bwapi_hsproxy_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "bwapi_hsproxy_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "bwapi_hsproxy_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "bwapi_hsproxy_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
