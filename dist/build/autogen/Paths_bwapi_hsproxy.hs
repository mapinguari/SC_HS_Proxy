module Paths_bwapi_hsproxy (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/users/ms12pb/.cabal/bin"
libdir     = "/users/ms12pb/.cabal/lib/bwapi-hsproxy-0.0/ghc-7.0.4"
datadir    = "/users/ms12pb/.cabal/share/bwapi-hsproxy-0.0"
libexecdir = "/users/ms12pb/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "bwapi_hsproxy_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "bwapi_hsproxy_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "bwapi_hsproxy_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "bwapi_hsproxy_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
