module Paths_communis_api (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/hefesto/dev/communis/communis-api/.stack-work/install/x86_64-linux/lts-3.10/7.10.2/bin"
libdir     = "/home/hefesto/dev/communis/communis-api/.stack-work/install/x86_64-linux/lts-3.10/7.10.2/lib/x86_64-linux-ghc-7.10.2/communis-api-0.1.0.0-4YWN4fiQlI0FPrIr2ZUvyD"
datadir    = "/home/hefesto/dev/communis/communis-api/.stack-work/install/x86_64-linux/lts-3.10/7.10.2/share/x86_64-linux-ghc-7.10.2/communis-api-0.1.0.0"
libexecdir = "/home/hefesto/dev/communis/communis-api/.stack-work/install/x86_64-linux/lts-3.10/7.10.2/libexec"
sysconfdir = "/home/hefesto/dev/communis/communis-api/.stack-work/install/x86_64-linux/lts-3.10/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "communis_api_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "communis_api_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "communis_api_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "communis_api_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "communis_api_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
