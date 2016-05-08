module Paths_AlgorithmsAndMe (
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

bindir     = "/Users/sampathsingamsetty/software/fp/haskell/sampath/AlgorithmsAndMe/.stack-work/install/x86_64-osx/lts-5.15/7.10.3/bin"
libdir     = "/Users/sampathsingamsetty/software/fp/haskell/sampath/AlgorithmsAndMe/.stack-work/install/x86_64-osx/lts-5.15/7.10.3/lib/x86_64-osx-ghc-7.10.3/AlgorithmsAndMe-0.1.0.0-31J7Jnmpw3y1lbjgkMYrWO"
datadir    = "/Users/sampathsingamsetty/software/fp/haskell/sampath/AlgorithmsAndMe/.stack-work/install/x86_64-osx/lts-5.15/7.10.3/share/x86_64-osx-ghc-7.10.3/AlgorithmsAndMe-0.1.0.0"
libexecdir = "/Users/sampathsingamsetty/software/fp/haskell/sampath/AlgorithmsAndMe/.stack-work/install/x86_64-osx/lts-5.15/7.10.3/libexec"
sysconfdir = "/Users/sampathsingamsetty/software/fp/haskell/sampath/AlgorithmsAndMe/.stack-work/install/x86_64-osx/lts-5.15/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "AlgorithmsAndMe_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "AlgorithmsAndMe_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "AlgorithmsAndMe_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "AlgorithmsAndMe_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "AlgorithmsAndMe_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
