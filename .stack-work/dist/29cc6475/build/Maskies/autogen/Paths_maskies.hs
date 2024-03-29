{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_maskies (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,2,0,9] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\msys64\\home\\Admin\\dev\\zjr\\maskies\\.stack-work\\install\\57d3c045\\bin"
libdir     = "C:\\msys64\\home\\Admin\\dev\\zjr\\maskies\\.stack-work\\install\\57d3c045\\lib\\x86_64-windows-ghc-8.8.4\\maskies-0.2.0.9-2vuGRM5O0cb7zbakJAfpT0-Maskies"
dynlibdir  = "C:\\msys64\\home\\Admin\\dev\\zjr\\maskies\\.stack-work\\install\\57d3c045\\lib\\x86_64-windows-ghc-8.8.4"
datadir    = "C:\\msys64\\home\\Admin\\dev\\zjr\\maskies\\.stack-work\\install\\57d3c045\\share\\x86_64-windows-ghc-8.8.4\\maskies-0.2.0.9"
libexecdir = "C:\\msys64\\home\\Admin\\dev\\zjr\\maskies\\.stack-work\\install\\57d3c045\\libexec\\x86_64-windows-ghc-8.8.4\\maskies-0.2.0.9"
sysconfdir = "C:\\msys64\\home\\Admin\\dev\\zjr\\maskies\\.stack-work\\install\\57d3c045\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "maskies_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "maskies_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "maskies_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "maskies_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "maskies_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "maskies_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
