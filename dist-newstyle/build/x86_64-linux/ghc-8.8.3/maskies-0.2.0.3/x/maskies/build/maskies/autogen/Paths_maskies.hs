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
version = Version [0,2,0,3] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/dave/.cabal/bin"
libdir     = "/home/dave/.cabal/lib/x86_64-linux-ghc-8.8.3/maskies-0.2.0.3-inplace-maskies"
dynlibdir  = "/home/dave/.cabal/lib/x86_64-linux-ghc-8.8.3"
datadir    = "/home/dave/.cabal/share/x86_64-linux-ghc-8.8.3/maskies-0.2.0.3"
libexecdir = "/home/dave/.cabal/libexec/x86_64-linux-ghc-8.8.3/maskies-0.2.0.3"
sysconfdir = "/home/dave/.cabal/etc"

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
  return (dir ++ "/" ++ name)
