{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Ex02 (
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
version = Version [1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/import/glass/5/z5282955/.cabal/bin"
libdir     = "/import/glass/5/z5282955/.cabal/lib/x86_64-linux-ghc-8.0.1/Ex02-1.0"
dynlibdir  = "/import/glass/5/z5282955/.cabal/lib/x86_64-linux-ghc-8.0.1"
datadir    = "/import/glass/5/z5282955/.cabal/share/x86_64-linux-ghc-8.0.1/Ex02-1.0"
libexecdir = "/import/glass/5/z5282955/.cabal/libexec"
sysconfdir = "/import/glass/5/z5282955/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Ex02_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Ex02_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Ex02_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Ex02_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Ex02_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Ex02_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
