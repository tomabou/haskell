{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_stackTest (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/tomabou/Documents/haskell/smallGame/stackTest/.stack-work/install/x86_64-linux/lts-8.11/8.0.2/bin"
libdir     = "/home/tomabou/Documents/haskell/smallGame/stackTest/.stack-work/install/x86_64-linux/lts-8.11/8.0.2/lib/x86_64-linux-ghc-8.0.2/stackTest-0.1.0.0-GYeYAx4ApZVF3rsQY1OaRV"
dynlibdir  = "/home/tomabou/Documents/haskell/smallGame/stackTest/.stack-work/install/x86_64-linux/lts-8.11/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/tomabou/Documents/haskell/smallGame/stackTest/.stack-work/install/x86_64-linux/lts-8.11/8.0.2/share/x86_64-linux-ghc-8.0.2/stackTest-0.1.0.0"
libexecdir = "/home/tomabou/Documents/haskell/smallGame/stackTest/.stack-work/install/x86_64-linux/lts-8.11/8.0.2/libexec"
sysconfdir = "/home/tomabou/Documents/haskell/smallGame/stackTest/.stack-work/install/x86_64-linux/lts-8.11/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "stackTest_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "stackTest_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "stackTest_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "stackTest_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "stackTest_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "stackTest_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
