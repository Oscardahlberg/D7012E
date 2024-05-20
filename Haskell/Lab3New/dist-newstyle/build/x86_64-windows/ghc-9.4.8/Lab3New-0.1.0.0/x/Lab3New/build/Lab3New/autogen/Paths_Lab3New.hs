{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_Lab3New (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "E:\\haskell\\cabal\\bin"
libdir     = "E:\\haskell\\cabal\\x86_64-windows-ghc-9.4.8\\Lab3New-0.1.0.0-inplace-Lab3New"
dynlibdir  = "E:\\haskell\\cabal\\x86_64-windows-ghc-9.4.8"
datadir    = "E:\\haskell\\cabal\\x86_64-windows-ghc-9.4.8\\Lab3New-0.1.0.0"
libexecdir = "E:\\haskell\\cabal\\Lab3New-0.1.0.0-inplace-Lab3New\\x86_64-windows-ghc-9.4.8\\Lab3New-0.1.0.0"
sysconfdir = "E:\\haskell\\cabal\\etc"

getBinDir     = catchIO (getEnv "Lab3New_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "Lab3New_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "Lab3New_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "Lab3New_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Lab3New_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Lab3New_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'