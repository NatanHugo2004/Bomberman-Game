{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_Bomberman_Game (
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
bindir     = "/mnt/c/Users/may_a/Documents/my_projects/Bomberman-Game/.stack-work/install/x86_64-linux/5afa23b490347518766af50b4fed67e763096396fd56c8d4fb25fb7003b80e48/9.10.2/bin"
libdir     = "/mnt/c/Users/may_a/Documents/my_projects/Bomberman-Game/.stack-work/install/x86_64-linux/5afa23b490347518766af50b4fed67e763096396fd56c8d4fb25fb7003b80e48/9.10.2/lib/x86_64-linux-ghc-9.10.2-aac9/Bomberman-Game-0.1.0.0-LknmGf9wH2d2Aj4gf2WI9c-Bomberman-Game-exe"
dynlibdir  = "/mnt/c/Users/may_a/Documents/my_projects/Bomberman-Game/.stack-work/install/x86_64-linux/5afa23b490347518766af50b4fed67e763096396fd56c8d4fb25fb7003b80e48/9.10.2/lib/x86_64-linux-ghc-9.10.2-aac9"
datadir    = "/mnt/c/Users/may_a/Documents/my_projects/Bomberman-Game/.stack-work/install/x86_64-linux/5afa23b490347518766af50b4fed67e763096396fd56c8d4fb25fb7003b80e48/9.10.2/share/x86_64-linux-ghc-9.10.2-aac9/Bomberman-Game-0.1.0.0"
libexecdir = "/mnt/c/Users/may_a/Documents/my_projects/Bomberman-Game/.stack-work/install/x86_64-linux/5afa23b490347518766af50b4fed67e763096396fd56c8d4fb25fb7003b80e48/9.10.2/libexec/x86_64-linux-ghc-9.10.2-aac9/Bomberman-Game-0.1.0.0"
sysconfdir = "/mnt/c/Users/may_a/Documents/my_projects/Bomberman-Game/.stack-work/install/x86_64-linux/5afa23b490347518766af50b4fed67e763096396fd56c8d4fb25fb7003b80e48/9.10.2/etc"

getBinDir     = catchIO (getEnv "Bomberman_Game_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "Bomberman_Game_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "Bomberman_Game_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "Bomberman_Game_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Bomberman_Game_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Bomberman_Game_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
