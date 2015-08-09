module Paths_335g_github_io (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/Yoshiki/dev/Web/335g.github.io/.stack-work/install/x86_64-osx/lts-2.21/7.8.4/bin"
libdir     = "/Users/Yoshiki/dev/Web/335g.github.io/.stack-work/install/x86_64-osx/lts-2.21/7.8.4/lib/x86_64-osx-ghc-7.8.4/335g-github-io-0.1.0.0"
datadir    = "/Users/Yoshiki/dev/Web/335g.github.io/.stack-work/install/x86_64-osx/lts-2.21/7.8.4/share/x86_64-osx-ghc-7.8.4/335g-github-io-0.1.0.0"
libexecdir = "/Users/Yoshiki/.cabal/libexec"
sysconfdir = "/Users/Yoshiki/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "335g_github_io_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "335g_github_io_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "335g_github_io_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "335g_github_io_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "335g_github_io_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
