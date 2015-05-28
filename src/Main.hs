{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

-- import System.FSNotify.Devel
import System.FSNotify (withManager, Event(..), eventPath, watchDir)
import Turtle
import Control.Monad.Managed (with)
import Control.Concurrent (threadDelay)
import Text.Sass
import Data.Text (pack, unpack)
import System.IO (hPutStr, hClose)
import System.Environment (getArgs)
import Filesystem.Path hiding (empty, toText)
import Filesystem.Path.CurrentOS hiding (empty, toText)
import Prelude hiding (FilePath)
import qualified System.Process as P

main :: IO ()
main = getArgs >>= \case
           [scss, css] -> watcher (decodeString scss) (decodeString css)
           _           -> error "Watch dir or static dir not specified."

watcher :: FilePath -> FilePath -> IO ()
watcher scssFolder cssFolder = void $ withManager $ \mgr -> do
    echo $ "watching " <> fpToText scssFolder <> " folder"
    currentDir <- pwd
    let outPath = currentDir </> cssFolder
    watchDir mgr scssFolder ((`hasExtension` "scss") . eventPath) $ \e ->
      compileSassFile (eventPath e) outPath
    forever $ threadDelay 1000000


compileSassFile :: FilePath -> FilePath -> IO ()
compileSassFile scssFilePath outPath =
  do echo ("compiling file" <> fpToText (filename scssFilePath))
     compileFile (encodeString scssFilePath) def >>= \case
         Left err -> print err
         Right css -> do let args = ["--use", "autoprefixer", "-o", unpack cssFile]
                             cProc =  P.proc "postcss" args
                         (rh, wh) <- P.createPipe
                         P.readCreateProcess cProc css
                         echo "Prefixing completed"
                         echo ("compiled file " <> fpToText (filename scssFilePath) <> " to " <> cssFile)
  where scssFolder = directory scssFilePath
        cssName = basename scssFilePath <.> "css"
        cssFile = fpToText $ outPath </> cssName

fpToText :: FilePath -> Text
fpToText = pack . encodeString
