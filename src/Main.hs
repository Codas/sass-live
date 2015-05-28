{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

-- import System.FSNotify.Devel
import System.FSNotify (withManager, Event(..), eventPath, watchDir)
import Turtle
import Control.Monad.Managed (with)
import Control.Concurrent (threadDelay)
import Text.Sass
import Data.Text (pack)
import System.IO (hPutStr)
import System.Environment (getArgs)
import Filesystem.Path hiding (empty, toText)
import Filesystem.Path.CurrentOS hiding (empty, toText)
import Prelude hiding (FilePath)

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
compileSassFile fp outPath =
  do echo ("compiling file" <> fpToText fp)
     compileFile (encodeString fp) def >>= \case
         Left err -> print err
         Right css -> do echo $ "writing css file " <> cssFile
                         with (mktemp scssFolder "compiled_css") $ \(fp, fh) ->
                           do hPutStr fh css
                              void $ proc "postcss" ["--use", "autoprefixer", fpToText fp, "-d", cssFile] empty
  where scssFolder = directory fp
        cssName = basename fp <.> "css"
        cssFile = fpToText $ outPath </> cssName

fpToText :: FilePath -> Text
fpToText = pack . encodeString
