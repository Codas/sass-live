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
import Filesystem.Path hiding (empty)
import Filesystem.Path.CurrentOS hiding (empty)
import Prelude hiding (FilePath)

main :: IO ()
main = getArgs >>= \case
           [scss, css] -> watcher (decodeString scss) (decodeString css)
           _           -> error "Watch dir or static dir not specified."

watcher :: FilePath -> FilePath -> IO ()
watcher scssFolder cssFolder = void $ withManager $ \mgr -> do
    echo $ "watching " <>  (toText scssFolder) <> " folder"
    currentDir <- pwd
    watchDir mgr scssFolder ((`hasExtension` "scss") . eventPath) $ \e -> do
      let fp = eventPath e
          dir = directory fp
          name = basename fp <.> "css"
          cssFile = toText $ currentDir </> cssFolder </> name
      echo $ "compiling file " <> toText fp
      compileFile (encodeString fp) def >>= \case
          Left err -> print err
          Right css -> do echo $ "writing css file " <> cssFile
                          with (mktemp scssFolder "compiled_css") $ \(fp, fh) ->
                            do hPutStr fh css
                               void $ proc "autoprefixer" ["-o", cssFile, toText fp] empty
    forever $ threadDelay 1000000
  where toText = pack . encodeString
