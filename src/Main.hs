{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

-- import System.FSNotify.Devel
import System.FSNotify (withManager, Event(..), eventPath, watchTree)
import Turtle
import Control.Monad.Managed (with)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM (atomically)
import Text.Sass
import Data.Text (pack, unpack)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import System.IO (hPutStr, hClose)
import System.Environment (getArgs)
import Filesystem.Path hiding (empty, toText, hasExtension)
import Filesystem.Path.CurrentOS hiding (empty, toText)
import Prelude hiding (FilePath)
import qualified System.Process as P
import qualified Control.Foldl as Fold

main :: IO ()
main = getArgs >>= \case
           [scss, css] -> watcher (decodeString scss) (decodeString css)
           _           -> error "Watch dir or static dir not specified."

watcher :: FilePath -> FilePath -> IO ()
watcher scssFolder cssFolder = void $ withManager $ \mgr -> do
    echo $ "watching " <> fpToText scssFolder <> " folder"
    currentDir <- pwd
    includeCache <- newTVarIO Map.empty
    let outPath = currentDir </> cssFolder
        watch e = do cache <- readTVarIO includeCache
                     let fp = eventPath e
                         scssFiles = fromMaybe (Set.singleton fp) (Map.lookup fp cache)
                     when (hasExtension fp "scss") $
                          mapM_ (\file -> compileSassFile file outPath includeCache) scssFiles
    -- compile all root files and fill the include cache
    files <- fmap (filter (`hasExtension` "scss")) (fold (ls scssFolder) Fold.list)
    mapM_ (\f -> compileSassFile f outPath includeCache) files
    watchTree mgr scssFolder ((`hasExtension` "scss") . eventPath) watch
    forever $ threadDelay 1000000


compileSassFile :: FilePath -> FilePath -> TVar (Map FilePath (Set FilePath)) -> IO ()
compileSassFile scssFilePath outPath includeCacheTVar =
  do echo ("compiling file" <> fpToText (filename scssFilePath))
     compileFile (encodeString scssFilePath) def >>= \case
         Left err -> print err
         Right res -> do let args = ["--use", "autoprefixer", "-o", unpack cssFile]
                             cProc =  P.proc "postcss" args
                         (rh, wh) <- P.createPipe
                         let css = resultString res
                         includes <- resultIncludes res
                         atomically $ let fn fp = Map.insertWith Set.union fp (Set.singleton scssFilePath)
                                          updateFn oldMap = foldr (fn . decodeString) oldMap includes
                                      in modifyTVar' includeCacheTVar updateFn
                         P.readCreateProcess cProc css
                         echo "Prefixing completed"
                         echo ("compiled file " <> fpToText (filename scssFilePath) <> " to " <> cssFile)
  where scssFolder = directory scssFilePath
        cssName = basename scssFilePath <.> "css"
        cssFile = fpToText $ outPath </> cssName

fpToText :: FilePath -> Text
fpToText = pack . encodeString
