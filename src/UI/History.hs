{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.History 
    ( readHistory
    , writeHistory 
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Exception (catch, SomeException)
import Control.Monad (filterM)
import System.FilePath.Posix ((</>))
import System.Directory (doesFileExist, getXdgDirectory, XdgDirectory(XdgConfig), createDirectoryIfMissing)
import UI.Utils (ordNub)

readHistory :: IO [T.Text]
readHistory = do
    configDir <- getXdgDirectory XdgConfig "epub-reader"
    let historyPath = configDir </> "history.txt"
    histRaw <- catch (TIO.readFile historyPath) (\(_::SomeException) -> return "")
    let files = T.lines histRaw
    filterM (doesFileExist . T.unpack) files

writeHistory :: FilePath -> IO ()
writeHistory newPath = do
    configDir <- getXdgDirectory XdgConfig "epub-reader"
    createDirectoryIfMissing True configDir
    let historyPath = configDir </> "history.txt"
    histRaw <- catch (TIO.readFile historyPath) (\(_::SomeException) -> return "")
    let newHist = take 10 $ ordNub (T.pack newPath : T.lines histRaw)
    TIO.writeFile historyPath (T.unlines newHist)
