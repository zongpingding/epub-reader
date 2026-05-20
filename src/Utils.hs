{-# LANGUAGE OverloadedStrings #-}
module Utils 
    ( unEscapeUrl
    , resolvePath
    , cleanTitle
    ) where

import Data.Char (chr, isHexDigit)
import Numeric (readHex)
import qualified Data.Text as T
import System.FilePath.Posix (splitDirectories, joinPath)

unEscapeUrl :: String -> String
unEscapeUrl [] = []
unEscapeUrl ('%':a:b:xs) | isHexDigit a && isHexDigit b, [(val, "")] <- readHex [a,b] = chr val : unEscapeUrl xs
unEscapeUrl (x:xs) = x : unEscapeUrl xs

resolvePath :: FilePath -> FilePath -> FilePath
resolvePath baseDir relPath = 
    let parts = splitDirectories baseDir ++ splitDirectories relPath
        cleanPath = foldl process [] parts
    in joinPath (reverse cleanPath)
  where
    process acc "." = acc
    process (_:acc) ".." = acc
    process acc ".." = acc
    process acc x = x : acc

cleanTitle :: T.Text -> T.Text
cleanTitle = T.unwords . T.words . T.replace "\n" " " . T.replace "\r" " "
