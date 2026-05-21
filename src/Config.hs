{-# LANGUAGE OverloadedStrings #-}
module Config (defaultConfig, loadConfigIO) where

import Types
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist, getXdgDirectory, XdgDirectory(XdgConfig), createDirectoryIfMissing)
import System.FilePath ((</>))

defaultConfig :: Config
defaultConfig = Config
    { cfgShowTitlebar = False
    , cfgIndentFirst  = 2
    , cfgLineHeight   = 1.4,       cfgBlockSpacing = 24
    , cfgBgColor      = "#f4ecd8", cfgFontColor    = "#2c3e50"
    , cfgHeaderColor  = "#8e8e8e", cfgHeaderSize   = 14
    , cfgFooterColor  = "#8e8e8e", cfgFooterSize   = 14
    , cfgMarginTop    = 30,      cfgMarginBottom = 30, cfgMarginLeft    = 80,  cfgMarginRight   = 80
    , cfgFontFamily   = "Serif", cfgFontSize     = 18, cfgTocLineHeight = 1.1, cfgTocSelectedBg = "#dcd3be"
    , cfgHeaderLeft   = "{book_title}", cfgHeaderCenter = "",                              cfgHeaderRight = "{chapter_name}"
    , cfgFooterLeft   = "",             cfgFooterCenter = "{current_page} / {total_page}", cfgFooterRight = "CHAPTER {chapter_index} / {total_chapter} TOTAL"
    }

defaultTomlStr :: T.Text
defaultTomlStr = T.unlines
    [ "# EPUB Reader Configuration File"
    , "[window]"
    , "show-titlebar = false"
    , ""
    , "[layout]"
    , "margin-top = 30"
    , "margin-bottom = 30"
    , "margin-left = 80"
    , "margin-right = 80"
    , "line-height = 1.4"
    , "block-spacing = 24"
    , "bg-color = \"#f4ecd8\""
    , "font-color = \"#2c3e50\""
    , "font-family = \"Serif\""
    , "default-font-size = 18"
    , "indent-first = 2"
    , ""
    , "[toc]"
    , "line-height = 1.2"
    , "selected-bg-color = \"#dcd3be\""
    , ""
    , "# Placeholder support: {book_title}, {chapter_name}, {chapter_index}, {total_chapter}, {current_page}, {total_page}"
    , "[header]"
    , "color = \"#8e8e8e\""
    , "font-size = 14"
    , "left = \"{book_title}\""
    , "center = \"\""
    , "right = \"{chapter_name}\""
    , ""
    , "[footer]"
    , "color = \"#8e8e8e\""
    , "font-size = 14"
    , "left = \"\""
    , "center = \"{current_page} / {total_page}\""
    , "right = \"CHAPTER {chapter_index} / {total_chapter} TOTAL\""
    ]

parseSimpleToml :: T.Text -> [(T.Text, T.Text)]
parseSimpleToml txt = go (T.lines txt) "" []
  where
    go [] _ acc = acc
    go (l:ls) currentSection acc
        | T.null (T.strip l) || T.isPrefixOf "#" (T.strip l) = go ls currentSection acc
        | T.isPrefixOf "[" (T.strip l) && T.isSuffixOf "]" (T.strip l) =
            let sec = T.drop 1 . T.dropEnd 1 $ T.strip l in go ls sec acc
        | "=" `T.isInfixOf` l =
            let (k, v) = T.breakOn "=" l
                key = T.strip k
                valRaw = T.strip (T.drop 1 v)
                val = if T.isPrefixOf "\"" valRaw && T.isSuffixOf "\"" valRaw
                      then T.drop 1 (T.dropEnd 1 valRaw) else valRaw
                fullKey = if T.null currentSection then key else currentSection <> "." <> key
            in go ls currentSection ((fullKey, val) : acc)
        | otherwise = go ls currentSection acc

applyConfig :: Config -> [(T.Text, T.Text)] -> Config
applyConfig = foldl updateConfig
  where
    updateConfig cfg (k, v) = case k of
        "window.show-titlebar"     -> cfg { cfgShowTitlebar  = readBool   v (cfgShowTitlebar cfg) }
        "layout.indent-first"      -> cfg { cfgIndentFirst   = readInt    v (cfgIndentFirst cfg)  }
        "layout.margin-top"        -> cfg { cfgMarginTop     = readInt    v (cfgMarginTop cfg)    }
        "layout.margin-bottom"     -> cfg { cfgMarginBottom  = readInt    v (cfgMarginBottom cfg) }
        "layout.margin-left"       -> cfg { cfgMarginLeft    = readInt    v (cfgMarginLeft cfg)   }
        "layout.margin-right"      -> cfg { cfgMarginRight   = readInt    v (cfgMarginRight cfg)  }
        "layout.line-height"       -> cfg { cfgLineHeight    = readDouble v (cfgLineHeight cfg)   }
        "layout.block-spacing"     -> cfg { cfgBlockSpacing  = readInt    v (cfgBlockSpacing cfg) }
        "layout.bg-color"          -> cfg { cfgBgColor       = v }
        "layout.font-color"        -> cfg { cfgFontColor     = v }
        "layout.font-family"       -> cfg { cfgFontFamily    = v }
        "layout.default-font-size" -> cfg { cfgFontSize      = readInt    v (cfgFontSize cfg)      }
        "toc.line-height"          -> cfg { cfgTocLineHeight = readDouble v (cfgTocLineHeight cfg) }
        "toc.selected-bg-color"    -> cfg { cfgTocSelectedBg = v }
        "header.left"              -> cfg { cfgHeaderLeft    = v }
        "header.center"            -> cfg { cfgHeaderCenter  = v }
        "header.right"             -> cfg { cfgHeaderRight   = v }
        "header.color"             -> cfg { cfgHeaderColor   = v }
        "header.font-size"         -> cfg { cfgHeaderSize    = readInt    v (cfgHeaderSize cfg) }
        "footer.left"              -> cfg { cfgFooterLeft    = v }
        "footer.center"            -> cfg { cfgFooterCenter  = v }
        "footer.right"             -> cfg { cfgFooterRight   = v }
        "footer.color"             -> cfg { cfgFooterColor   = v }
        "footer.font-size"         -> cfg { cfgFooterSize    = readInt    v (cfgFooterSize cfg) }
        _ -> cfg

    readInt txt fallback = case reads (T.unpack txt) of [(n, "")] -> n; _ -> fallback
    readBool txt fallback
        | T.toLower txt == "true"  = True
        | T.toLower txt == "false" = False
        | otherwise = fallback
    readDouble txt fallback = case reads (T.unpack txt) of [(n, "")] -> n; _ -> fallback

loadConfigIO :: IO Config
loadConfigIO = do
    configDir <- getXdgDirectory XdgConfig "epub-reader"
    createDirectoryIfMissing True configDir
    let path = configDir </> "config.toml"
    exists <- doesFileExist path
    if exists
        then do
            txt <- TIO.readFile path
            return $ applyConfig defaultConfig (parseSimpleToml txt)
        else do
            TIO.writeFile path defaultTomlStr
            return defaultConfig
