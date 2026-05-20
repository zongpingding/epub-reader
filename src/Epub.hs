{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Epub 
    ( extractSpine
    , getBookTitle
    , extractToc
    , getChapterTitle
    , getChapterBlocks
    , getZipEntryBytes
    ) where

import Types
import Utils
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Archive.Zip as Zip
import Text.HTML.TagSoup (parseTags, Tag(..), fromAttrib)
import System.FilePath.Posix (takeDirectory, takeFileName)
import Control.Applicative ((<|>))
import Data.List (elemIndex)

getZipEntryContent :: Zip.Archive -> FilePath -> T.Text
getZipEntryContent archive path =
    case filter (\e -> Zip.eRelativePath e == path) (Zip.zEntries archive) of
        (e:_) -> TE.decodeUtf8 (BL.toStrict (Zip.fromEntry e))
        []    -> ""

getZipEntryBytes :: Zip.Archive -> FilePath -> Maybe BS.ByteString
getZipEntryBytes archive path =
    case filter (\e -> Zip.eRelativePath e == path) (Zip.zEntries archive) of
        (e:_) -> Just (BL.toStrict (Zip.fromEntry e))
        []    -> Nothing

getOpfPath :: Zip.Archive -> FilePath
getOpfPath archive =
    let containerTags = parseTags (getZipEntryContent archive "META-INF/container.xml")
    in case [fromAttrib "full-path" t | t@(TagOpen "rootfile" _) <- containerTags] of
        (p:_) -> T.unpack p
        _     -> "OEBPS/content.opf"

extractSpine :: Zip.Archive -> [FilePath]
extractSpine archive =
    let opfPath = getOpfPath archive
        opfDir = takeDirectory opfPath
        opfTags = parseTags (getZipEntryContent archive opfPath)
        idMap = [ (fromAttrib "id" t, fromAttrib "href" t) | t@(TagOpen "item" _) <- opfTags ]
        spineRefs = [ fromAttrib "idref" t | t@(TagOpen "itemref" _) <- opfTags ]
        resolve href = if opfDir == "." then T.unpack href else opfDir `resolvePath` T.unpack href
    in [ unEscapeUrl (resolve href) | ref <- spineRefs, Just href <- [lookup ref idMap] ]

getBookTitle :: Zip.Archive -> T.Text
getBookTitle archive =
    let opfTags = parseTags (getZipEntryContent archive (getOpfPath archive))
        findTitle [] = "Unkown Book"
        findTitle (TagOpen t _ : rest) | t == "dc:title" || t == "title" =
            let (inner, _) = break (\case TagClose c -> c == t; _ -> False) rest
            in T.strip (T.concat [text | TagText text <- inner])
        findTitle (_:rest) = findTitle rest
    in case findTitle opfTags of "" -> "Unkown Book"; t -> cleanTitle t

extractToc :: Zip.Archive -> [FilePath] -> [TocEntry]
extractToc archive spine =
    let opfPath = getOpfPath archive
        opfTags = parseTags (getZipEntryContent archive opfPath)
        idMap = [ (fromAttrib "id" t, fromAttrib "href" t) | t@(TagOpen "item" _) <- opfTags ]
        spineTocId = case [fromAttrib "toc" t | t@(TagOpen "spine" _) <- opfTags] of
                        (x:_) | not (T.null x) -> Just x
                        _ -> Nothing
        mNcxHref = do
            tid <- spineTocId <|> Just "ncx"
            lookup tid idMap
            
        parseNcx content ncxDir =
            let tags = parseTags content
                go [] _ _ acc = reverse acc
                go (TagOpen "navPoint" _ : ts) level currentTitle acc = go ts (level + 1) currentTitle acc
                go (TagClose "navPoint" : ts) level currentTitle acc = go ts (max 1 (level - 1)) currentTitle acc
                go (TagOpen "text" _ : ts) level _ acc =
                    let (inner, rest) = break (\case TagClose "text" -> True; _ -> False) ts
                        txt = cleanTitle (T.concat [t | TagText t <- inner])
                    in go rest level txt acc
                go (TagOpen "content" attrs : ts) level currentTitle acc =
                    let src = case lookup "src" attrs of Just s -> s; Nothing -> ""
                        cleanSrc = T.unpack $ fst $ T.breakOn "#" src
                        fullPath = unEscapeUrl $ resolvePath ncxDir cleanSrc
                        idx = case elemIndex fullPath spine of
                                Just i -> i
                                Nothing -> case filter (\p -> takeFileName fullPath == takeFileName p) spine of
                                             (p:_) -> case elemIndex p spine of Just i -> i; Nothing -> -1
                                             [] -> -1
                    in if idx >= 0 && not (T.null currentTitle)
                       then go ts level "" (TocEntry level currentTitle idx : acc)
                       else go ts level "" acc
                go (_:ts) level currentTitle acc = go ts level currentTitle acc
            in go tags 0 "" []

    in case mNcxHref of
        Just href -> 
            let ncxPath = resolvePath (takeDirectory opfPath) (T.unpack href)
                entries = parseNcx (getZipEntryContent archive ncxPath) (takeDirectory ncxPath)
            in if null entries then fallbackToc archive spine else entries
        Nothing -> fallbackToc archive spine

fallbackToc :: Zip.Archive -> [FilePath] -> [TocEntry]
fallbackToc archive spine = zipWith (\idx path -> TocEntry 1 (getChapterTitle archive path) idx) [0..] spine

getChapterTitle :: Zip.Archive -> FilePath -> T.Text
getChapterTitle archive path =
    let tags = parseTags (getZipEntryContent archive path)
        bodyTags = dropWhile (\case TagOpen "body" _ -> False; _ -> True) tags
        findH [] = Nothing
        findH (TagOpen t _ : rest) | t `elem` ["h1", "h2", "h3"] =
            let (inner, _) = break (\case TagClose c -> c == t; _ -> False) rest
                txt = T.strip (T.concat [text | TagText text <- inner])
            in if T.null txt then findH rest else Just txt
        findH (_:rest) = findH rest
        firstLine = case filter (\t -> T.length t >= 2) (map T.strip [text | TagText text <- bodyTags]) of
                        (x:_) -> x
                        _ -> T.pack (takeFileName path) 
    in cleanTitle $ case findH bodyTags of Just t -> t; Nothing -> firstLine

removeHiddenTags :: [T.Text] -> [Tag T.Text] -> [Tag T.Text]
removeHiddenTags _ [] = []
removeHiddenTags hideTags (TagOpen n _ : ts) | n `elem` hideTags =
    let (_, rest) = break (\case TagClose c -> c == n; _ -> False) ts
    in removeHiddenTags hideTags (drop 1 rest) 
removeHiddenTags hideTags (t:ts) = t : removeHiddenTags hideTags ts

escapePango :: T.Text -> T.Text
escapePango t = T.replace "&" "&amp;" $ T.replace "<" "&lt;" $ T.replace ">" "&gt;" t

htmlToPango :: [Tag T.Text] -> T.Text
htmlToPango [] = ""
htmlToPango (tag:tags) = case tag of
    TagText t -> escapePango t <> htmlToPango tags
    TagOpen "b"  _ -> "<b>" <> htmlToPango tags
    TagOpen "strong" _ -> "<b>" <> htmlToPango tags
    TagOpen "i"  _ -> "<i>" <> htmlToPango tags
    TagOpen "em" _ -> "<i>" <> htmlToPango tags
    TagClose "b"  -> "</b>" <> htmlToPango tags
    TagClose "strong" -> "</b>" <> htmlToPango tags
    TagClose "i"  -> "</i>" <> htmlToPango tags
    TagClose "em" -> "</i>" <> htmlToPango tags
    TagOpen "br" _ -> "\n" <> htmlToPango tags
    TagOpen "br/" _ -> "\n" <> htmlToPango tags
    _ -> htmlToPango tags

toBlocks :: [Tag T.Text] -> [ContentBlock]
toBlocks tags = filter nonEmpty (buildBlocks tags)
  where
    nonEmpty (TextBlock t) = not (T.null (T.strip t))
    nonEmpty (ImgBlock _) = True
    
    buildBlocks [] = []
    buildBlocks (TagOpen t attrs : ts) | t `elem` ["img", "image"] =
        let src = lookup "src" attrs <|> lookup "xlink:href" attrs <|> lookup "href" attrs
            imgB = case src of Just s -> [ImgBlock (T.unpack s)]; Nothing -> []
        in imgB ++ buildBlocks ts
        
    buildBlocks (TagOpen t _ : ts) | t `elem` ["h1", "h2", "h3", "h4"] =
        let (inner, after) = break (\case TagClose c -> c == t; _ -> False) ts
            content = cleanTitle (htmlToPango inner)  
            markup = case t of
                "h1" -> "<span size='xx-large' weight='bold'>" <> content <> "</span>"
                "h2" -> "<span size='x-large' weight='bold'>" <> content <> "</span>"
                "h3" -> "<span size='large' weight='bold'>" <> content <> "</span>"
                _    -> "<span size='large' weight='bold'>" <> content <> "</span>"
            next = drop 1 after
        in TextBlock markup : buildBlocks next
        
    buildBlocks tags' =
        let isBoundary (TagOpen t _) = t `elem` ["img", "image", "h1", "h2", "h3", "h4", "p", "div"]
            isBoundary (TagClose t)  = t `elem` ["p", "div"]
            isBoundary _ = False
            
            (inline, rest) = break isBoundary tags'
            content = T.strip (htmlToPango inline)
            
            droppedRest = case rest of
                              (TagOpen t _:ts) | t `elem` ["p", "div"] -> ts
                              (TagClose t :ts) | t `elem` ["p", "div"] -> ts
                              _ -> rest
        in TextBlock content : buildBlocks droppedRest

-- | Expose chapter content Placeholder
getChapterBlocks :: Zip.Archive -> FilePath -> [ContentBlock]
getChapterBlocks archive path =
    let rawHtml = getZipEntryContent archive path
        tags = removeHiddenTags ["style", "script", "head", "title"] (parseTags rawHtml)
    in toBlocks tags
