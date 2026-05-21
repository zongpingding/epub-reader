{-# LANGUAGE OverloadedStrings #-}
module UI.Render 
    ( renderBlock 
    ) where

import Types
import Utils (resolvePath, unEscapeUrl)
import Epub (getZipEntryBytes)

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import qualified GI.GLib as GLib
import qualified Codec.Archive.Zip as Zip
import qualified Data.Text as T

renderBlock :: Config -> Gtk.Box -> FilePath -> Zip.Archive -> ContentBlock -> IO ()
renderBlock cfg box _ _ (TextBlock pangoText) = do
    lbl <- Gtk.labelNew Nothing
    Gtk.widgetAddCssClass lbl "reader-text"
    Gtk.labelSetUseMarkup lbl True
    Gtk.labelSetWrap lbl True
    Gtk.labelSetJustify lbl Gtk.JustificationFill 
    Gtk.widgetSetHexpand lbl True
    Gtk.labelSetXalign lbl 0.0 
    
    let isTitle = "<span size=" `T.isPrefixOf` pangoText
        indentStr = if not isTitle && cfgIndentFirst cfg > 0
                    then T.replicate (cfgIndentFirst cfg) "　" 
                    else ""
                    
    Gtk.labelSetMarkup lbl (indentStr <> pangoText)
    Gtk.boxAppend box lbl
renderBlock _ box currentDir archive (ImgBlock src) = do
    let imgPath = unEscapeUrl (resolvePath currentDir src)
    case getZipEntryBytes archive imgPath of
        Nothing -> return ()
        Just bs -> do
            gbytes  <- GLib.bytesNew (Just bs)
            texture <- Gdk.textureNewFromBytes gbytes
            picture <- Gtk.pictureNewForPaintable (Just texture)
            Gtk.boxAppend box picture
