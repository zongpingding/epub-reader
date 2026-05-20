{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified GI.Gtk as Gtk
import qualified GI.Gio as Gio
import Control.Monad (void)
import System.Environment (getArgs)

import Config (loadConfigIO)
import UI (activateApp)

main :: IO ()
main = do
    -- load config(parse TOML)
    cfg <- loadConfigIO
    
    -- register & init GTK App
    app <- Gtk.applicationNew (Just "com.github.epub-reader") []
    void $ Gio.onApplicationActivate app (activateApp app cfg)
    
    -- main loop
    args <- getArgs
    void $ Gio.applicationRun app (Just args)
