{-# LANGUAGE LambdaCase #-}
module UI.Utils 
    ( clearBox
    , clearListBox
    , safeIndex
    , ordNub
    , strictLength
    ) where

import qualified GI.Gtk as Gtk
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set

clearBox :: Gtk.Box -> IO ()
clearBox box = Gtk.widgetGetFirstChild box >>= \case
    Just child -> Gtk.boxRemove box child >> clearBox box
    Nothing    -> return ()

clearListBox :: Gtk.ListBox -> IO ()
clearListBox listBox = Gtk.widgetGetFirstChild listBox >>= \case
    Just child -> Gtk.listBoxRemove listBox child >> clearListBox listBox
    Nothing    -> return ()

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
    | i < 0     = Nothing
    | otherwise = listToMaybe (drop i xs)

ordNub :: Ord a => [a] -> [a]
ordNub = go Set.empty
  where
    go _ [] = []
    go s (x:xs)
      | Set.member x s = go s xs
      | otherwise      = x : go (Set.insert x s) xs

strictLength :: [a] -> Int
strictLength = foldl' (\c _ -> c + 1) 0
