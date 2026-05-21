module UI.Types 
    ( TocRow(..)
    , ReadBuffer(..)
    , ScrollDir(..)
    ) where

import Types (TocEntry)
import qualified GI.Gtk as Gtk
import qualified Data.Text as T

data TocRow = TocRow 
    { trIdx      :: !Int
    , trEntry    :: !TocEntry
    , trHasChild :: !Bool
    , trRowWidget:: !Gtk.ListBoxRow
    , trIcon     :: !(Maybe Gtk.Image)
    }

data ReadBuffer = ReadBuffer
    { bufName   :: !T.Text
    , bufScroll :: !Gtk.ScrolledWindow
    , bufBox    :: !Gtk.Box
    , bufVadj   :: !Gtk.Adjustment
    }

data ScrollDir = ScrollNext | ScrollPrev | ScrollNone deriving (Eq, Show)
