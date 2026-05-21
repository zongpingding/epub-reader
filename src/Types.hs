module Types 
    ( Config(..)
    , TocEntry(..)
    , AppState(..)
    , ContentBlock(..)
    ) where

import Data.Int (Int32)
import qualified Data.Text as T


-- | Global config
data Config = Config
    { cfgMarginTop    :: !Int32,  cfgMarginBottom :: !Int32
    , cfgMarginLeft   :: !Int32,  cfgMarginRight  :: !Int32
    , cfgLineHeight   :: !Double, cfgBlockSpacing :: !Int32
    , cfgBgColor      :: !T.Text, cfgFontColor    :: !T.Text
    , cfgFontFamily   :: !T.Text, cfgFontSize     :: !Int
    , cfgTocLineHeight:: !Double, cfgTocSelectedBg:: !T.Text
    , cfgHeaderLeft   :: !T.Text, cfgHeaderCenter :: !T.Text, cfgHeaderRight :: !T.Text
    , cfgHeaderColor  :: !T.Text, cfgHeaderSize   :: !Int
    , cfgFooterLeft   :: !T.Text, cfgFooterCenter :: !T.Text, cfgFooterRight :: !T.Text
    , cfgFooterColor  :: !T.Text, cfgFooterSize   :: !Int
    , cfgIndentFirst  :: !Int
    , cfgShowTitlebar :: !Bool
    }

-- | Directory Tree node
data TocEntry = TocEntry
    { tocLevel   :: !Int
    , tocTitle   :: !T.Text
    , tocSpineIdx:: !Int
    }

-- | Kernel running state
data AppState = AppState
    { appEpubPath         :: !FilePath
    , appSpine            :: ![FilePath]  
    , appToc              :: ![TocEntry]
    , appChapIdx          :: !Int         
    , appFontSize         :: !Int    
    , appBookTitle        :: !T.Text       
    , appCurrentChapTitle :: !T.Text
    }

-- | Render Block
data ContentBlock 
    = TextBlock !T.Text 
    | ImgBlock !FilePath
