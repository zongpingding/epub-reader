{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module UI (activateApp) where

import Types
import Epub
import Utils
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IORef (modifyIORef)

import qualified GI.Gtk as Gtk
import qualified GI.Gio as Gio
import qualified GI.Gdk as Gdk
import qualified GI.GLib as GLib
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Archive.Zip as Zip
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Exception (catch, SomeException)
import Control.Monad (when, void)
import Data.Foldable (forM_)
import System.FilePath.Posix (takeDirectory)

clearBox :: Gtk.Box -> IO ()
clearBox box = Gtk.widgetGetFirstChild box >>= \case
    Just child -> Gtk.boxRemove box child >> clearBox box
    Nothing    -> return ()

-- | filter toc nodes should be rendered currently, with sub-nodes status
-- return val: [(raw_idx, toc_node, contain_sub_nodes_Q)]
filterVisible :: Set Int -> [(Int, TocEntry, Bool)] -> [(Int, TocEntry, Bool)]
filterVisible _ [] = []
filterVisible collapsed ((i, e, hc):xs) =
    let current = (i, e, hc)
    in if hc && Set.member i collapsed
       -- if current node is collapsed, skip all of the following nodes whose tocLevel is more than before
       then current : filterVisible collapsed (dropWhile (\(_, child, _) -> tocLevel child > tocLevel e) xs)
       else current : filterVisible collapsed xs

clearListBox :: Gtk.ListBox -> IO ()
clearListBox listBox = Gtk.widgetGetFirstChild listBox >>= \case
    Just child -> Gtk.listBoxRemove listBox child >> clearListBox listBox
    Nothing    -> return ()

loadChapter :: IORef (Maybe AppState) -> Gtk.Box -> Gtk.Adjustment -> IO () -> IO ()
loadChapter stateRef box vadj updateHF = do
    mst <- readIORef stateRef
    case mst of
        Nothing -> return ()
        Just st -> do
            clearBox box
            let idx = appChapIdx st
                spine = appSpine st

            if idx >= 0 && idx < length spine
                then do
                    let currentPath = spine !! idx
                        currentDir = takeDirectory currentPath
                        blocks = getChapterBlocks (appArchive st) currentPath

                    if null blocks
                        then do
                            writeIORef stateRef (Just st{ appChapIdx = idx + 1 })
                            loadChapter stateRef box vadj updateHF
                        else do
                            mapM_ (renderBlock box currentDir (appArchive st)) blocks
                            Gtk.adjustmentSetValue vadj 0
                            void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE (updateHF >> return False)
                else do
                    lbl <- Gtk.labelNew Nothing
                    Gtk.labelSetUseMarkup lbl True
                    Gtk.labelSetMarkup lbl "\n\n<span size='xx-large'>Here is the End of The Book.</span>"
                    Gtk.boxAppend box lbl

renderBlock :: Gtk.Box -> FilePath -> Zip.Archive -> ContentBlock -> IO ()
renderBlock box _ _ (TextBlock pangoText) = do
    lbl <- Gtk.labelNew Nothing
    Gtk.labelSetUseMarkup lbl True
    Gtk.labelSetWrap lbl True
    Gtk.widgetSetHexpand lbl True
    Gtk.labelSetXalign lbl 0.0
    Gtk.labelSetMarkup lbl pangoText
    Gtk.boxAppend box lbl
renderBlock box currentDir archive (ImgBlock src) = do
    let imgPath = unEscapeUrl (resolvePath currentDir src)
    case getZipEntryBytes archive imgPath of
        Nothing -> return ()
        Just bs -> do
            gbytes <- GLib.bytesNew (Just bs)
            texture <- Gdk.textureNewFromBytes gbytes
            picture <- Gtk.pictureNewForPaintable (Just texture)
            Gtk.boxAppend box picture

pageDown, pageUp :: IORef (Maybe AppState) -> Gtk.Box -> Gtk.Adjustment -> IO () -> IO ()
pageDown stateRef box vadj updateHF = do
    mst <- readIORef stateRef
    case mst of
        Nothing -> return ()
        Just st -> do
            val <- Gtk.adjustmentGetValue vadj
            pageSize <- Gtk.adjustmentGetPageSize vadj
            upper <- Gtk.adjustmentGetUpper vadj
            if val + pageSize >= upper - 1.0
                then do
                    when (appChapIdx st < length (appSpine st) - 1) $ do
                        writeIORef stateRef (Just st{ appChapIdx = appChapIdx st + 1 })
                        loadChapter stateRef box vadj updateHF
                else Gtk.adjustmentSetValue vadj (val + pageSize)

pageUp stateRef box vadj updateHF = do
    mst <- readIORef stateRef
    case mst of
        Nothing -> return ()
        Just st -> do
            val <- Gtk.adjustmentGetValue vadj
            pageSize <- Gtk.adjustmentGetPageSize vadj
            if val <= 1.0
                then do
                    when (appChapIdx st > 0) $ do
                        writeIORef stateRef (Just st{ appChapIdx = appChapIdx st - 1 })
                        loadChapter stateRef box vadj updateHF
                else Gtk.adjustmentSetValue vadj (max 0 (val - pageSize))

activateApp :: Gtk.Application -> Config -> IO ()
activateApp app cfg = do
    win <- Gtk.applicationWindowNew app
    Gtk.windowSetTitle win (Just "Haskell EPUB Reader")
    Gtk.windowSetDefaultSize win 800 1000

    provider <- Gtk.cssProviderNew
    let updateCss fontSize = do
            let css = T.concat
                    [ "window { background-color: ", cfgBgColor cfg, "; } "
                    , "label { color: ", cfgFontColor cfg
                    , "; font-family: '", cfgFontFamily cfg
                    , "'; font-size: ", T.pack (show fontSize), "px"
                    , "; line-height: ", T.pack (show $ cfgLineHeight cfg), "; } "
                    , "list { background-color: transparent; } "
                    , "row { padding: 4px 15px; font-family: '", cfgFontFamily cfg
                    , "'; font-size: ", T.pack (show fontSize), "px"
                    , "; color: ", cfgFontColor cfg
                    , "; line-height: ", T.pack (show $ cfgTocLineHeight cfg), "; } "
                    , "row:selected { background-color: #dcd3be; } "
                    , ".header-label { color: ", cfgHeaderColor cfg, "; font-size: ", T.pack (show $ cfgHeaderSize cfg), "px; }"
                    , ".footer-label { color: ", cfgFooterColor cfg, "; font-size: ", T.pack (show $ cfgFooterSize cfg), "px; }"
                    ]
            Gtk.cssProviderLoadFromString provider css

    updateCss (cfgFontSize cfg)

    mDisplay <- Gdk.displayGetDefault
    case mDisplay of
        Just display -> Gtk.styleContextAddProviderForDisplay display provider (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)
        Nothing -> return ()

    stack <- Gtk.stackNew
    Gtk.stackSetTransitionType stack Gtk.StackTransitionTypeCrossfade

    -- ================= Level 1: Reading Page =================
    pageContainer <- Gtk.boxNew Gtk.OrientationVertical 0
    Gtk.widgetSetMarginStart pageContainer (cfgMarginLeft cfg)
    Gtk.widgetSetMarginEnd pageContainer (cfgMarginRight cfg)
    Gtk.widgetSetMarginTop pageContainer (cfgMarginTop cfg)
    Gtk.widgetSetMarginBottom pageContainer (cfgMarginBottom cfg)

    headerBox <- Gtk.boxNew Gtk.OrientationHorizontal 0
    hLeft <- Gtk.labelNew (Just "")
    hCenter <- Gtk.labelNew (Just "")
    hRight <- Gtk.labelNew (Just "")
    Gtk.widgetSetHalign hLeft Gtk.AlignStart
    Gtk.widgetSetHalign hCenter Gtk.AlignCenter
    Gtk.widgetSetHalign hRight Gtk.AlignEnd
    mapM_ (\w -> Gtk.widgetSetHexpand w True >> Gtk.widgetAddCssClass w "header-label") [hLeft, hCenter, hRight]
    mapM_ (Gtk.boxAppend headerBox) [hLeft, hCenter, hRight]
    Gtk.widgetSetMarginBottom headerBox 15

    readBox <- Gtk.boxNew Gtk.OrientationVertical (cfgBlockSpacing cfg)
    readScroll <- Gtk.scrolledWindowNew
    Gtk.scrolledWindowSetPolicy readScroll Gtk.PolicyTypeNever Gtk.PolicyTypeExternal
    Gtk.widgetSetVexpand readScroll True
    Gtk.scrolledWindowSetChild readScroll (Just readBox)
    vadj <- Gtk.scrolledWindowGetVadjustment readScroll

    footerBox <- Gtk.boxNew Gtk.OrientationHorizontal 0
    fLeft <- Gtk.labelNew (Just "")
    fCenter <- Gtk.labelNew (Just "")
    fRight <- Gtk.labelNew (Just "")
    Gtk.widgetSetHalign fLeft Gtk.AlignStart
    Gtk.widgetSetHalign fCenter Gtk.AlignCenter
    Gtk.widgetSetHalign fRight Gtk.AlignEnd
    mapM_ (\w -> Gtk.widgetSetHexpand w True >> Gtk.widgetAddCssClass w "footer-label") [fLeft, fCenter, fRight]
    mapM_ (Gtk.boxAppend footerBox) [fLeft, fCenter, fRight]
    Gtk.widgetSetMarginTop footerBox 15

    Gtk.boxAppend pageContainer headerBox
    Gtk.boxAppend pageContainer readScroll
    Gtk.boxAppend pageContainer footerBox

    void $ Gtk.stackAddNamed stack pageContainer (Just "page")

    -- ================= Level 2: TOC Page =================
    tocScroll <- Gtk.scrolledWindowNew
    tocListBox <- Gtk.listBoxNew
    Gtk.widgetSetMarginStart tocListBox (cfgMarginLeft cfg)
    Gtk.widgetSetMarginEnd tocListBox (cfgMarginRight cfg)
    Gtk.widgetSetMarginTop tocListBox (cfgMarginTop cfg)
    Gtk.widgetSetMarginBottom tocListBox (cfgMarginBottom cfg)
    Gtk.scrolledWindowSetChild tocScroll (Just tocListBox)
    void $ Gtk.stackAddNamed stack tocScroll (Just "toc")

    stateRef :: IORef (Maybe AppState) <- newIORef Nothing
    tocCollapsedRef <- newIORef (Set.empty :: Set Int)
    visualToSpineRef <- newIORef ([] :: [Int])  -- map 'Index' of visual lines to real 'Spine Index'

    let updateHeaderFooter = do
            mst <- readIORef stateRef
            case mst of
                Nothing -> return ()
                Just st -> do
                    let idx = appChapIdx st
                        spine = appSpine st
                        totalChaps = length spine

                        chapTitle = if idx >= 0 && idx < totalChaps
                                    then getChapterTitle (appArchive st) (spine !! idx)
                                    else ""

                    val <- Gtk.adjustmentGetValue vadj
                    pageSize <- Gtk.adjustmentGetPageSize vadj
                    upper <- Gtk.adjustmentGetUpper vadj

                    let totalP = max 1 (ceiling (upper / pageSize) :: Int)
                        currP  = min totalP (floor (val / pageSize) + 1 :: Int)

                    let fillTemplate templateStr = foldl (\acc (k, v) -> T.replace k v acc) templateStr
                            [ ("{book_title}", appBookTitle st)
                            , ("{chapter_name}", chapTitle)
                            , ("{chapter_index}", T.pack $ show (idx + 1))
                            , ("{total_chapter}", T.pack $ show totalChaps)
                            , ("{current_page}", T.pack $ show currP)
                            , ("{total_page}", T.pack $ show totalP)
                            ]

                    Gtk.labelSetText hLeft (fillTemplate $ cfgHeaderLeft cfg)
                    Gtk.labelSetText hCenter (fillTemplate $ cfgHeaderCenter cfg)
                    Gtk.labelSetText hRight (fillTemplate $ cfgHeaderRight cfg)

                    Gtk.labelSetText fLeft (fillTemplate $ cfgFooterLeft cfg)
                    Gtk.labelSetText fCenter (fillTemplate $ cfgFooterCenter cfg)
                    Gtk.labelSetText fRight (fillTemplate $ cfgFooterRight cfg)

    let renderTocList tocEntries = do
            clearListBox tocListBox
            collapsed <- readIORef tocCollapsedRef

            let indexed = zip [0..] tocEntries
                precalc = map (\(i, e) ->
                    let hc = case drop (i+1) tocEntries of
                                (next:_) -> tocLevel next > tocLevel e
                                [] -> False
                    in (i, e, hc)) indexed

            let visible = filterVisible collapsed precalc
            writeIORef visualToSpineRef (map (\(_, e, _) -> tocSpineIdx e) visible)

            forM_ visible $ \(idx, entry, hc) -> do
                rowBox <- Gtk.boxNew Gtk.OrientationHorizontal 4
                Gtk.widgetSetMarginStart rowBox (fromIntegral $ max 0 ((tocLevel entry - 1) * 20))

                if hc
                    then do
                        toggleBtn <- Gtk.buttonNew
                        Gtk.widgetAddCssClass toggleBtn "flat"
                        Gtk.widgetAddCssClass toggleBtn "circular"
                        let isCollapsed = Set.member idx collapsed
                            iconName = if isCollapsed then "pan-end-symbolic" else "pan-down-symbolic"
                        icon <- Gtk.imageNewFromIconName (Just iconName)
                        Gtk.buttonSetChild toggleBtn (Just icon)

                        void $ Gtk.onButtonClicked toggleBtn $ do
                            if isCollapsed
                                then modifyIORef tocCollapsedRef (Set.delete idx)
                                else modifyIORef tocCollapsedRef (Set.insert idx)
                            renderTocList tocEntries

                        Gtk.boxAppend rowBox toggleBtn
                    else do
                        -- NOTE: if no sub_nodes, use a blank Box(32px is the minimum buttom width in GTK)
                        spacer <- Gtk.boxNew Gtk.OrientationHorizontal 0
                        Gtk.widgetSetSizeRequest spacer 32 (-1)
                        Gtk.boxAppend rowBox spacer

                lbl <- Gtk.labelNew (Just $ tocTitle entry)
                Gtk.labelSetXalign lbl 0.0
                Gtk.widgetSetHexpand lbl True
                Gtk.boxAppend rowBox lbl

                row <- Gtk.listBoxRowNew
                Gtk.listBoxRowSetChild row (Just rowBox)
                Gtk.listBoxAppend tocListBox row

    void $ Gtk.onAdjustmentValueChanged vadj updateHeaderFooter
    void $ Gtk.onAdjustmentChanged vadj updateHeaderFooter

    let loadEpubFile path = catch (do
            archive <- Zip.toArchive <$> BL.readFile path
            let spine = extractSpine archive
                bookTitle = getBookTitle archive
                tocEntries = extractToc archive spine

            writeIORef stateRef (Just (AppState archive spine tocEntries 0 (cfgFontSize cfg) bookTitle))
            writeIORef tocCollapsedRef Set.empty
            renderTocList tocEntries

            loadChapter stateRef readBox vadj updateHeaderFooter
            Gtk.stackSetVisibleChildName stack "page"
            void $ Gtk.widgetGrabFocus win
            ) (\(e :: SomeException) -> do
                clearBox readBox
                lbl <- Gtk.labelNew Nothing
                Gtk.labelSetText lbl ("Failed to read the file：\n" <> T.pack (show e))
                Gtk.boxAppend readBox lbl)

    let openFileDialog = do
            dlg <- Gtk.fileDialogNew
            Gtk.fileDialogSetTitle dlg "Choose the EPUB file"
            Gtk.fileDialogOpen dlg (Just win) (Nothing :: Maybe Gio.Cancellable) (Just $ \_obj res -> do
                catch (do
                    file <- Gtk.fileDialogOpenFinish dlg res
                    mpath <- Gio.fileGetPath file
                    case mpath of
                        Just path -> loadEpubFile path
                        Nothing   -> return ()
                    ) (\(_ :: SomeException) -> return ())
                )

    void $ Gtk.onListBoxRowActivated tocListBox $ \row -> do
        vIdx <- Gtk.listBoxRowGetIndex row
        when (vIdx >= 0) $ do
            mapping <- readIORef visualToSpineRef
            when (fromIntegral vIdx < length mapping) $ do
                let targetIdx = mapping !! fromIntegral vIdx
                mst <- readIORef stateRef
                case mst of
                    Nothing -> return ()
                    Just st -> do
                        writeIORef stateRef (Just st{ appChapIdx = targetIdx })
                        loadChapter stateRef readBox vadj updateHeaderFooter
                        Gtk.stackSetVisibleChildName stack "page"
                        void $ Gtk.widgetGrabFocus win

    keyCtrl <- Gtk.eventControllerKeyNew
    Gtk.eventControllerSetPropagationPhase keyCtrl Gtk.PropagationPhaseCapture

    void $ Gtk.onEventControllerKeyKeyPressed keyCtrl $ \keyval _ modifiers -> do
        let isShift = Gdk.ModifierTypeShiftMask `elem` modifiers
            isCtrl  = Gdk.ModifierTypeControlMask `elem` modifiers
        currentChild <- Gtk.stackGetVisibleChildName stack

        mst <- readIORef stateRef
        case mst of
            Nothing -> do
                if (keyval == 111 || keyval == 79) && isCtrl
                    then openFileDialog >> return True
                else if keyval == 32
                    then return True
                else return False
            Just st -> do
                if (keyval == 111 || keyval == 79) && isCtrl
                    then openFileDialog >> return True
                else if keyval `elem` [43, 61, 65451]
                    then do
                        let newSize = appFontSize st + 2
                        writeIORef stateRef (Just st { appFontSize = newSize })
                        updateCss newSize
                        return True
                else if keyval `elem` [45, 65453]
                    then do
                        let newSize = max 8 (appFontSize st - 2)
                        writeIORef stateRef (Just st { appFontSize = newSize })
                        updateCss newSize
                        return True
                else if keyval `elem` [48, 65456]
                    then do
                        writeIORef stateRef (Just st { appFontSize = cfgFontSize cfg })
                        updateCss (cfgFontSize cfg)
                        return True
                else if currentChild == Just "toc"
                    then do
                        if keyval == 65289
                            then Gtk.stackSetVisibleChildName stack "page" >> Gtk.widgetGrabFocus win >> return True
                            else return False
                else do
                    if keyval == 65289
                        then Gtk.stackSetVisibleChildName stack "toc" >> Gtk.widgetGrabFocus tocListBox >> return True
                    else if keyval == 32 || keyval == 65363 || keyval == 65364
                        then do
                            if keyval == 32 && isShift
                                then pageUp stateRef readBox vadj updateHeaderFooter
                                else pageDown stateRef readBox vadj updateHeaderFooter
                            return True
                    else if keyval == 65361 || keyval == 65362
                        then pageUp stateRef readBox vadj updateHeaderFooter >> return True
                    else return False

    Gtk.widgetAddController win keyCtrl

    Gtk.windowSetChild win (Just stack)
    Gtk.windowPresent win
