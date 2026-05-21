{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module UI (activateApp) where

import Types
import Epub
import UI.Types
import UI.Utils
import UI.Toc
import UI.History
import UI.Render

import qualified GI.Gtk as Gtk
import qualified GI.Gio as Gio
import qualified GI.Gdk as Gdk
import qualified GI.GLib as GLib
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Archive.Zip as Zip
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Control.Exception (catch, SomeException)
import Control.Monad (when, void, forM, forM_)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (find)
import System.FilePath.Posix (takeDirectory)

activateApp :: Gtk.Application -> Config -> IO ()
activateApp app cfg = do
    win <- Gtk.applicationWindowNew app
    Gtk.windowSetTitle win (Just "Haskell EPUB Reader")
    Gtk.windowSetDefaultSize win 800 1000

    Gtk.windowSetDecorated win (cfgShowTitlebar cfg)
    provider <- Gtk.cssProviderNew
    let updateCss fontSize = do
            let css = T.concat
                    [ "window { background-color: ", cfgBgColor cfg, "; } "
                    , ".reader-text { color: ", cfgFontColor cfg
                    , "; font-family: '", cfgFontFamily cfg
                    , "'; font-size: ", T.pack (show fontSize), "px"
                    , "; line-height: ", T.pack (show $ cfgLineHeight cfg)
                    , "; padding: 3px 0px; } " 
                    , "list { background-color: transparent; } "
                    , "row { padding: 4px 15px; font-family: '", cfgFontFamily cfg
                    , "'; font-size: ", T.pack (show fontSize), "px"
                    , "; color: ", cfgFontColor cfg
                    , "; line-height: ", T.pack (show $ cfgTocLineHeight cfg), "; } "
                    , "row:selected { background-color: ", cfgTocSelectedBg cfg, "; } "
                    , ".header-label { color: ", cfgHeaderColor cfg, "; font-size: ", T.pack (show $ cfgHeaderSize cfg), "px; }"
                    , ".footer-label { color: ", cfgFooterColor cfg, "; font-size: ", T.pack (show $ cfgFooterSize cfg), "px; }"
                    ]
            Gtk.cssProviderLoadFromString provider css
    updateCss (cfgFontSize cfg)

    mDisplay <- Gdk.displayGetDefault
    case mDisplay of
        Just display -> Gtk.styleContextAddProviderForDisplay display provider (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)
        Nothing -> return ()

    stateRef :: IORef (Maybe AppState) <- newIORef Nothing
    tocRowsRef <- newIORef ([] :: [TocRow])
    tocCollapsedRef <- newIORef (Set.empty :: Set Int)

    mainStack <- Gtk.stackNew
    Gtk.stackSetTransitionType mainStack Gtk.StackTransitionTypeCrossfade
    Gtk.windowSetChild win (Just mainStack)

    -- ================= Level 0: History =================
    historyContainer <- Gtk.boxNew Gtk.OrientationVertical 20
    Gtk.widgetSetMarginStart historyContainer 100
    Gtk.widgetSetMarginEnd historyContainer 100
    Gtk.widgetSetMarginTop historyContainer 80

    histTitle <- Gtk.labelNew (Just "<span size='xx-large' weight='bold'>Reading History</span>")
    Gtk.labelSetUseMarkup histTitle True
    Gtk.boxAppend historyContainer histTitle

    historyList <- Gtk.listBoxNew
    histScroll <- Gtk.scrolledWindowNew
    Gtk.scrolledWindowSetChild histScroll (Just historyList)
    Gtk.widgetSetVexpand histScroll True
    Gtk.boxAppend historyContainer histScroll
    void $ Gtk.stackAddNamed mainStack historyContainer (Just "history")

    -- ================= Level 1: Reading page / buffers =================
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

    chapStack <- Gtk.stackNew
    Gtk.widgetSetVexpand chapStack True

    let setupBuffer bName = do
            box <- Gtk.boxNew Gtk.OrientationVertical (cfgBlockSpacing cfg)
            Gtk.widgetSetHalign box Gtk.AlignCenter
            Gtk.widgetSetSizeRequest box 800 (-1)

            scroll <- Gtk.scrolledWindowNew
            Gtk.scrolledWindowSetPolicy scroll Gtk.PolicyTypeNever Gtk.PolicyTypeExternal
            Gtk.scrolledWindowSetChild scroll (Just box)
            vadj <- Gtk.scrolledWindowGetVadjustment scroll
            return $ ReadBuffer bName scroll box vadj

    bufA <- setupBuffer "bufA"
    bufB <- setupBuffer "bufB"
    activeBufRef <- newIORef (0 :: Int)

    void $ Gtk.stackAddNamed chapStack (bufScroll bufA) (Just "bufA")
    void $ Gtk.stackAddNamed chapStack (bufScroll bufB) (Just "bufB")

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
    Gtk.boxAppend pageContainer chapStack
    Gtk.boxAppend pageContainer footerBox
    void $ Gtk.stackAddNamed mainStack pageContainer (Just "page")

    -- ================= Level 2: TOC page =================
    tocScroll <- Gtk.scrolledWindowNew
    tocListBox <- Gtk.listBoxNew
    Gtk.widgetSetMarginStart tocListBox (cfgMarginLeft cfg)
    Gtk.widgetSetMarginEnd tocListBox (cfgMarginRight cfg)
    Gtk.widgetSetMarginTop tocListBox (cfgMarginTop cfg)
    Gtk.widgetSetMarginBottom tocListBox (cfgMarginBottom cfg)
    Gtk.scrolledWindowSetChild tocScroll (Just tocListBox)
    void $ Gtk.stackAddNamed mainStack tocScroll (Just "toc")

    -- ================= Control Logic =================
    let renderHistoryUI = do
            clearListBox historyList
            validFiles <- readHistory
            if null validFiles
                then do
                    emptyLbl <- Gtk.labelNew (Just "NO-HISTORY, Press 'Ctrl+O' to open an EPUB file.")
                    Gtk.widgetSetMarginTop emptyLbl 20
                    Gtk.listBoxAppend historyList emptyLbl
                else do
                    forM_ validFiles $ \f -> do
                        lbl <- Gtk.labelNew (Just f)
                        Gtk.labelSetXalign lbl 0.0
                        Gtk.widgetSetMarginTop lbl 10
                        Gtk.widgetSetMarginBottom lbl 10
                        row <- Gtk.listBoxRowNew
                        Gtk.listBoxRowSetChild row (Just lbl)
                        Gtk.listBoxAppend historyList row

    Gtk.stackSetVisibleChildName mainStack "history"
    renderHistoryUI

    let updateHeaderFooter = do
            mst <- readIORef stateRef
            case mst of
                Nothing -> return ()
                Just st -> do
                    activeIdx <- readIORef activeBufRef
                    let activeBuf = if activeIdx == 0 then bufA else bufB
                        vadj = bufVadj activeBuf

                    val <- Gtk.adjustmentGetValue vadj
                    pageSize <- Gtk.adjustmentGetPageSize vadj
                    upper <- Gtk.adjustmentGetUpper vadj

                    let totalP = max 1 (ceiling (upper / pageSize) :: Int)
                        isAtBottom = val + pageSize >= upper - 1.0
                        currP = if pageSize > 0
                                then if isAtBottom
                                     then totalP
                                     else min totalP (floor (val / pageSize) + 1 :: Int)
                                else 1

                    let totalChaps = strictLength (appSpine st)
                        fillTemplate templateStr = foldl' (\acc (k, v) -> T.replace k v acc) templateStr
                            [ ("{book_title}", appBookTitle st)
                            , ("{chapter_name}", appCurrentChapTitle st)
                            , ("{chapter_index}", T.pack $ show (appChapIdx st + 1))
                            , ("{total_chapter}", T.pack $ show totalChaps)
                            , ("{current_page}", T.pack $ show currP)
                            , ("{total_page}", T.pack $ show totalP)
                            ]

                    Gtk.labelSetText hLeft   (fillTemplate $ cfgHeaderLeft   cfg)
                    Gtk.labelSetText hCenter (fillTemplate $ cfgHeaderCenter cfg)
                    Gtk.labelSetText hRight  (fillTemplate $ cfgHeaderRight  cfg)
                    Gtk.labelSetText fLeft   (fillTemplate $ cfgFooterLeft   cfg)
                    Gtk.labelSetText fCenter (fillTemplate $ cfgFooterCenter cfg)
                    Gtk.labelSetText fRight  (fillTemplate $ cfgFooterRight  cfg)

    let attachSignals buf = do
            void $ Gtk.onAdjustmentValueChanged (bufVadj buf) updateHeaderFooter
            void $ Gtk.onAdjustmentChanged (bufVadj buf) updateHeaderFooter

    attachSignals bufA
    attachSignals bufB

    let loadChapter dir = do
            mst <- readIORef stateRef
            case mst of
                Nothing -> return ()
                Just st -> do
                    activeIdx <- readIORef activeBufRef
                    let inactiveBuf = if activeIdx == 0 then bufB else bufA
                        idx = appChapIdx st
                        spine = appSpine st

                    case safeIndex spine idx of
                        Just currentPath -> do
                            let currentDir = takeDirectory currentPath
                            archive <- Zip.toArchive <$> BL.readFile (appEpubPath st)
                            let blocks = getChapterBlocks archive currentPath
                                chapTitle = getChapterTitle archive currentPath

                            writeIORef stateRef (Just st { appCurrentChapTitle = chapTitle })

                            if null blocks
                                then do
                                    let nextIdx = if dir == ScrollPrev then idx - 1 else idx + 1
                                    writeIORef stateRef (Just st{ appChapIdx = nextIdx })
                                    loadChapter dir
                                else do
                                    clearBox (bufBox inactiveBuf)
                                    mapM_ (renderBlock cfg (bufBox inactiveBuf) currentDir archive) blocks

                                    if dir == ScrollPrev
                                        then do
                                            Gtk.stackSetVisibleChildFull chapStack (bufName inactiveBuf) Gtk.StackTransitionTypeSlideRight
                                            let vadj = bufVadj inactiveBuf
                                            let checkLayout lastUpper stableCount retries = do
                                                    upper <- Gtk.adjustmentGetUpper vadj
                                                    pageSize <- Gtk.adjustmentGetPageSize vadj
                                                    Gtk.adjustmentSetValue vadj (max 0 (upper - pageSize))

                                                    if upper == lastUpper && upper > 0
                                                        then do
                                                            if stableCount >= 2
                                                                then return False
                                                                else do
                                                                    void $ GLib.timeoutAdd GLib.PRIORITY_DEFAULT 20 (checkLayout upper (stableCount + 1) (retries - 1))
                                                                    return False
                                                        else if retries > (0 :: Int)
                                                            then do
                                                                void $ GLib.timeoutAdd GLib.PRIORITY_DEFAULT 20 (checkLayout upper 0 (retries - 1))
                                                                return False
                                                            else return False
                                            void $ GLib.timeoutAdd GLib.PRIORITY_DEFAULT 20 (checkLayout (-1.0 :: Double) (0 :: Int) (50 :: Int))
                                        else do
                                            Gtk.adjustmentSetValue (bufVadj inactiveBuf) 0
                                            let trans = if dir == ScrollNext then Gtk.StackTransitionTypeSlideLeft else Gtk.StackTransitionTypeCrossfade
                                            Gtk.stackSetVisibleChildFull chapStack (bufName inactiveBuf) trans

                                    writeIORef activeBufRef (if activeIdx == 0 then 1 else 0)
                                    void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE (updateHeaderFooter >> return False)
                        Nothing -> do
                            clearBox (bufBox inactiveBuf)
                            lbl <- Gtk.labelNew Nothing
                            Gtk.labelSetUseMarkup lbl True
                            Gtk.labelSetMarkup lbl "\n\n<span size='xx-large'>No more content</span>"
                            Gtk.boxAppend (bufBox inactiveBuf) lbl
                            Gtk.stackSetVisibleChildFull chapStack (bufName inactiveBuf) Gtk.StackTransitionTypeCrossfade
                            writeIORef activeBufRef (if activeIdx == 0 then 1 else 0)

    let pageDown = do
            mst <- readIORef stateRef
            case mst of
                Nothing -> return ()
                Just st -> do
                    activeIdx <- readIORef activeBufRef
                    let activeBuf = if activeIdx == 0 then bufA else bufB
                    val <- Gtk.adjustmentGetValue (bufVadj activeBuf)
                    pageSize <- Gtk.adjustmentGetPageSize (bufVadj activeBuf)
                    upper <- Gtk.adjustmentGetUpper (bufVadj activeBuf)

                    if val + pageSize >= upper - 1.0
                        then do
                            when (appChapIdx st < strictLength (appSpine st) - 1) $ do
                                writeIORef stateRef (Just st{ appChapIdx = appChapIdx st + 1 })
                                loadChapter ScrollNext
                        else do
                            let nextVal = (fromIntegral (floor (val / pageSize) :: Int) + 1.0) * pageSize
                            Gtk.adjustmentSetValue (bufVadj activeBuf) (min nextVal (upper - pageSize))

    let pageUp = do
            mst <- readIORef stateRef
            case mst of
                Nothing -> return ()
                Just st -> do
                    activeIdx <- readIORef activeBufRef
                    let activeBuf = if activeIdx == 0 then bufA else bufB
                    val <- Gtk.adjustmentGetValue (bufVadj activeBuf)
                    pageSize <- Gtk.adjustmentGetPageSize (bufVadj activeBuf)

                    if val <= 1.0
                        then do
                            when (appChapIdx st > 0) $ do
                                writeIORef stateRef (Just st{ appChapIdx = appChapIdx st - 1 })
                                loadChapter ScrollPrev
                        else do
                            let nextVal = (fromIntegral (ceiling ((val - 0.001) / pageSize) :: Int) - 1.0) * pageSize
                            Gtk.adjustmentSetValue (bufVadj activeBuf) (max 0 nextVal)

    let updateTocVisibility = do
            collapsed <- readIORef tocCollapsedRef
            rows <- readIORef tocRowsRef
            let precalc = map (\tr -> (trIdx tr, trEntry tr, trHasChild tr)) rows
                visibleList = filterVisible collapsed precalc
                visibleSet = Set.fromList $ map (\(i,_,_) -> i) visibleList

            forM_ rows $ \tr -> do
                let isVis = Set.member (trIdx tr) visibleSet
                Gtk.widgetSetVisible (trRowWidget tr) isVis
                case trIcon tr of
                    Just img -> Gtk.imageSetFromIconName img (Just $ if Set.member (trIdx tr) collapsed then "pan-end-symbolic" else "pan-down-symbolic")
                    Nothing  -> return ()

    let loadEpubFile path = catch (do
            writeHistory path
            archive <- Zip.toArchive <$> BL.readFile path
            let spine = extractSpine archive
                bookTitle = getBookTitle archive
                tocEntries = extractToc archive spine
                initialChapTitle = case spine of
                                     (firstChap:_) -> getChapterTitle archive firstChap
                                     []            -> ""

            writeIORef stateRef (Just (AppState path spine tocEntries 0 (cfgFontSize cfg) bookTitle initialChapTitle))

            clearListBox tocListBox
            let indexed = zip [0..] tocEntries
                precalc = map (\(i, e) ->
                    let hc = case drop (i+1) tocEntries of
                                (next:_) -> tocLevel next > tocLevel e
                                [] -> False
                    in (i, e, hc)) indexed

            rowsData <- forM precalc $ \(idx, entry, hc) -> do
                row <- Gtk.listBoxRowNew
                rowBox <- Gtk.boxNew Gtk.OrientationHorizontal 4
                Gtk.widgetSetMarginStart rowBox (fromIntegral $ max 0 ((tocLevel entry - 1) * 20))

                mIcon <- if hc
                    then do
                        toggleBtn <- Gtk.buttonNew
                        Gtk.widgetAddCssClass toggleBtn "flat"
                        Gtk.widgetAddCssClass toggleBtn "circular"
                        icon <- Gtk.imageNewFromIconName (Just "pan-down-symbolic")
                        Gtk.buttonSetChild toggleBtn (Just icon)

                        void $ Gtk.onButtonClicked toggleBtn $ do
                            collapsed <- readIORef tocCollapsedRef
                            if Set.member idx collapsed
                                then modifyIORef tocCollapsedRef (Set.delete idx)
                                else modifyIORef tocCollapsedRef (Set.insert idx)
                            updateTocVisibility

                        Gtk.boxAppend rowBox toggleBtn
                        return (Just icon)
                    else do
                        spacer <- Gtk.boxNew Gtk.OrientationHorizontal 0
                        Gtk.widgetSetSizeRequest spacer 32 (-1)
                        Gtk.boxAppend rowBox spacer
                        return Nothing

                lbl <- Gtk.labelNew (Just $ tocTitle entry)
                Gtk.labelSetXalign lbl 0.0
                Gtk.widgetSetHexpand lbl True
                Gtk.boxAppend rowBox lbl

                Gtk.listBoxRowSetChild row (Just rowBox)
                Gtk.listBoxAppend tocListBox row
                return (TocRow idx entry hc row mIcon)

            writeIORef tocRowsRef rowsData
            writeIORef tocCollapsedRef Set.empty
            updateTocVisibility

            loadChapter ScrollNone
            Gtk.stackSetVisibleChildName mainStack "page"
            void $ Gtk.widgetGrabFocus win
            ) (\(e :: SomeException) -> do
                activeIdx <- readIORef activeBufRef
                let activeBuf = if activeIdx == 0 then bufA else bufB
                clearBox (bufBox activeBuf)
                lbl <- Gtk.labelNew Nothing
                Gtk.labelSetText lbl ("Failed to read file：\n" <> T.pack (show e))
                Gtk.boxAppend (bufBox activeBuf) lbl)

    void $ Gtk.onListBoxRowActivated historyList $ \row -> do
        mChild <- Gtk.widgetGetFirstChild row
        case mChild of
            Just child -> do
                mLabel <- Gtk.castTo Gtk.Label child
                case mLabel of
                    Just l -> do
                        txt <- Gtk.labelGetText l
                        if "NO-HISTORY" `T.isPrefixOf` txt
                            then return ()
                            else loadEpubFile (T.unpack txt)
                    Nothing -> return ()
            Nothing -> return ()

    let openFileDialog = do
            dlg <- Gtk.fileDialogNew
            Gtk.fileDialogSetTitle dlg "Choose an EPUB file"
            Gtk.fileDialogOpen dlg (Just win) (Nothing :: Maybe Gio.Cancellable) (Just $ \_obj res -> do
                catch (do
                    file <- Gtk.fileDialogOpenFinish dlg res
                    mpath <- Gio.fileGetPath file
                    case mpath of
                        Just path -> loadEpubFile path
                        Nothing   -> return ()
                    ) (\(_ :: SomeException) -> return ())
                )

    let handleTocExpand isCollapse = do
            mRow <- Gtk.listBoxGetSelectedRow tocListBox
            case mRow of
                Just row -> do
                    idx <- fromIntegral <$> Gtk.listBoxRowGetIndex row
                    rows <- readIORef tocRowsRef
                    collapsed <- readIORef tocCollapsedRef
                    case safeIndex rows idx of
                        Just tr -> do
                            if isCollapse
                                then if trHasChild tr && not (Set.member idx collapsed)
                                     then modifyIORef tocCollapsedRef (Set.insert idx) >> updateTocVisibility
                                     else do
                                         let currentLevel = tocLevel (trEntry tr)
                                             pMaybe = find (\i -> case safeIndex rows i of
                                                                      Just parentTr -> tocLevel (trEntry parentTr) < currentLevel
                                                                      Nothing -> False) [idx-1, idx-2 .. 0]
                                         case pMaybe of
                                             Just pIdx ->
                                                 case safeIndex rows pIdx of
                                                     Just parentTr -> do
                                                         modifyIORef tocCollapsedRef (Set.insert pIdx)
                                                         updateTocVisibility
                                                         void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ do
                                                             Gtk.listBoxSelectRow tocListBox (Just (trRowWidget parentTr))
                                                             void $ Gtk.widgetGrabFocus (trRowWidget parentTr)
                                                             return False
                                                     Nothing -> return ()
                                             Nothing -> return ()
                                else when (trHasChild tr) $ modifyIORef tocCollapsedRef (Set.delete idx) >> updateTocVisibility
                        Nothing -> return ()
                Nothing -> return ()

    void $ Gtk.onListBoxRowActivated tocListBox $ \row -> do
        vIdx <- Gtk.listBoxRowGetIndex row
        rows <- readIORef tocRowsRef
        case safeIndex rows (fromIntegral vIdx) of
            Just tr -> do
                let targetIdx = tocSpineIdx (trEntry tr)
                mst <- readIORef stateRef
                case mst of
                    Nothing -> return ()
                    Just st -> do
                        writeIORef stateRef (Just st{ appChapIdx = targetIdx })
                        loadChapter ScrollNone
                        Gtk.stackSetVisibleChildName mainStack "page"
                        void $ Gtk.widgetGrabFocus win
            Nothing -> return ()

    keyCtrl <- Gtk.eventControllerKeyNew
    Gtk.eventControllerSetPropagationPhase keyCtrl Gtk.PropagationPhaseCapture

    void $ Gtk.onEventControllerKeyKeyPressed keyCtrl $ \keyval _ modifiers -> do
        let isShift = Gdk.ModifierTypeShiftMask `elem` modifiers
            isCtrl  = Gdk.ModifierTypeControlMask `elem` modifiers
        currentChild <- Gtk.stackGetVisibleChildName mainStack

        mst <- readIORef stateRef
        case mst of
            Nothing -> do
                if (keyval == 111 || keyval == 79) && isCtrl
                    then openFileDialog >> return True
                else if (keyval == 104 || keyval == 72) && isCtrl
                    then renderHistoryUI >> Gtk.stackSetVisibleChildName mainStack "history" >> return True
                else return False
            Just st -> do
                if (keyval == 111 || keyval == 79) && isCtrl
                    then openFileDialog >> return True
                else if (keyval == 104 || keyval == 72) && isCtrl
                    then do
                        if currentChild == Just "history"
                            then Gtk.stackSetVisibleChildName mainStack "page" >> void (Gtk.widgetGrabFocus win)
                            else renderHistoryUI >> Gtk.stackSetVisibleChildName mainStack "history"
                        return True
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
                else if currentChild == Just "toc" || currentChild == Just "history"
                    then do
                        case keyval of
                            65289 -> do
                                Gtk.stackSetVisibleChildName mainStack "page"
                                void $ Gtk.widgetGrabFocus win
                                return True
                            65361 -> do
                                handleTocExpand True
                                return True
                            65363 -> do
                                handleTocExpand False
                                return True
                            _ -> return False
                else do
                    if keyval == 65289
                        then do
                            Gtk.stackSetVisibleChildName mainStack "toc"
                            rows <- readIORef tocRowsRef
                            let currentIdx = appChapIdx st
                            case find (\tr -> tocSpineIdx (trEntry tr) == currentIdx) rows of
                                Just r -> do
                                    Gtk.listBoxSelectRow tocListBox (Just $ trRowWidget r)
                                    void $ Gtk.widgetGrabFocus (trRowWidget r)
                                Nothing -> void $ Gtk.widgetGrabFocus tocListBox
                            return True
                    else if keyval == 32 || keyval == 65363 || keyval == 65364
                        then do
                            if keyval == 32 && isShift
                                then pageUp
                                else pageDown
                            return True
                    else if keyval == 65361 || keyval == 65362
                        then pageUp >> return True
                    else return False

    Gtk.widgetAddController win keyCtrl
    Gtk.windowPresent win
