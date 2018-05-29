{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Network
    ( bootstrap
    , network
    ) where

import Control.Monad.IO.Class
import Data.Functor.Compose
import Data.Functor.Syntax
import Data.Maybe
import Data.Text (Text, pack, unpack)
import Data.Traversable
import GI.Gtk
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.GI.Gtk
import System.Directory

import Format
import Hoogle
import UI

-- | Flipped '<$'
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

-- | Bootstrapping of Gio Application and creation of GUI
bootstrap :: Application -> MomentIO HoogleGTK
bootstrap app = do
    gui <- hoogleGTK
    activate <- signalE0 app #activate
    reactimate $ activate $> do
        set (window gui) [#application := app]
        widgetShowAll (window gui)
        checkDB gui
    pure gui

-- | Check for Hoogle database at standard location
checkDB :: MonadIO m => HoogleGTK -> m ()
checkDB gui = do
    hasDB <-
        liftIO $ do
            loc <- defaultDatabaseLocation
            doesFileExist loc
    if hasDB
        then set (stack gui) [#visibleChild := paned gui]
        else set (stack gui) [#visibleChild := noDatabase gui]

-- | Obtain an event from a 'SearchEntry' on @search-changed@. This will wait
-- 150ms after the last keystroke to emit the event
searchText :: SearchEntry -> MomentIO (Event Text)
searchText entry =
    mapEventIO (\_ -> get entry #text) =<< signalE0 entry #searchChanged

-- | Obtain Behavior for the currently selected index in a 'ListBox'
selectedRow :: ListBox -> MomentIO (Behavior (Maybe Int))
selectedRow listBox = do
    selected <- signalE1 listBox #rowSelected
    indexE <- mapEventIO (flip for listBoxRowGetIndex) selected
    stepper Nothing $ fromIntegral <$$> indexE

network :: HoogleGTK -> MomentIO ()
network gui
 = do
    -- search targets
    targets <-
        do search <- searchText (typeSearch gui)
           targetE <-
               flip mapEventIO search $ \s -> do
                   loc <- defaultDatabaseLocation
                   withDatabase loc $ \db ->
                       pure (take 50 $ searchDatabase db (unpack s))
           stepper [] targetE

    -- result list
    rowsChangeE <- changes $ map targetRow <$> targets
    reactimate' $ loadListBox (resultsList gui) <$$> rowsChangeE

    -- selection
    selection <- selectedRow (resultsList gui)
    let selectedTarget =
            getCompose $ (!!) <$> Compose (Just <$> targets) <*>
            Compose selection
        selectedDisplay =
            maybe emptyDisplayTarget displayTarget <$> selectedTarget
    sink (typeLabel gui) [#label :== dtargetType <$> selectedDisplay]
    sink (moduleLabel gui) [#label :== dtargetModule <$> selectedDisplay]
    sink (packageLabel gui) [#label :== dtargetPackage <$> selectedDisplay]
    sink (docsLabel gui) [#label :== dtargetDocs <$> selectedDisplay]
    sink
        (browserButton gui)
        [ #uri :== pack . fromMaybe "https://hackage.haskell.org" .
          fmap targetURL <$>
          selectedTarget
        ]

    -- orientation
    rotate <- signalE0 (rotateButton gui) #clicked
    orientation <- accumB OrientationVertical (reorient <$ rotate)
    sink (paned gui) [#orientation :== orientation]
    sink (rotateImage gui) [#iconName :== reorientIcon <$> orientation]
    
    -- retry for database
    retry <- signalE0 (retryDbButton gui) #clicked
    reactimate $ retry $> checkDB gui

-- | Reorient from portrait to landscape and vice versa
reorient :: Orientation -> Orientation
reorient OrientationVertical = OrientationHorizontal
reorient OrientationHorizontal = OrientationVertical
reorient _ = OrientationVertical

-- | Obtain reorientation icon. For portrait this will show a landscape icon
-- and vice versa
reorientIcon :: Orientation -> Text
reorientIcon OrientationVertical = "orientation-landscape-symbolic"
reorientIcon _ = "orientation-portrait-symbolic"
