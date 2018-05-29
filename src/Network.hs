{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Network (
    bootstrap,
    network
) where

import Control.Monad
import Data.Text (Text, pack, unpack)
import Data.Traversable
import Data.Functor.Syntax
import Data.Functor.Compose
import Control.Monad.IO.Class
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.GI.Gtk
import GI.Gtk

import UI
import Format
import Hoogle

-- | Flipped '<$>'
(<&>) :: Functor f => f a -> (a -> b) -> f b
a <&> f = f <$> a

-- | Flipped '<$'
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

bootstrap :: Application -> MomentIO HoogleGTK
bootstrap app = do
    gui <- hoogleGTK
    activate <- signalE0 app #activate
    reactimate $ activate $> do
        set (window gui) [ #application := app ]
        widgetShowAll (window gui)
    pure gui

-- | Debugging function to print behaviours
printB :: Show a => Behavior a -> MomentIO ()
printB b = do
    e <- changes b
    reactimate' $ print <$$> e

searchText :: SearchEntry -> MomentIO (Event Text)
searchText entry =
    mapEventIO (\_ -> get entry #text) =<< signalE0 entry #searchChanged

selectedRow :: ListBox -> MomentIO (Behavior (Maybe Int))
selectedRow listBox = do
    selected <- signalE1 listBox #rowSelected
    indexE <- mapEventIO (flip for listBoxRowGetIndex) selected
    stepper Nothing $ fromIntegral <$$> indexE

network :: HoogleGTK -> MomentIO ()
network gui
    -- search targets
 = do
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
        selectedDisplay = displayTarget <$> selectedTarget

    sink (typeLabel gui) [ #label :== dtargetType <$> selectedDisplay ]
    sink (moduleLabel gui) [ #label :== dtargetModule <$> selectedDisplay ]
    sink (packageLabel gui) [ #label :== dtargetPackage <$> selectedDisplay ]
    sink (docsLabel gui) [ #label :== dtargetDocs <$> selectedDisplay ]
