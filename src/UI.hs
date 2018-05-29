{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module UI where

import Control.Monad.IO.Class
import Data.Foldable
import Data.FileEmbed
import Data.Text (Text)
import GI.Gtk
import Reactive.Banana.GI.Gtk

data HoogleGTK = HoogleGTK
    { typeSearch :: !SearchEntry
    , browserButton :: !LinkButton
    , rotateButton :: !Button
    , rotateImage :: !Image
    , typeLabel :: !Label
    , packageLabel :: !Label
    , moduleLabel :: !Label
    , docsLabel :: !Label
    , resultsList :: !ListBox
    , window :: !ApplicationWindow
    , paned :: !Paned
    , noDatabase :: !Box
    , stack :: !Stack
    , retryDbButton :: !Button
    }

-- | Create a new Function Row given a type, a package, and a module
functionRow :: MonadIO m 
    => Text   -- ^ Type
    -> Text   -- ^ Package
    -> Text   -- ^ Module
    -> m ListBoxRow
functionRow ty pkg mdl = do
    b <- builderNewFromString $(embedStringFile "res/function-row.ui") (-1)
    tLabel <- castB b "typeLabel" Label
    locLabel <- castB b "locationLabel" Label
    set tLabel [#label := ty]
    set locLabel [#label := (pkg <> " – " <> mdl)]
    castB b "hoogleRow" ListBoxRow

-- | Describes composite rows (modules or packages)
data Composite
    = Module
    | Package

-- | Create a new composite row
compRow :: MonadIO m => Composite -> Text -> m ListBoxRow
compRow comp name = do
    b <- builderNewFromString $(embedStringFile "res/comp-row.ui") (-1)
    compLabel <- castB b "compLabel" Label
    nameLabel <- castB b "nameLabel" Label
    set
        compLabel
        [ #label :=
          case comp of
              Module -> "module"
              Package -> "package"
        ]
    set nameLabel [#label := name]
    castB b "hoogleRow" ListBoxRow

-- | Load GUI
hoogleGTK :: MonadIO m => m HoogleGTK
hoogleGTK = do
    b <- builderNewFromString $(embedStringFile "res/main-window.ui") (-1)
    HoogleGTK 
        <$> castB b "typeSearch" SearchEntry
        <*> castB b "browserButton" LinkButton
        <*> castB b "rotateButton" Button
        <*> castB b "rotateImage" Image
        <*> castB b "typeLabel" Label
        <*> castB b "packageLabel" Label
        <*> castB b "moduleLabel" Label
        <*> castB b "docsLabel" Label
        <*> castB b "resultsList" ListBox
        <*> castB b "hoogle-gtk" ApplicationWindow
        <*> castB b "paned" Paned
        <*> castB b "noDatabase" Box
        <*> castB b "stack" Stack
        <*> castB b "retryDbButton" Button

-- | (Re)load a 'ListBox' with some traversable of row building actions
loadListBox :: (Traversable t, MonadIO m) => ListBox -> t (m ListBoxRow) -> m ()
loadListBox list rowActions = do
    containerGetChildren list >>= mapM_ (containerRemove list)
    for_ rowActions $ \newRow -> do
        row <- newRow
        listBoxInsert list row (-1)
