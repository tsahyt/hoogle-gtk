{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module UI where

import Control.Monad.IO.Class
import Data.Foldable
import Data.FileEmbed
import Data.Text (Text, pack)
import GI.Gtk
import Reactive.Banana.GI.Gtk

data HoogleGTK = HoogleGTK
    { typeSearch :: !SearchEntry
    , browserButton :: !LinkButton
    , typeLabel :: !Label
    , packageLabel :: !Label
    , moduleLabel :: !Label
    , docsLabel :: !Label
    , resultsList :: !ListBox
    , window :: !ApplicationWindow
    , paned :: !Paned
    }

functionRow :: MonadIO m 
    => Text   -- ^ Type
    -> Text   -- ^ Package
    -> Text   -- ^ Module
    -> m ListBoxRow
functionRow ty pkg mod = do
    b <- builderNewFromString $(embedStringFile "res/function-row.ui") (-1)
    typeLabel <- castB b "typeLabel" Label
    locationLabel <- castB b "locationLabel" Label
    set typeLabel [#label := ty]
    set locationLabel [#label := (pkg <> " – " <> mod)]
    castB b "hoogleRow" ListBoxRow

data Composite
    = Module
    | Package

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

hoogleGTK :: MonadIO m => m HoogleGTK
hoogleGTK = do
    b <- builderNewFromString $(embedStringFile "res/main-window.ui") (-1)
    HoogleGTK 
        <$> castB b "typeSearch" SearchEntry
        <*> castB b "browserButton" LinkButton
        <*> castB b "typeLabel" Label
        <*> castB b "packageLabel" Label
        <*> castB b "moduleLabel" Label
        <*> castB b "docsLabel" Label
        <*> castB b "resultsList" ListBox
        <*> castB b "hoogle-gtk" ApplicationWindow
        <*> castB b "paned" Paned

loadListBox :: (Traversable t, MonadIO m) => ListBox -> t (m ListBoxRow) -> m ()
loadListBox list rowActions = do
    containerGetChildren list >>= mapM_ (containerRemove list)
    for_ rowActions $ \newRow -> do
        row <- newRow
        listBoxInsert list row (-1)
