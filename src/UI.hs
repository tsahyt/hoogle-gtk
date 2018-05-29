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
    , browserButton :: !Button
    , typeLabel :: !Label
    , packageLabel :: !Label
    , moduleLabel :: !Label
    , docsLabel :: !Label
    , resultsList :: !ListBox
    , window :: !ApplicationWindow
    }

functionRow :: MonadIO m 
    => String   -- ^ Type
    -> String   -- ^ Package
    -> String   -- ^ Module
    -> m ListBoxRow
functionRow ty pkg mod = do
    b <- builderNewFromString $(embedStringFile "res/function-row.ui") (-1)
    typeLabel <- castB b "typeLabel" Label
    locationLabel <- castB b "locationLabel" Label
    set typeLabel [#label := pack ty]
    set locationLabel [#label := (pack pkg <> " – " <> pack mod)]
    castB b "hoogleRow" ListBoxRow

data Composite
    = Module
    | Package

compRow :: MonadIO m => Composite -> String -> m ListBoxRow
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
    set nameLabel [#label := pack name]
    castB b "hoogleRow" ListBoxRow

hoogleGTK :: MonadIO m => m HoogleGTK
hoogleGTK = do
    b <- builderNewFromString $(embedStringFile "res/main-window.ui") (-1)
    HoogleGTK 
        <$> castB b "typeSearch" SearchEntry
        <*> castB b "browserButton" Button
        <*> castB b "typeLabel" Label
        <*> castB b "packageLabel" Label
        <*> castB b "moduleLabel" Label
        <*> castB b "docsLabel" Label
        <*> castB b "resultsList" ListBox
        <*> castB b "hoogle-gtk" ApplicationWindow

loadListBox :: (Traversable t, MonadIO m) => ListBox -> t (m ListBoxRow) -> m ()
loadListBox list rowActions = do
    containerGetChildren list >>= mapM_ (containerRemove list)
    for_ rowActions $ \newRow -> do
        row <- newRow
        listBoxInsert list row (-1)
