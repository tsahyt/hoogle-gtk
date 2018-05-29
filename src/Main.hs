{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Exception
import Reactive.Banana.Frameworks
import GI.Gtk (GError, gerrorMessage)
import qualified GI.Gtk as Gtk
import qualified GI.Gio as Gio
import qualified Data.Text as T

import UI
import Network

runGtk :: IO ()
runGtk = void $ do
    Gtk.init Nothing
    Gtk.applicationNew Nothing [] >>= \case
        Nothing -> error "Error while creating GTK Application"
        Just app -> do
            compile (bootstrap app >>= network) >>= actuate
            Gio.applicationRun app Nothing

main :: IO ()
main = runGtk `catch` (\(e::GError) -> gerrorMessage e >>= putStrLn . T.unpack)
