{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception
import Reactive.Banana.Frameworks
import GI.Gtk (GError, gerrorMessage)
import qualified GI.Gtk as Gtk
import qualified Data.Text as T

runGtk = do
    Gtk.init Nothing
    compile (pure ()) >>= actuate
    Gtk.main

main :: IO ()
main = runGtk `catch` (\(e::GError) -> gerrorMessage e >>= putStrLn . T.unpack)
