module UI where

import Control.Monad.IO.Class
import GI.Gtk
import Data.FileEmbed

data HoogleGTK = HoogleGTK
    { typeSearch :: !SearchEntry
    , browserButton :: !Button
    , packageLabel :: !Label
    , moduleLabel :: !Label
    , docsLabel :: !Label
    , resultsList :: !ListBox
    }

hoogleGTK :: MonadIO m => m HoogleGTK
hoogleGTK = undefined
