{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}

module Format
    ( targetRow
    , DisplayTarget(..)
    , emptyDisplayTarget
    , displayTarget
    , tagged
    ) where

import Control.Monad.IO.Class
import Data.Maybe
import Data.Text (Text, pack)
import qualified Data.Text as T
import Text.HTML.TagSoup

import GI.Gtk (ListBoxRow)

import Hoogle
import UI

-- | Build a suitable row from a Hoogle 'Target'
targetRow :: MonadIO m => Target -> m ListBoxRow
targetRow t
    | Nothing <- targetPackage t
    , Nothing <- targetModule t = compRow Package (formatComp $ targetItem t)
    | Nothing <- targetModule t
    , Just _ <- targetPackage t = compRow Module (formatComp $ targetItem t)
    | Just (pkg, _) <- targetPackage t
    , Just (mdl, _) <- targetModule t =
        functionRow (formatItem True . targetItem $ t) (pack pkg) (pack mdl)
    | otherwise = functionRow "Invalid Hoogle Result!" "" ""

-- | Formatted 'Target'
data DisplayTarget = DisplayTarget
    { dtargetType :: Text
    , dtargetModule :: Text
    , dtargetPackage :: Text
    , dtargetDocs :: Text
    }

emptyDisplayTarget :: DisplayTarget
emptyDisplayTarget = DisplayTarget "" "" "" ""

-- | Format a 'Target'
displayTarget :: Target -> DisplayTarget
displayTarget t =
    DisplayTarget
    { dtargetType = formatItem False $ targetItem t
    , dtargetModule = formatLink $ targetModule t
    , dtargetPackage = formatLink $ targetPackage t
    , dtargetDocs = formatDocs $ targetDocs t
    }

formatLink :: Maybe (String, URL) -> Text
formatLink Nothing = ""
formatLink (Just (name, url)) =
    "<a href='" <> pack url <> "'>" <> pack name <> "</a>"

tagged :: String -> [Text]
tagged = map pack . mapMaybe (maybeTagText) . parseTags

formatComp :: String -> Text
formatComp x =
    case tagged x of
        ["module", prefix, name] -> T.drop 1 prefix <> T.drop 3 name
        ["package", " ", name] -> T.drop 3 name
        _ -> pack x

formatItem :: Bool -> String -> Text
formatItem pretty x =
    case tagged x of
        [name, sig] ->
            T.drop 3 name <>
            if pretty
                then prettySig sig
                else sig
        [op, ")", sig]
            | pretty -> escapeHTML ("(" <> T.drop 4 op <> ")") <> prettySig sig
            | otherwise -> "(" <> T.drop 4 op <> ")" <> sig
        "newtype":" ":name:args
            | pretty ->
                prettySig $
                "<span font_weight='semibold'>newtype</span> " <> T.drop 3 name <>
                mconcat args
            | otherwise -> "newtype " <> T.drop 3 name <> mconcat args
        "data":" ":name:args
            | pretty ->
                prettySig $
                "<span font_weight='semibold'>data</span> " <> T.drop 3 name <>
                mconcat args
            | otherwise -> "data " <> T.drop 3 name <> mconcat args
        "type":" ":name:args
            | pretty ->
                prettySig $
                "<span font_weight='semibold'>type</span> " <> T.drop 3 name <>
                mconcat args
            | otherwise -> "type " <> T.drop 3 name <> mconcat args
        "class":context:clName:clRest
            | pretty ->
                prettySig $
                "<span font_weight='semibold'>class</span>" <> context <>
                T.drop 3 clName <>
                mconcat clRest
            | otherwise ->
                "class" <> context <> T.drop 3 clName <> mconcat clRest
        _ -> ""

prettySig :: Text -> Text
prettySig = T.replace "=>" "⇒" . T.replace "->" "→" . T.replace "<->" "↔"

formatDocs :: String -> Text
formatDocs = renderTags . mapMaybe go . parseTags . pack
  where
    go (TagOpen t _) =
        case t of
            "pre" ->
                Just $ TagOpen "span" [("face", "monospace"), ("size", "small")]
            "tt" -> Just $ TagOpen "span" [("face", "monospace")]
            "a" -> Just $ TagOpen "i" []
            _ -> Nothing
    go (TagClose t) =
        case t of
            "pre" -> Just $ TagClose "span"
            "tt" -> Just $ TagClose "span"
            "a" -> Just $ TagClose "i"
            _ -> Nothing
    go (TagText t) = Just $ TagText t
    go _ = Nothing
