{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections, ViewPatterns #-}

module Npaperbot (writeNpaperbotFiles) where

import Prelude hiding ((++), (.), writeFile)
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import Render (outputDir, parentLink)
import IrcRender (ircRender)
import SlackRender (slackRender)
import Document
import Util ((++), (.), Text, writeFile)

maybeEntry :: Int -> Text -> Maybe Text
maybeEntry maxLength x
    | Text.length x > maxLength = Nothing
    | "FAILFAILFAIL" `Text.isInfixOf` x = Nothing
    | otherwise = Just $ Text.strip $ Text.pack $ collapseSpaces $ Text.unpack $ Text.replace "\t" " " x

collapseSpaces :: String -> String
collapseSpaces [] = []
collapseSpaces (' ' : x) = ' ' : collapseSpaces (dropWhile (== ' ') x)
collapseSpaces (c : x) = c : collapseSpaces x

ircSentenceEntry :: Sentence -> Maybe Text
ircSentenceEntry = maybeEntry 200 . ircRender

slackSentenceEntry :: Sentence -> Maybe Text
slackSentenceEntry = maybeEntry 1000 . slackRender

data Entry = Entry
    { key :: Text
    , ircMsg :: Maybe Text
    , slackMsg :: Maybe Text }

sentenceEntry :: Section -> Paragraph -> Sentence -> Maybe Entry
sentenceEntry page Paragraph{..} s@Sentence{..}
  | sentenceNumber == Nothing = Nothing
  | paraNumber == Nothing = Nothing
  | Nothing <- ircSentenceEntry s, Nothing <- slackSentenceEntry s = Nothing
  | otherwise = Just Entry {
      key = "http://eel.is/c++draft/" ++ abbreviation page
          ++ "#" ++ (if paraSection == page then "" else parentLink page (abbreviation paraSection) ++ "-") ++ Text.pack (show $ fromJust paraNumber)
          ++ ".sentence-" ++ Text.pack (show $ fromJust sentenceNumber),
      ircMsg = ircSentenceEntry s,
      slackMsg = slackSentenceEntry s }

paraEntries :: Paragraph -> [Entry]
paraEntries p@Paragraph{..} =
    [e | sent <- paraElems >>= sentences
       , section <- paraSection : parents paraSection
       , Just e <- [sentenceEntry section p sent]]

npaperbotEntries :: Draft -> [Entry]
npaperbotEntries = (>>= paraEntries) . Document.allParagraphs

makeJsonLiteral :: Text -> Text
makeJsonLiteral x = "\"" ++ Text.replace "\n" "\\n" (Text.replace "\"" "\\\"" (Text.replace "\\" "\\\\" x)) ++ "\"" -- todo

writeEntries :: [Entry] -> IO ()
writeEntries entries = writeFile (outputDir ++ "npaperbot-cxxdraft-entries.json") $ "{" ++ Text.intercalate ",\n\n" (map renderEntry entries) ++ "}\n"
	where
	    renderEntry :: Entry -> Text
	    renderEntry Entry{..} =
	        makeJsonLiteral key ++ ":{\n  " ++ Text.intercalate ",\n  "
	            (
	               [makeJsonLiteral "ircMsg" ++ ":   " ++ makeJsonLiteral txt | Just txt <- [ircMsg]] ++
	               [makeJsonLiteral "slackMsg" ++ ": " ++ makeJsonLiteral txt | Just txt <- [slackMsg]]
	            ) ++ "}"

writeNpaperbotFiles :: Draft -> IO ()
writeNpaperbotFiles draft = do
	putStrLn "  npaperbot files"
	writeEntries $ npaperbotEntries draft
	writeFile (outputDir ++ "sections.txt") $ Text.unlines $ abbreviation . sections draft
