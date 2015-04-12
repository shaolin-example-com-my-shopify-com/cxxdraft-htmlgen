{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Toc (tocFileContent) where

import qualified Data.Text as Text
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import System.IO.Unsafe (unsafePerformIO)
import Data.Time.Clock (getCurrentTime)
import Prelude hiding ((.), (++))
import Render
import Util
import Load14882

tocHeader :: Text -> Text
tocHeader commitUrl =
	xml "div" [("class", "tocHeader")] (
		"Generated on " ++ date ++
		" from the C++ standard's <a href='" ++ commitUrl ++ "'>draft LaTeX sources</a>" ++
		" by <a href='https://github.com/Eelis/cxxdraft-htmlgen'>cxxdraft-htmlgen</a>." ++
		"<hr/>") ++
	"<h1>Contents</h1>"
	where
		date = Text.pack $ formatTime defaultTimeLocale "%F" $ unsafePerformIO getCurrentTime

tocSection :: (SectionPath, Section) -> Text
tocSection (sectionPath, Section{..}) =
	xml "div" [("id", render abbreviation)] $
	h Nothing (min 4 $ 1 + length (sectionNums sectionPath)) (
		secnum "" sectionPath ++ " " ++
		render (sectionName ++ " ", (linkToSection TocToSection abbreviation){aClass="abbr_ref"})) ++
	mconcat (tocSection . numberSubsecs sectionPath subsections)

tocChapter :: (SectionPath, Section) -> Text
tocChapter (sectionPath, Section{..}) =
	xml "div" [("id", render abbreviation)] $
	h Nothing (min 4 $ 1 + length (sectionNums sectionPath)) (
		secnum "" sectionPath ++ " " ++
		render (sectionName ++ " ", anchor{
			aClass = "folded_abbr_ref",
			aText  = "[" ++ render abbreviation ++ "]",
			aHref  = "#" ++ render abbreviation}) ++
		render (linkToSection TocToSection abbreviation){aClass="unfolded_abbr_ref"}) ++
	xml "div" [("class", "tocChapter")] (mconcat (tocSection . numberSubsecs sectionPath subsections))

tocListOfTables :: [(LaTeX, Table)] -> Text
tocListOfTables tables =
	xml "div" [("id", "tables")] $
		"<h2><a href='#tables'>List of Tables</a></h2>"
		++ xml "div" [("class", "tocChapter")] (mconcat (tableItem . tables))
	where
		tableItem :: (LaTeX, Table) -> Text
		tableItem (section, Table{..}) =
			spanTag "secnum" (render tableNumber)
			++ render tableCaption
			++ render anchor{
				aHref  = "TocToSection/" ++ url section
				         ++ "#" ++ replace ":" "-" (render $ head tableAbbrs),
				aText  = "[" ++ render (head tableAbbrs) ++ "]",
				aClass = "abbr_ref"}
			++ "<br>"

tocListOfFigures :: [(LaTeX, Figure)] -> Text
tocListOfFigures figures =
	xml "div" [("id", "figures")] $
		"<h2><a href='#figures'>List of Figures</a></h2>"
		++ xml "div" [("class", "tocChapter")] (mconcat (figureItem . figures))
	where
		figureItem :: (LaTeX, Figure) -> Text
		figureItem (section, Figure{..}) =
			spanTag "secnum" (render figureNumber)
			++ render figureName
			++ render anchor{
				aHref  = "TocToSection/" ++ url section
				         ++ "#" ++ replace ":" "-" (render figureAbbr),
				aText  = "[" ++ render figureAbbr ++ "]",
				aClass = "abbr_ref"}
			++ "<br>"

tocFileContent :: SectionFileStyle -> Draft -> Text
tocFileContent sfs Draft{..} =
		applySectionFileStyle sfs $ fileContent "" "14882: Contents" $
			tocHeader commitUrl ++
			tocListOfTables tables ++
			tocListOfFigures figures ++
			mconcat (tocChapter . withPaths chapters)
