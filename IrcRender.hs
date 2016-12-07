{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE
	OverloadedStrings,
	RecordWildCards,
	TupleSections,
	ViewPatterns,
	LambdaCase,
	TypeSynonymInstances,
	FlexibleInstances #-}

module IrcRender (
	IrcRender(ircRender)
	) where

import Document (
	CellSpan(..), Cell(..), RowSepKind(..), Row(..), Element(..), Draft(..), Footnote(..),
	TeXPara(..), Sentence(..), Abbreviation,
	Section(..), Table(..), Figure(..), Sections(..), figures, tables, Item(..),
	IndexComponent(..), IndexTree, IndexNode(..), IndexKind(..), IndexEntry(..),
	IndexPath, indexKeyContent, tableByAbbr, figureByAbbr, Paragraph(..), Note(..), Example(..))
import LaTeXBase (LaTeX, LaTeXUnit(..), ArgKind(..))
import qualified Data.Text.Lazy.Builder as TextBuilder
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Data.Char (isSpace)
import qualified Prelude
import Prelude hiding (take, (.), (++), writeFile)
import Data.List (intersperse)
import Util ((.), (++), replace, Text, greekAlphabet)
import Render (squareAbbr)

kill :: [String]
kill = words $
	"clearpage renewcommand newcommand enlargethispage noindent indent vfill pagebreak setlength " ++
	"caption capsep continuedcaption bottomline hline rowsep hspace endlist cline itcorr " ++
	"hfill nocorr small endhead kill footnotesize rmfamily microtypesetup nobreak nolinebreak " ++
	"label topline FlushAndPrintGrammar left right protect = ! @ - xspace obeyspaces " ++
	"index footnoteref discretionary footnotesize rmfamily itshape small left right break bigl bigr nobreak raggedright big"

simpleMacros :: [(String, Text)]
simpleMacros =
	[ ("dcr"            , "--")
	, (","              , " ")
	, ("\""             , "\"")
	, ("prime"          , "'")
	, ("caret"          , "^")
	, ("copyright"      , "©")
	, ("textregistered" , "®")
	, ("Cpp"            , "C++")
	, ("sum"            , "∑")
	, ("ell"            , "ℓ")
	, ("shr"            , ">>")
	, ("cv"             , "cv")
	, ("shl"            , "<<")
	, ("br"             , " ")
	, ("linebreak"      , " ")
	, ("sim"            , "~")
	, ("quad"           , "  ")
	, ("indent"         , "  ")
	, ("unun"           , "__")
	, ("^"              , "^")
	, ("ldots"          , "...")
	, ("vdots"          , "...")
	, ("dotsc"          , "...")
	, ("dotsb"          , "...")
	, ("times"          , "×")
	, ("&"              , "&")
	, ("$"              , "$")
	, ("backslash"      , "\\")
	, ("textbackslash"  , "\\")
	, ("textunderscore" , "_")
	, ("colcol"         , "::")
	, ("tilde"          , "~")
	, ("hspace"         , " ")
	, ("space"          , " ")
	, ("equiv"          , "&equiv;") -- todo
	, ("le"             , "≤")
	, ("leq"            , "≤")
	, ("ge"             , "≥")
	, ("geq"            , "≥")
	, ("neq"            , "≠")
	, ("land"           , "∧")
	, ("lor"            , "∨")
	, ("cdot"           , "·")
	, ("cdots"          , "⋯")
	, ("to"             , "→")
	, ("rightarrow"     , "→")
	, ("mapsto"         , "↦")
	, ("sqrt"           , "√")
	, ("lfloor"         , "⌊")
	, ("rfloor"         , "⌋")
	, ("int"            , "FAILFAILFAIL") -- can't do integrals in irc
	, ("sin"            , "sin")
	, ("cos"            , "cos")
	, ("lceil"          , "⌈")
	, ("rceil"          , "⌉")
	, (";"              , " ")
	, ("min"            , "min")
	, ("max"            , "max")
	, ("bmod"           , "mod")
	, ("exp"            , "exp")
	, ("ln"             , "ln")
	, ("log"            , "log")
	, ("opt"            , "<sub><small>opt</small></sub>")
	, ("rightshift"     , "rshift")
	, ("textlangle"     , "<")
	, ("textrangle"     , ">")
	, ("textmu"         , "μ")
	, ("tablerefname"   , "Table")
	, ("figurerefname"  , "Figure")
	, ("newline"        , "\n")
	, ("infty"          , "∞")
	, (">"              , "&#9;")
	, ("bnfindent"      , "&#9;")
	, ("\n"             , "\n")
	, (":"              , "") -- todo: wtf is this
	]
	++ [(n, Text.pack [c]) | (n, c) <- greekAlphabet]

simpleFlatMacros :: String -> Maybe Text
simpleFlatMacros = override (flip lookup simpleMacros) (flip lookup flatMacros)
	where
		flatMacros :: [(String, Text)]
		flatMacros =
			[ (",", " ")
			, ("space", " ")
			, ("{", "{")
			, ("}", "}")
			, ("%", "%")
			, ("#", "#")
			, ("~", "~")
			]

override :: (a -> Maybe b) -> (a -> Maybe b) -> (a -> Maybe b)
override f g x -- todo: rmeove
	| Just y <- g x = Just y
	| otherwise = f x

trimString :: String -> String
trimString = reverse . dropWhile isSpace . reverse . dropWhile isSpace

class IrcRender a where ircRender :: a -> Text

instance IrcRender TeXPara where
	ircRender = mconcat . intersperse " " . map ircRender . sentences

instance IrcRender Sentence where
	ircRender = mconcat . map ircRender . sentenceElems

instance IrcRender [TeXPara] where
	ircRender = ircRender . head

instance IrcRender LaTeX where
	ircRender = mconcat . map ircRender

instance IrcRender LaTeXUnit where
	ircRender TeXLineBreak = ""
	ircRender (TeXRaw x  ) = replace "--" "–"
	                      $ replace "---" "—"
	                      $ replace "~" " "
	                      $ replace "\n" " "
	                      $ x
	ircRender (TeXComm "texttt" [(_, x)]) = ircRender x
	ircRender (TeXBraces t              ) = ircRender t
	ircRender (TeXMath _ t         ) = ircRender t
	ircRender (TeXComm "term" [(FixArg, t)]) = ircRender t
	ircRender (TeXComm "grammarterm" [(_, t)]) = ircRender t
	ircRender (TeXComm "grammarterm_" [_, (_, [TeXRaw name])]) = name
	ircRender (TeXComm "grammarterm" [_, (_, [TeXRaw name])]) = name
	ircRender (TeXComm "phantom" _) = ""
	ircRender (TeXComm "relax" _) = ""
	ircRender (TeXComm "nopnumdiffref" _) = "FAILFAILFAIL"
	ircRender (TeXComm "mathbin" [(FixArg, t)]) = ircRender t
	ircRender (TeXComm "mathrel" [(FixArg, t)]) = ircRender t
	ircRender (TeXComm "mathrm" [(_, x)]) = ircRender x
	ircRender (TeXComm "emph" [(_, x)]) = ircRender x
	ircRender (TeXComm "mathit" [(_, x)]) = ircRender x
	ircRender (TeXComm "mathsf" [(_, x)]) = ircRender x
	ircRender (TeXComm "mathscr" [(_, x)]) = ircRender x
	ircRender (TeXComm "text" [(_, x)]) = ircRender x
	ircRender (TeXComm "tcode" [(_, x)]) = ircRender x
	ircRender (TeXComm "noncxxtcode" [(_, x)]) = ircRender x
	ircRender (TeXComm "mathtt" [(_, x)]) = ircRender x
	ircRender (TeXComm "ensuremath" [(_, x)]) = ircRender x
	ircRender (TeXComm "textit" [(_, x)]) = ircRender x
	ircRender (TeXComm "mbox" x) = mconcat $ map (ircRender . snd) x
	ircRender (TeXComm "verb" [(_, x)]) = ircRender x
	ircRender (TeXComm "textsc" [(_, x)]) = ircRender x
	ircRender (TeXComm "textrm" [(_, x)]) = ircRender x
	ircRender (TeXComm "textbf" [(_, x)]) = ircRender x
	ircRender (TeXComm "fref" [(FixArg, [TeXRaw abbr])]) = "Figure [fig:" ++ abbr ++ "]"
	ircRender (TeXComm "deflink" ((FixArg, x) : _)) = ircRender x
	ircRender (TeXComm "deflinkx" ((FixArg, x) : _)) = ircRender x
	ircRender (TeXComm "liblinkx" ((FixArg, x) : _)) = ircRender x
	ircRender (TeXComm "weblink" ((FixArg, x) : _)) = ircRender x
	ircRender (TeXComm "indexedspan" ((FixArg, x) : _)) = ircRender x
	ircRender (TeXComm "textsuperscript" ((FixArg, x) : _)) = ircRender x
	ircRender (TeXComm "frac" [(FixArg, x), (FixArg, y)]) = ircRender x ++ "/" ++ ircRender y
	ircRender (TeXComm "link" ((FixArg, x) : _)) = ircRender x
	ircRender (TeXComm "linkx" ((FixArg, x) : _)) = ircRender x
	ircRender (TeXComm "bigoh" [(_, x)]) = "Ο(" ++ ircRender x ++ ")"
	ircRender (TeXComm "textsl" [(_, x)]) = ircRender x
	ircRender (TeXComm "ref" [(FixArg, [TeXRaw abbr])]) = LazyText.toStrict $ TextBuilder.toLazyText $ squareAbbr abbr
	ircRender (TeXComm "binom" _) = "FAILFAILFAIL"
	ircRender (TeXComm "defnx" (_ : (FixArg, x) : _)) = ircRender x
	ircRender (TeXComm "\n" []) = " "
	ircRender (TeXComm " " []) = " "
	ircRender (TeXComm x s)
	    | trimString x `elem` kill                = ""
	    | null s, Just y <-
	       simpleFlatMacros (trimString x)       = y
	    | [(FixArg, z)] <- s
	    , Just y <- simpleFlatMacros x = y ++ ircRender z
	ircRender (TeXEnv "indexed" _ body) = ircRender body
	ircRender (TeXEnv "indented" _ body) = ircRender body
	ircRender (TeXEnv "array" _ _) = "FAILFAILFAIL"
	ircRender (TeXEnv "eqnarray*" _ _) = "FAILFAILFAIL"
	ircRender (TeXEnv "outputblock" _ _) = "FAILFAILFAIL"
	ircRender x = error $ "can't render: " ++ show x

instance IrcRender Element where
	ircRender (LatexElement t) = ircRender t
	ircRender (ExampleElement t) = "[Example: " ++ ircRender (exampleContent t) ++ " - end example]"
	ircRender (NoteElement t) = "[Note: " ++ ircRender (noteContent t) ++ " - end note]"
	ircRender (Enumerated _ _) = "TODO" -- todo
	ircRender _ = "FAILFAILFAIL"
