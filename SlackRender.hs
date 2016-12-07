{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE
	OverloadedStrings,
	RecordWildCards,
	TupleSections,
	ViewPatterns,
	LambdaCase,
	TypeSynonymInstances,
	FlexibleInstances #-}

module SlackRender (SlackRender(slackRender)) where

import Document (Element(..), TeXPara(..), Sentence(..), Item(..), Note(..), Example(..))
import LaTeXBase (LaTeX, LaTeXUnit(..), ArgKind(..))
import qualified Data.Text.Lazy.Builder as TextBuilder
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Data.Char (isSpace)
import Prelude hiding (take, (.), (++), writeFile)
import Data.List (intersperse)
import Util ((.), (++), replace, Text, greekAlphabet)
import Render (squareAbbr)

kill :: [String]
kill = words $
	"clearpage renewcommand newcommand enlargethispage noindent indent vfill pagebreak setlength " ++
	"caption capsep continuedcaption bottomline hline rowsep hspace endlist cline itcorr " ++
	"hfill nocorr small endhead kill footnotesize rmfamily microtypesetup nobreak nolinebreak " ++
	"label topline FlushAndPrintGrammar left right protect = ! @ - xspace obeyspaces index " ++
	"footnoteref discretionary footnotesize rmfamily itshape small left right break bigl bigr nobreak raggedright big"

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
	, ("equiv"          , "&equiv;")
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
	, ("int"            , "FAILFAILFAIL") -- can't do integrals in slack
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
	, ("opt"            , "_opt")
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
override f g x
	| Just y <- g x = Just y
	| otherwise = f x

trimString :: String -> String
trimString = reverse . dropWhile isSpace . reverse . dropWhile isSpace

class SlackRender a where slackRender :: a -> Text

instance SlackRender TeXPara where
	slackRender = mconcat . intersperse " " . map slackRender . sentences

instance SlackRender Sentence where
	slackRender = mconcat . map slackRender . sentenceElems

instance SlackRender [TeXPara] where
	slackRender = slackRender . head

instance SlackRender LaTeX where
	slackRender = mconcat . map slackRender

instance SlackRender LaTeXUnit where
	slackRender TeXLineBreak = ""
	slackRender (TeXRaw x  ) = replace "--" "–"
	                      $ replace "---" "—"
	                      $ replace "~" " "
	                      $ replace "\n" " "
	                      $ x
	slackRender (TeXComm "texttt" [(_, x)]) = slackRender x
	slackRender (TeXBraces t              ) = slackRender t
	slackRender (TeXMath _ t         ) = slackRender t
	slackRender (TeXComm "term" [(FixArg, t)]) = slackRender t
	slackRender (TeXComm "url" [(FixArg, t)]) = slackRender t
	slackRender (TeXComm "phantom" _) = ""
	slackRender (TeXComm "relax" _) = ""
	slackRender (TeXComm "nopnumdiffref" _) = "FAILFAILFAIL"
	slackRender (TeXComm "mathbin" [(FixArg, t)]) = slackRender t
	slackRender (TeXComm "mathrel" [(FixArg, t)]) = slackRender t
	slackRender (TeXComm "grammarterm" [(_, t)]) = "_"++ slackRender t ++ "_"
	slackRender (TeXComm "grammarterm_" [_, (_, [TeXRaw name])]) = "_" ++ name ++ "_"
	slackRender (TeXComm "grammarterm" [_, (_, [TeXRaw name])]) = "_" ++ name ++ "_"
	slackRender (TeXComm "mathrm" [(_, x)]) = slackRender x
	slackRender (TeXComm "emph" [(_, x)]) = slackRender x
	slackRender (TeXComm "mathit" [(_, x)]) = "_" ++ slackRender x ++ "_"
	slackRender (TeXComm "mathsf" [(_, x)]) = slackRender x
	slackRender (TeXComm "mathscr" [(_, x)]) = slackRender x
	slackRender (TeXComm "text" [(_, x)]) = slackRender x
	slackRender (TeXComm "tcode" [(_, x)]) = "`" ++ slackRender x ++ "`"
	slackRender (TeXComm "noncxxtcode" [(_, x)]) = slackRender x
	slackRender (TeXComm "mathtt" [(_, x)]) = slackRender x
	slackRender (TeXComm "ensuremath" [(_, x)]) = slackRender x
	slackRender (TeXComm "textit" [(_, x)]) = "_" ++ slackRender x ++ "_"
	slackRender (TeXComm "mbox" x) = mconcat $ map (slackRender . snd) x
	slackRender (TeXComm "verb" [(_, x)]) = slackRender x
	slackRender (TeXComm "textsc" [(_, x)]) = slackRender x
	slackRender (TeXComm "textrm" [(_, x)]) = slackRender x
	slackRender (TeXComm "textbf" [(_, x)]) = "*" ++ slackRender x ++ "*"
	slackRender (TeXComm "fref" [(FixArg, [TeXRaw abbr])]) = "Figure [fig:" ++ abbr ++ "]"
	slackRender (TeXComm "deflink" ((FixArg, x) : _)) = slackRender x
	slackRender (TeXComm "deflinkx" ((FixArg, x) : _)) = slackRender x
	slackRender (TeXComm "liblinkx" ((FixArg, x) : _)) = slackRender x
	slackRender (TeXComm "weblink" ((FixArg, x) : _)) = slackRender x
	slackRender (TeXComm "indexedspan" ((FixArg, x) : _)) = slackRender x
	slackRender (TeXComm "textsuperscript" ((FixArg, x) : _)) = slackRender x
	slackRender (TeXComm "frac" [(FixArg, x), (FixArg, y)]) = slackRender x ++ "/" ++ slackRender y
	slackRender (TeXComm "link" ((FixArg, x) : _)) = slackRender x
	slackRender (TeXComm "linkx" ((FixArg, x) : _)) = slackRender x
	slackRender (TeXComm "bigoh" [(_, x)]) = "Ο(" ++ slackRender x ++ ")"
	slackRender (TeXComm "textsl" [(_, x)]) = slackRender x
	slackRender (TeXComm "ref" [(FixArg, [TeXRaw abbr])]) = LazyText.toStrict $ TextBuilder.toLazyText $ squareAbbr abbr
	slackRender (TeXComm "defnx" (_ : (FixArg, x) : _)) = slackRender x
	slackRender (TeXComm "\n" []) = " "
	slackRender (TeXComm " " []) = " "
	slackRender (TeXComm "binom" _) = "FAILFAILFAIL"
	slackRender (TeXComm x s)
	    | trimString x `elem` kill                = ""
	    | null s, Just y <-
	       simpleFlatMacros (trimString x)       = y
	    | [(FixArg, z)] <- s
	    , Just y <- simpleFlatMacros x = y ++ slackRender z
	slackRender (TeXEnv "indexed" _ body) = slackRender body
	slackRender (TeXEnv "indented" _ body) = slackRender body
	slackRender (TeXEnv "array" _ _) = "FAILFAILFAIL"
	slackRender (TeXEnv "eqnarray*" _ _) = "FAILFAILFAIL"
	slackRender (TeXEnv "outputblock" [] x) = "```\n" ++ Text.concat (map raw x) ++ "\n```\n"
	slackRender (TeXComm "terminal" [(FixArg, x)]) = "`" ++ slackRender x ++ "`"
	slackRender (TeXComm "nontermdef" _) = "FAILFAILFAIL"
	slackRender x = error $ "can't render: " ++ show x

instance SlackRender Item where
    slackRender Item{..} = "- " ++ slackRender itemContent ++ "\n"

-- todo: bug in slack: 3<`b` works fine, but 3≤`b` does not work.
-- todo: plural grammarterm leads to: _c-char__s_. should be rewritten to _c-chars_

raw :: LaTeXUnit -> Text
raw (TeXRaw x) = x
raw (TeXBraces x) = Text.concat $ map raw x
raw (TeXMath _ x) = Text.concat $ map raw x
raw (TeXComm c [(FixArg, x)])
    | c `elem` ["textrm", "textit", "texttt", "mathit", "mathtt", "ensuremath", "grammarterm", "tcode", "comment"] = Text.concat $ map raw x
raw (TeXComm "textbackslash" []) = "\\"
raw (TeXComm "ref" [(FixArg, [TeXRaw abbr])]) = "[" ++ abbr ++ "]"
raw (TeXComm "dotsc" []) = "..."
raw (TeXComm "to" []) = "→"
raw (TeXComm "&" []) = "&"
raw (TeXComm "commentellip" []) = "/* ... */"
raw (TeXComm "!" []) = ""
raw (TeXComm "#" []) = "&"
raw (TeXComm "index" _) = ""
raw (TeXComm "cdot " []) = "·"
raw (TeXComm "ge " []) = ">= "
raw (TeXComm "neq " []) = "!= "
raw (TeXComm "bmod " []) = "bmod "
raw x = error $ "raw: " ++ show x

instance SlackRender Element where
	slackRender (LatexElement t) = slackRender t
	slackRender (ExampleElement t) = "[Example: " ++ slackRender (exampleContent t) ++ " - end example]"
	slackRender (NoteElement t) = "[Note: " ++ slackRender (noteContent t) ++ " - end note]"
	slackRender (Enumerated _ items) = "\n" ++ Text.concat (map slackRender items)
	slackRender (TableElement _) = "<table>"
	slackRender (Codeblock (TeXEnv "codeblock" _ x)) = "\n```\n" ++ Text.concat (map raw x) ++ "\n```\n"
	slackRender (Bnf _ x) = slackRender x
	slackRender x = error (show x)
