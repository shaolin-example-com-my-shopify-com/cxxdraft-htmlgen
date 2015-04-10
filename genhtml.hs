{-# LANGUAGE
	OverloadedStrings,
	RecordWildCards,
	TupleSections,
	ViewPatterns,
	LambdaCase #-}

import Load14882 (CellSpan(..), Cell(..), RowSepKind(..), Row(..), Element(..), Paragraph, ChapterKind(..), Section(..), Chapter, Draft(..), load14882)

import Text.LaTeX.Base.Syntax (LaTeX(..), TeXArg(..), MathType(..), matchCommand, matchEnv)
import qualified Text.LaTeX.Base.Render as TeXRender
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.IO (writeFile)
import Data.Char (isSpace, isAlpha)
import Data.Monoid (Monoid(mappend), mconcat)
import Control.Monad (forM_, when)
import qualified Prelude
import Prelude hiding (take, last, (.), (++), writeFile)
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcess)
import System.Directory (createDirectoryIfMissing, copyFile, setCurrentDirectory, getCurrentDirectory, doesFileExist)
import System.Environment (getArgs)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import Data.Hashable (hash)

(.) :: Functor f => (a -> b) -> (f a -> f b)
(.) = fmap

(++) :: Monoid a => a -> a -> a
(++) = mappend

xml :: Text -> [(Text, Text)] -> Text -> Text
xml t attrs = (("<" ++ t ++ " " ++ Text.unwords (map f attrs) ++ ">") ++) . (++ ("</" ++ t ++ ">"))
	where
		f (n, v) = n ++ "='" ++ v ++ "'"

spanTag :: Text -> Text -> Text
spanTag = xml "span" . (:[]) . ("class",)

rangeElem :: Char -> Text -> Text -> Char -> Text
rangeElem open from to close
	= spanTag "range" $ Text.pack [open] ++ from ++ ", " ++ to ++ Text.pack [close]

h :: Maybe Text -> Int -> Text -> Text
h mc = flip xml (maybe [] ((:[]) . ("class",)) mc) . ("h" ++) . Text.pack . show

kill, literal :: [String]
kill = ["indextext", "indexdefn", "indexlibrary", "indeximpldef", "printindex", "clearpage", "renewcommand", "brk", "newcommand", "footnotetext", "enlargethispage", "index", "noindent", "indent", "vfill", "pagebreak", "topline", "xspace", "!", "linebreak", "caption", "setcounter", "addtocounter", "capsep", "continuedcaption", "bottomline", "-", "hline", "rowsep", "hspace", "ttfamily", "endlist"]
literal = [" ", "#", "{", "}", "~", "%", ""]

texFromArg :: TeXArg -> LaTeX
texFromArg (FixArg t) = t
texFromArg (OptArg t) = t
texFromArg (SymArg t) = t
texFromArg _ = error "no"

simpleMacros :: [(String, Text)]
simpleMacros =
	[ ("dcr"            , "--")
	, (","              , "&nbsp;")
	, ("prime"          , "'")
	, ("atsign"         , "@")
	, ("copyright"      , "&copy;")
	, ("textregistered" , "&reg;")
	, ("Cpp"            , "C++")
	, ("cppver"         , "201402L")
	, ("alpha"          , "α")
	, ("beta"           , "β")
	, ("delta"          , "δ")
	, ("lambda"         , "λ")
	, ("mu"             , "μ")
	, ("pi"             , "π")
	, ("rho"            , "ρ")
	, ("sigma"          , "σ")
	, ("Gamma"          , "Γ")
	, ("sum"            , "∑")
	, ("ell"            , "ℓ")
	, ("shr"            , ">>")
	, ("cv"             , "cv")
	, ("shl"            , "&lt;&lt;")
	, ("br"             , "<br/>&emsp;")
	, ("sim"            , "~")
	, ("quad"           , "&emsp;&ensp;")
	, ("indent"         , "&emsp;")
	, ("unun"           , "__")
	, ("^"              , "^")
	, ("ldots"          , "&hellip;")
	, ("times"          , "&times;")
	, ("&"              , "&amp;")
	, ("$"              , "&#36;")
	, ("backslash"      , "\\")
	, ("textbackslash"  , "\\")
	, ("textunderscore" , "_")
	, ("colcol"         , "::")
	, ("tilde"          , "~")
	, ("hspace"         , " ")
	, ("equiv"          , "&equiv;")
	, ("le"             , "≤")
	, ("leq"            , "≤")
	, ("ge"             , "≥")
	, ("geq"            , "≥")
	, ("neq"            , "≠")
	, ("cdot"           , "·")
	, ("cdots"          , "⋯")
	, ("to"             , "→")
	, ("rightarrow"     , "→")
	, ("sqrt"           , "√")
	, ("lfloor"         , "⌊")
	, ("rfloor"         , "⌋")
	, ("lceil"          , "⌈")
	, ("rceil"          , "⌉")
	, (";"              , " ")
	, ("min"            , "<span class=\"mathrm\">min</span>")
	, ("max"            , "<span class=\"mathrm\">max</span>")
	, ("bmod"           , "<span class=\"mathrm\">mod</span>")
	, ("exp"            , "<span class=\"mathrm\">exp</span>")
	, ("ln"             , "<span class=\"mathrm\">ln</span>")
	, ("log"            , "<span class=\"mathrm\">log</span>")
	, ("opt"            , "<sub><small>opt</small></sub>")
	, ("rightshift"     , "<span class=\"mathsf\">rshift</span>")
	]

makeSpan, makeDiv, makeBnfTable, makeBnfPre, makeRowsep, makeCodeblock :: [String]
makeSpan = words "indented itemdescr minipage center"
makeDiv = words "defn definition cvqual tcode textit textnormal term emph grammarterm exitnote footnote terminal nonterminal mathit enternote exitnote enterexample exitexample indented paras ttfamily TableBase table tabular longtable"
makeBnfTable = words "bnfkeywordtab bnftab"
makeBnfPre = words "bnf simplebnf"
makeRowsep = words "hline"
makeCodeblock = words "codeblock codeblockdigitsep"

data Anchor = Anchor { aClass, aId, aHref, aText :: Text }

anchor :: Anchor
anchor = Anchor{aClass="", aId="", aHref="", aText=""}

instance Render Anchor where
	render Anchor{..} = xml "a" ([("class", aClass) | aClass /= "" ] ++
	                             [("href" , aHref ) | aHref  /= "" ] ++
	                             [("id"   , aId   ) | aId    /= "" ])
	                        aText


class Render a where render :: a -> Text

instance Render a => Render [a] where
	render = mconcat . map render

instance (Render a, Render b) => Render (a, b) where
	render (x, y) = render x ++ render y

instance Render LaTeX where
	render (TeXSeq (TeXCommS "textbackslash") y)
		| TeXSeq (TeXRaw s) rest <- y  = "\\" ++ render (TeXRaw $ unspace s) ++ render rest
		| TeXRaw s <- y                = "\\" ++ render (TeXRaw $ unspace s)
		where
			unspace s
				| Just suffix <- Text.stripPrefix " " s = suffix
				| otherwise = s
	render (TeXSeq (TeXCommS "itshape") x) = "<i>" ++ render x ++ "</i>"
	render (TeXSeq x y               ) = render x ++ render y
	render (TeXRaw x                 ) = Text.replace "~" " "
	                                   $ Text.replace "--" "–"
	                                   $ Text.replace "---" "—"
	                                   $ Text.replace ">" "&gt;"
	                                   $ Text.replace "<" "&lt;"
	                                   $ Text.replace "&" "&amp;"
	                                   $ x
	render (TeXComment _             ) = ""
	render (TeXCommS "br"            ) = "<br/>"
	render (TeXLineBreak _ _         ) = "<br/>"
	render (TeXCommS "break"         ) = "<br/>"
	render (TeXEmpty                 ) = ""
	render (TeXBraces t              ) = render t
	render m@(TeXMath _ _            ) = renderMath m
	render (TeXComm "ensuremath" [FixArg x]) = renderMath x
	render (TeXComm "ref" [FixArg x])  = render $ linkToSection SectionToSection x
	render (TeXComm "impldef" _) = "implementation-defined"
	render (TeXComm "impdefx" [FixArg _description_for_index]) = "implementation-defined"
	render (TeXComm "xname" [FixArg (TeXRaw s)]) = spanTag "texttt" $ "_<span class=\"ungap\"></span>_" ++ s
	render (TeXComm "mname" [FixArg (TeXRaw s)]) = spanTag "texttt" $ "_<span class=\"ungap\"></span>_" ++ s ++ "_<span class=\"ungap\"></span>_"
	render (TeXComm "nontermdef" [FixArg (TeXRaw s)]) = spanTag "nontermdef" s ++ ":"
	render (TeXComm "bigoh" [FixArg content]) =
		spanTag "math" $ spanTag "mathscr" "𝓞" ++ "(" ++ render content ++ ")"
	render (TeXComm "defnx" [FixArg a, FixArg _description_for_index]) = render a
	render (TeXComm "texttt" [FixArg x]) = "<code>" ++ render x ++ "</code>"
	render (TeXComm "textit" [FixArg x]) = "<i>" ++ render x ++ "</i>"
	render (TeXComm "textit" [FixArg x, OptArg y]) = "<i>" ++ render x ++ "</i>[" ++ render y ++ "]"
	render (TeXComm "textbf" [FixArg x]) = "<b>" ++ render x ++ "</b>"
	render (TeXComm "label" [FixArg (TeXRaw x)]) = render anchor{aId=x}
	render (TeXComm "multicolumn" [FixArg (TeXRaw n), _, FixArg content]) = xml "td" [("colspan", n)] $ render content
	render (TeXComm "leftshift" [FixArg content]) =
		spanTag "mathsf" "lshift" ++ xml "sub" [("class", "math")] (render content)
	render (TeXComm "state" [FixArg a, FixArg b]) =
		spanTag "tcode" (render a) ++ xml "sub" [("class", "math")] (render b)
	render (TeXComm "verb" [FixArg a]) = xml "code" [] $ renderVerb a
	render (TeXComm "footnoteref" [FixArg (TeXRaw n)]) = makeFootnoteRef n
	render (TeXComm x s)
	    | x `elem` kill                = ""
	    | null s, Just y <-
	       lookup x simpleMacros       = y
	    | [FixArg z] <- s, Just y <-
	       lookup x simpleMacros       = y ++ render z
	    | otherwise                    = spanTag (Text.pack x) (render (map texFromArg s))
	render (TeXCommS s)
	    | s `elem` literal             = Text.pack s
	    | Just x <-
	       lookup s simpleMacros       = x
	    | s `elem` kill                = ""
	    | otherwise                    = spanTag (Text.pack s) ""
	render (TeXEnv "itemdecl" [] t)    = spanTag "itemdecl"
	                                     $ Text.strip
	                                     $ Text.replace "@" "" $ render t
	render (TeXEnv "tabbing" [] t)     = renderTabbing t
	render env@(TeXEnv e _ t)
	    | e `elem` makeCodeblock       = spanTag "codeblock" $ Text.strip $ renderCode t
	    | e `elem` makeSpan            = spanTag (Text.pack e) (render t)
	    | e `elem` makeDiv             = xml "div" [("class", Text.pack e)] (render t)
	    | isComplexMath env            = renderComplexMath env
	    | otherwise                    = error "unexpected env"

instance Render Element where
	render (LatexElements t) = case render t of "" -> ""; x -> xml "p" [] x
	render (Bnf e t)
		| e `elem` makeBnfTable = renderBnfTable t
		| e `elem` makeBnfPre = bnfPre $ render $ preprocessPre t
		| otherwise = error "unexpected bnf"
	render Table{..} =
		spanTag "tabletitle" (render tableCaption)
		++ renderTable columnSpec tableBody
	render (Tabbing t) = renderTabbing t
	render Figure{..} =
		xml "div" [("class", "figure")] $ figureSvg ++ "<br>" ++ render figureName
	render (Enumerated ek ps) = xml t [] $ mconcat $ xml "li" [] . render . ps
		where
			t = case ek of
				"enumeraten" -> "ol"
				"enumeratea" -> "ol"
				"enumerate" -> "ol"
				"itemize" -> "ul"
				"description" -> "ul"
				_ -> undefined
	render (Footnote num content) =
		xml "div" [("class", "footnote")] $
		makeFootnoteRef (Text.pack $ show num) ++ render content

makeFootnoteRef :: Text -> Text
makeFootnoteRef n = xml "sup" [] $ spanTag "footnoteref" $ "&lang;" ++ n ++ "&rang;"

renderVerb :: LaTeX -> Text
renderVerb t@(TeXRaw _) = renderCode t
renderVerb (TeXBraces _) = ""
renderVerb other = render other

renderCode :: LaTeX -> Text
renderCode (TeXRaw s) =
	Text.replace "<" "&lt;"
	$ Text.replace ">" "&gt;"
	$ Text.replace "@" ""
	$ Text.replace "&" "&amp;"
	$ s
renderCode (TeXSeq a b) = renderCode a ++ renderCode b
renderCode (TeXBraces x) = "{" ++ renderCode x ++ "}"
renderCode (TeXEnv e [] x) | e `elem` makeCodeblock = renderCode x
renderCode other = render other

isComplexMath :: LaTeX -> Bool
isComplexMath (TeXMath _ t) = 
	(not . null $ matchCommand (`elem` ["frac", "sum", "binom"]) t)
	||
	(not . null $ matchEnv (`elem` ["array"]) t)
isComplexMath (TeXEnv e _ _) = e `elem` ["eqnarray*"]
isComplexMath _ = False

renderMath :: LaTeX -> Text
renderMath m
	| isComplexMath m = renderComplexMath m
	| otherwise = spanTag s $ renderSimpleMath m
	where
		s = mathKind m
		mathKind (TeXMath Square _) = "mathblock"
		mathKind _ = "math"

renderSimpleMath :: LaTeX -> Text
renderSimpleMath (TeXRaw s) =
	case suffix of
		Just ('^', rest) -> italicise prefix ++ output "sup" rest
		Just ('_', rest) -> italicise prefix ++ output "sub" rest
		_ -> italicise s
	where
		(prefix, suffix') = Text.break (`elem` ['^', '_']) s
		suffix = Text.uncons suffix'

		output tag rest =
			case Text.uncons rest of
				Just (c, rest') -> xml tag [] (italicise $ Text.singleton c) ++ (renderSimpleMath $ TeXRaw rest')
				Nothing -> error "Malformed math"

		italicise t =
			case Text.span isAlpha t of
				("", "") -> ""
				("", rest) ->
					case Text.uncons rest of
						Just (c, rest') -> entities c ++ italicise rest'
						Nothing -> error ""
				(alpha, rest) -> spanTag "mathalpha" alpha ++ italicise rest

		entities :: Char -> Text
		entities '<' = "&lt;"
		entities '>' = "&gt;"
		entities c = Text.singleton c
renderSimpleMath (TeXSeq (TeXRaw s) rest)
	| last `elem` ["^", "_"] =
		renderSimpleMath (TeXRaw $ Text.reverse $ Text.drop 1 s')
		++ xml tag [] (renderSimpleMath content)
		++ renderSimpleMath rest'
	| otherwise = renderSimpleMath (TeXRaw s) ++ renderSimpleMath rest
	where
		s' = Text.reverse s
		last = Text.take 1 s'
		tag = case last of
			"^" -> "sup"
			"_" -> "sub"
			_ -> error ""
		(content, rest') = case rest of
			(TeXSeq a b) -> (a, b)
			other -> (other, TeXEmpty)
renderSimpleMath (TeXBraces x) = renderSimpleMath x
renderSimpleMath (TeXSeq (TeXComm "frac" [(FixArg num)]) rest) =
	"[" ++ renderSimpleMath num ++ "] / [" ++ renderSimpleMath den ++ "]" ++ renderSimpleMath rest'
	where
		(den, rest') = findDenum rest
		findDenum (TeXSeq (TeXBraces d) r) = (d, r)
		findDenum (TeXSeq _ r) = findDenum r
		findDenum r = (r, TeXEmpty)
renderSimpleMath (TeXSeq a b) = (renderSimpleMath a) ++ (renderSimpleMath b)
renderSimpleMath (TeXMath _ m) = renderSimpleMath m
renderSimpleMath other = render other

renderComplexMath :: LaTeX -> Text
renderComplexMath m =
	unsafePerformIO $ do
	exists <- doesFileExist filePath
	when (not exists) generateImage
	return $ "<img src='ToImage/" ++ Text.pack fileName ++ "' class='" ++ imgClass m ++ "' alt='Formula'/>"

	where
		imgClass (TeXMath Square _) = "mathblockimg"
		imgClass (TeXMath _ _) = "mathinlineimg"
		imgClass _ = "mathblockimg"

		generateImage =
			withSystemTempDirectory "genhtml" $ \tmp -> do
			_ <- readProcess "latex" ["-output-format=dvi", "-output-directory=" ++ tmp, "-halt-on-error"] latex
			_ <- readProcess "dvipng" ["-T", "tight", "-D", "130", tmp ++ "/texput.dvi", "-o", filePath] ""
			return ()

		math = TeXRender.render m
		fileName = (show . abs $ hash math) ++ ".png"
		filePath = outputDir ++ imgDir ++ fileName
		latex = Text.unpack $
			"\\documentclass{article}\n" ++
			"\\pagestyle{empty}\n" ++
			"\\usepackage{array}\n" ++
			"\\usepackage{amsmath}\n" ++
			"\\usepackage{mathrsfs}\n" ++
			"\\usepackage[T1]{fontenc}\n" ++
			"\\begin{document}\n"
			++
			math
			++
			"\\end{document}\n"

renderTable :: LaTeX -> [Row] -> Text
renderTable colspec =
	xml "table" [] .
	renderRows (parseColspec $ Text.unpack $ stripColspec colspec)
	where
		stripColspec (TeXRaw s) = s
		stripColspec (TeXSeq a b) = stripColspec a ++ stripColspec b
		stripColspec _ = ""

		parseColspec :: String -> [Text]
		parseColspec ('|' : rest) = pc rest
		parseColspec other = pc other

		pc ('|' : []) = []
		pc ('|' : letter : rest) =
			"border " ++ (colClass letter) : pc rest
		pc (letter : rest) =
			colClass letter : pc rest
		pc "" = []

		colClass x | x `elem` ['l', 'm', 'x'] = "left"
		colClass 'p' = "justify"
		colClass 'r' = "right"
		colClass 'c' = "center"
		colClass other = error $ "Unexpected column type " ++ (other : [])

		combine newCs oldCs
			| Just _ <- Text.stripPrefix "border" oldCs,
			  Nothing <- Text.stripPrefix "border" newCs = "border " ++ newCs
			| otherwise = newCs

		renderRows _ [] = ""
		renderRows cs (Row{..} : rest) =
			(xml "tr" cls $ renderCols cs cells) ++ renderRows cs rest
			where
				cls | RowSep <- rowSep = [("class", "rowsep")]
				    | CapSep <- rowSep = [("class", "capsep")]
				    | otherwise = []

		renderCols _ [] = ""
		renderCols (c : cs) (Cell{..} : rest)
			| length cs < length rest = undefined
			| Multicolumn w cs' <- cellSpan =
				let 
					[c''] = parseColspec $ Text.unpack $ stripColspec cs'
					c' = combine c'' c
					colspan
						| rest == [] = length cs + 1
						| otherwise = w
				in
					(xml "td" [("colspan", Text.pack $ show colspan), ("class", c')] $ renderCell content)
					++ renderCols (drop (colspan - 1) cs) rest
			| otherwise =
				(xml "td" [("class", c)] $ renderCell content)
				++ renderCols cs rest
		renderCols [] (_ : _) = error "Too many columns"

renderCell :: Paragraph -> Text
renderCell = mconcat . map renderCell'
	where
		renderCell' (LatexElements t) = render t
		renderCell' other = render other

-- Explicit <br/>'s are redundant in <pre>, so strip them.
preprocessPre :: LaTeX -> LaTeX
preprocessPre (TeXLineBreak _ _) = TeXEmpty
preprocessPre (TeXCommS "br") = TeXEmpty
preprocessPre (TeXEnv e a c) = TeXEnv e a (preprocessPre c)
preprocessPre (TeXSeq a b) = TeXSeq (preprocessPre a) (preprocessPre b)
preprocessPre rest = rest

bnfPre :: Text -> Text
bnfPre = xml "pre" [("class", "bnf")] . Text.strip

preprocessTabbing :: LaTeX -> LaTeX
preprocessTabbing = go
	where
		initialTab (Text.stripPrefix ">" -> Just rest) =
			"\t" ++ rest
		initialTab other = other

		go (TeXBraces t) = t
		go (TeXSeq (TeXCommS "") (TeXSeq (TeXRaw s) rest)) =
			TeXSeq (TeXCommS "") (TeXSeq (TeXRaw $ initialTab s) $ go rest)
		go (TeXSeq (TeXCommS "") (TeXRaw s)) =
			TeXSeq (TeXCommS "") (TeXRaw $ initialTab s)
		go (TeXSeq a b) = TeXSeq (go a) (go b)
		go (TeXEnv e a c) = TeXEnv e a (go c)
		go other = other

htmlTabs :: Text -> Text
htmlTabs = Text.replace "\t" "&#9;"

renderBnfTable :: LaTeX -> Text
renderBnfTable = bnfPre . htmlTabs . render . preprocessTabbing . preprocessPre

renderTabbing :: LaTeX -> Text
renderTabbing = xml "pre" [] . htmlTabs . render . preprocessTabbing . preprocessPre

renderParagraph :: Text -> (Int, Paragraph) -> Text
renderParagraph idPrefix (show -> Text.pack -> i, x) =
	xml "div" [("class", "para"), ("id", idPrefix ++ i)] $
	xml "div" [("class", "paranumberparent")]
		(render (anchor{aClass="paranumber", aHref="#" ++ idPrefix ++ i,aText=i})) ++
	render x

data Link = TocToSection | SectionToToc | SectionToSection | ToImage
	deriving Show

linkToSection :: Link -> LaTeX -> Anchor
linkToSection link abbr = anchor
	{	aHref = Text.pack (show link) ++ "/" ++ url abbr
	,	aText  = "[" ++ render abbr ++ "]" }

url :: LaTeX -> Text
url (TeXRaw x) = urlEncode x
url (TeXSeq x y) = url x ++ url y
url (TeXCommS "dcr") = "--"
url _ = "TODO"

data SectionPath = SectionPath
	{ chapterKind :: ChapterKind
	, sectionNums :: [Int]
	}

numberSubsecs :: SectionPath -> [Section] -> [(SectionPath, Section)]
numberSubsecs (SectionPath k ns) = zip [SectionPath k (ns ++ [i]) | i <- [1..]]

renderSection :: Maybe LaTeX -> Bool -> (SectionPath, Section) -> (Text, Bool)
renderSection specific parasEmitted (path@SectionPath{..}, Section{..})
	| full = (, True) $
		xml "div" [("id", render abbreviation)] $ header ++
		xml "div" [("class", "para")] (render preamble) ++
		mconcat (map
			(renderParagraph (if parasEmitted then url abbreviation ++ "-" else ""))
			(zip [1..] paragraphs)) ++
		mconcat (fst . renderSection Nothing True . numberSubsecs path subsections)
	| not anysubcontent = ("", False)
	| otherwise =
		( header ++
		  mconcat (fst . renderSection specific False . numberSubsecs path subsections)
		, anysubcontent )
	where
		full = specific == Nothing || specific == Just abbreviation
		header = h Nothing (min 4 $ length sectionNums) $
			(if full
				then render $ anchor{
					aClass = "secnum",
					aHref  = "#" ++ url abbreviation,
					aText  = render path }
				else spanTag "secnum" (render path))
			++ render sectionName
			++ if specific == Just abbreviation
				then xml "span" [("class","abbr_ref")] $
					"[" ++ render abbreviation ++ "] "
					++ render (linkToSection SectionToToc abbreviation){aText="⬆"}
				else render (linkToSection SectionToSection abbreviation){aClass="abbr_ref"}
				
		anysubcontent =
			or $ map (snd . renderSection specific True)
			   $ numberSubsecs path subsections


instance Render SectionPath where
	render (SectionPath k ns)
		| k == InformativeAnnex, [_] <- ns = "Annex " ++ chap ++ "&emsp;(informative)<br/>"
		| k == NormativeAnnex, [_] <- ns = "Annex " ++ chap ++ "&emsp;(normative)<br/>"
		| otherwise = Text.intercalate "." (chap : Text.pack . show . tail ns)
		where
			chap :: Text
			chap
				| k == NormalChapter = Text.pack (show (head ns))
				| otherwise = Text.pack [['A'..] !! (head ns - 1)]

abbreviations :: Section -> [LaTeX]
abbreviations Section{..} = abbreviation : concatMap abbreviations subsections

withPaths :: [Chapter] -> [(SectionPath, Section)]
withPaths chapters = f normals ++ f annexes
	where
		f :: [(ChapterKind, Section)] -> [(SectionPath, Section)]
		f x = (\(i, (k, s)) -> (SectionPath k [i], s)) . zip [1..] x
		(normals, annexes) = span ((== NormalChapter) . fst) chapters

sectionFileContent :: SectionFileStyle -> [Chapter] -> LaTeX -> Text
sectionFileContent sfs chapters abbreviation = applySectionFileStyle sfs $
	fileContent
		("[" ++ render abbreviation ++ "]")
		(mconcat $ fst . renderSection (Just abbreviation) False . withPaths chapters)
		(if sfs == InSubdir then "../" else "")

tocFileContent :: SectionFileStyle -> Draft -> Text
tocFileContent sfs Draft{..} = applySectionFileStyle sfs $
		fileContent
			"14882: Contents"
			(	"<p style='text-align:center'>"
				++ "Generated on " ++ date
				++ " from the C++ standard's <a href='" ++ commitUrl ++ "'>draft LaTeX sources</a>"
				++ " by <a href='https://github.com/Eelis/cxxdraft-htmlgen'>cxxdraft-htmlgen</a>."
				++ "</p><hr/>"
				++ mconcat (map section (withPaths chapters))
			)
			""
	where
		date = Text.pack (formatTime defaultTimeLocale "%F" (unsafePerformIO getCurrentTime))
		section :: (SectionPath, Section) -> Text
		section (sectionPath, Section{..}) =
			xml "div" [("id", render abbreviation)] $
			h Nothing (min 4 $ 1 + length (sectionNums sectionPath)) (
				spanTag "secnum" (render sectionPath) ++
				render (sectionName, (linkToSection TocToSection abbreviation){aClass="abbr_ref"})) ++
			mconcat (map section (numberSubsecs sectionPath subsections))

fullFileContent :: SectionFileStyle -> [Chapter] -> Text
fullFileContent sfs chapters = applySectionFileStyle sfs $
	fileContent
		"14882"
		(mconcat $ applySectionFileStyle sfs . fst . renderSection Nothing True . withPaths chapters)
		(if sfs == InSubdir then "../" else "")

fileContent :: Text -> Text -> Text -> Text
fileContent title body pathHome =
	"<!DOCTYPE html>" ++
	"<html>" ++
		"<head>" ++
			"<title>" ++ title ++ "</title>" ++
			"<meta charset='UTF-8'/>" ++
			"<link rel='stylesheet' type='text/css' href='" ++ pathHome ++ "14882.css'/>" ++
		"</head>" ++
		"<body><div class='wrapper'>" ++ body ++ "</div></body>" ++
	"</html>"

urlEncode :: Text -> Text
urlEncode
	= Text.replace "<" "%3c"
	. Text.replace ">" "%3e"
	. Text.replace ":" "%3a"

abbrAsPath :: LaTeX -> Text
abbrAsPath (TeXRaw x) = x
abbrAsPath (TeXSeq x y) = abbrAsPath x ++ abbrAsPath y
abbrAsPath (TeXCommS "dcr") = "--"
abbrAsPath _ = "TODO"

data SectionFileStyle
	= Bare          -- e.g. intro.execution
	| WithExtension -- e.g. intro.execution.html
	| InSubdir      -- e.g. intro.execution/index.html
	deriving (Eq, Read)

doLink :: SectionFileStyle -> Link -> Text -> Text
doLink sfs l = go . Text.splitOn (Text.pack (show l) ++ "/")
	where
		go (x : (Text.breakOn "'" -> (a, b)) : z) = x ++ f a ++ go (b : z)
		go [x] = x
		go _ = undefined
		idir = Text.pack imgDir
		f :: Text -> Text
		f u = case (sfs, l) of
			(Bare, SectionToToc) -> "./#" ++ u
			(Bare, TocToSection) -> u
			(Bare, SectionToSection) -> u
			(Bare, ToImage) -> idir ++ u
			(InSubdir, SectionToToc) -> "../#" ++ u
			(InSubdir, TocToSection) -> u ++ "/"
			(InSubdir, SectionToSection) -> "../" ++ u
			(InSubdir, ToImage) -> "../" ++ idir ++ u
			(WithExtension, SectionToToc) -> "index.html#" ++ u
			(WithExtension, TocToSection) -> u ++ ".html"
			(WithExtension, SectionToSection) -> u ++ ".html"
			(WithExtension, ToImage) -> idir ++ u

applySectionFileStyle :: SectionFileStyle -> Text -> Text
applySectionFileStyle sfs =
	doLink sfs SectionToSection
	. doLink sfs SectionToToc
	. doLink sfs TocToSection
	. doLink sfs ToImage

writeStuff :: SectionFileStyle -> Draft -> IO ()
writeStuff sfs d@Draft{..} = do
	putStr $ "Writing to " ++ outputDir; hFlush stdout

	createDirectoryIfMissing True outputDir
	createDirectoryIfMissing True (outputDir ++ imgDir)

	copyFile "14882.css" (outputDir ++ "/14882.css")

	writeFile (outputDir ++ "/index.html") $ tocFileContent sfs d

	fullFile <- case sfs of
		Bare -> return "full"
		WithExtension -> return "full.html"
		InSubdir -> do
			createDirectoryIfMissing True (outputDir ++ "/full")
			return "full/index.html"
	writeFile (outputDir ++ fullFile) $ fullFileContent sfs chapters

	let allAbbrs = concatMap abbreviations (snd . chapters)
	forM_ allAbbrs $ \abbreviation -> do
		putStr "."; hFlush stdout
		f <- case sfs of
			Bare -> return $ Text.unpack $ abbrAsPath abbreviation
			WithExtension -> return $ Text.unpack $ abbrAsPath abbreviation ++ ".html"
			InSubdir -> do
				let dir = "/" ++ Text.unpack (abbrAsPath abbreviation)
				createDirectoryIfMissing True (outputDir ++ dir)
				return $ dir ++ "/index.html"
		writeFile (outputDir ++ f) $ sectionFileContent sfs chapters abbreviation
	putStrLn $ " " ++ show (length allAbbrs) ++ " sections"

data CmdLineArgs = CmdLineArgs
	{ repo :: FilePath
	, sectionFileStyle :: SectionFileStyle }

readCmdLineArgs :: [String] -> CmdLineArgs
readCmdLineArgs = \case
	[repo, read -> sectionFileStyle] -> CmdLineArgs{..}
	[repo] -> CmdLineArgs{sectionFileStyle=WithExtension,..}
	_ -> error "param: path/to/repo"

outputDir, imgDir :: FilePath
outputDir = "14882/"
imgDir = "math/"

main :: IO ()
main = do
	cwd <- getCurrentDirectory

	CmdLineArgs{..} <- readCmdLineArgs . getArgs

	setCurrentDirectory $ repo ++ "/source"
	
	draft <- load14882

	setCurrentDirectory cwd
	writeStuff sectionFileStyle draft
