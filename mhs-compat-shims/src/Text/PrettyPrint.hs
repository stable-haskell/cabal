-- | MicroHs-compatible subset of "Text.PrettyPrint" (HughesPJ).
--
-- This module reimplements the HughesPJ pretty-printing combinators from
-- the Haskell @pretty@ package (version 1.1.3.6) in plain Haskell 2010,
-- restricted to the names used by @Cabal-syntax@ (see
-- @Distribution.Pretty@): 'Doc', 'Style', 'Mode', 'style', 'render',
-- 'renderStyle', 'text', 'char', 'int', 'integer', 'empty', 'isEmpty',
-- '(<>)', '(<+>)', '($$)', '($+$)', 'nest', 'sep', 'cat', 'fsep', 'hsep',
-- 'vcat', 'hcat', 'punctuate', 'comma', 'colon', 'semi', 'space',
-- 'parens', 'brackets', 'braces', 'quotes' and 'doubleQuotes'.
--
-- The layout algorithm is the original one: a document describes a set of
-- layouts (via 'Union'), and rendering picks the widest fitting layout by
-- comparing each first line against the available space (page width and
-- ribbon derived from the 'Style'). 'PageMode' fits, 'LeftMode' renders
-- flat (ignoring line length, honouring explicit newlines) and
-- 'OneLineMode' renders everything on one line.
module Text.PrettyPrint
  ( Doc
  , Style (..)
  , Mode (..)
  , style
  , render
  , renderStyle
  , text
  , char
  , int
  , integer
  , empty
  , isEmpty
  , (<>)
  , (<+>)
  , ($$)
  , ($+$)
  , nest
  , sep
  , cat
  , fsep
  , hsep
  , vcat
  , hcat
  , punctuate
  , comma
  , colon
  , semi
  , space
  , parens
  , brackets
  , braces
  , quotes
  , doubleQuotes
  ) where

import Prelude hiding ((<>))
import qualified Data.Semigroup as Semi (Semigroup ((<>)))
import Data.String (IsString (..))

infixl 6 <>
infixl 6 <+>
infixl 5 $$, $+$

-- ---------------------------------------------------------------------------
-- The Doc data type
--
-- A 'Doc' represents a /set/ of layouts. A 'Doc' with no 'Union' or 'NoDoc'
-- inside represents just one layout. 'Beside' and 'Above' are unreduced
-- compositions; 'reduceDoc' normalises them into the reduced constructors
-- ('Empty', 'NilAbove', 'TextBeside', 'Nest', 'Union', 'NoDoc').

data Doc
  = Empty
  | NilAbove Doc
  | TextBeside String Int Doc
  | Nest Int Doc
  | Union Doc Doc
  | NoDoc
  | Beside Doc Bool Doc
  | Above Doc Bool Doc

-- | 'mappend' is '(<>)', 'mempty' is 'empty'.
instance Semi.Semigroup Doc where
  (<>) = (Text.PrettyPrint.<>)

instance Monoid Doc where
  mempty = empty
  mappend = (Semi.<>)

instance IsString Doc where
  fromString = text

-- | Rendered comparison, as in the original.
instance Eq Doc where
  p == q = render p == render q

-- | Shows via the default 'style', as in the original.
instance Show Doc where
  showsPrec _ doc rest = renderStyle style doc ++ rest

-- ---------------------------------------------------------------------------
-- Rendering style

-- | Rendering style: mode, maximum line length, and ribbon ratio.
data Style = Style
  { mode :: Mode
  , lineLength :: Int
  , ribbonsPerLine :: Float
  }
  deriving (Eq, Show)

-- | The default style (@mode=PageMode, lineLength=100, ribbonsPerLine=1.5@).
style :: Style
style = Style { mode = PageMode, lineLength = 100, ribbonsPerLine = 1.5 }

-- | Rendering mode.
data Mode
  = PageMode
  | ZigZagMode
  | LeftMode
  | OneLineMode
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Simple constructors and predicates

-- | An empty document ('mempty').
empty :: Doc
empty = Empty

-- | True only for 'empty'.
isEmpty :: Doc -> Bool
isEmpty Empty = True
isEmpty _ = False

-- | A document of height and width 1, containing a literal character.
char :: Char -> Doc
char c = TextBeside [c] 1 Empty

-- | A document containing a string. The string is output verbatim.
text :: String -> Doc
text s = TextBeside s (length s) Empty

-- | @int n = text (show n)@.
int :: Int -> Doc
int n = text (show n)

-- | @integer n = text (show n)@.
integer :: Integer -> Doc
integer n = text (show n)

-- | A ';' character.
semi :: Doc
semi = char ';'

-- | A ',' character.
comma :: Doc
comma = char ','

-- | A ':' character.
colon :: Doc
colon = char ':'

-- | A space character.
space :: Doc
space = char ' '

-- | Wrap a document in @(...)@.
parens :: Doc -> Doc
parens p = char '(' <> p <> char ')'

-- | Wrap a document in @[...]@.
brackets :: Doc -> Doc
brackets p = char '[' <> p <> char ']'

-- | Wrap a document in @{...}@.
braces :: Doc -> Doc
braces p = char '{' <> p <> char '}'

-- | Wrap a document in @\'...\'@.
quotes :: Doc -> Doc
quotes p = char '\'' <> p <> char '\''

-- | Wrap a document in @\"...\"@.
doubleQuotes :: Doc -> Doc
doubleQuotes p = char '"' <> p <> char '"'

-- | @punctuate p [d1, ... dn] = [d1 <> p, d2 <> p, ... dn-1 <> p, dn]@.
punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ [] = []
punctuate p (x : xs) = go x xs
  where
    go y [] = [y]
    go y (z : zs) = (y <> p) : go z zs

-- | Indent a document by the given number of positions.
nest :: Int -> Doc -> Doc
nest k p = mkNest k (reduceDoc p)

-- ---------------------------------------------------------------------------
-- Structural operations

-- | Simplify a built up 'Doc' into reduced form (no top-level
-- 'Beside' or 'Above').
reduceDoc :: Doc -> Doc
reduceDoc (Beside p g q) = beside p g (reduceDoc q)
reduceDoc (Above p g q) = above p g (reduceDoc q)
reduceDoc p = p

-- | List version of '(<>)'.
hcat :: [Doc] -> Doc
hcat = snd . reduceHoriz . foldr (\p q -> Beside p False q) empty

-- | List version of '(<+>)'.
hsep :: [Doc] -> Doc
hsep = snd . reduceHoriz . foldr (\p q -> Beside p True q) empty

-- | List version of '($$)'.
vcat :: [Doc] -> Doc
vcat = snd . reduceVert . foldr (\p q -> Above p False q) empty

data IsEmpty = IsEmpty | NotEmpty

reduceHoriz :: Doc -> (IsEmpty, Doc)
reduceHoriz (Beside p g q) =
  eliminateEmpty Beside (snd (reduceHoriz p)) g (reduceHoriz q)
reduceHoriz doc = (NotEmpty, doc)

reduceVert :: Doc -> (IsEmpty, Doc)
reduceVert (Above p g q) =
  eliminateEmpty Above (snd (reduceVert p)) g (reduceVert q)
reduceVert doc = (NotEmpty, doc)

eliminateEmpty ::
  (Doc -> Bool -> Doc -> Doc) ->
  Doc ->
  Bool ->
  (IsEmpty, Doc) ->
  (IsEmpty, Doc)
eliminateEmpty _ Empty _ q = q
eliminateEmpty cons p g q =
  ( NotEmpty,
    case q of
      (NotEmpty, q') -> cons p g q'
      (IsEmpty, _) -> p
  )

mkNest :: Int -> Doc -> Doc
mkNest k (Nest k1 p) = mkNest (k + k1) p
mkNest _ NoDoc = NoDoc
mkNest _ Empty = Empty
mkNest 0 p = p
mkNest k p = Nest k p

mkUnion :: Doc -> Doc -> Doc
mkUnion Empty _ = Empty
mkUnion p q = p `union_` q

nilAbove_ :: Doc -> Doc
nilAbove_ = NilAbove

textBeside_ :: String -> Int -> Doc -> Doc
textBeside_ = TextBeside

nest_ :: Int -> Doc -> Doc
nest_ = Nest

union_ :: Doc -> Doc -> Doc
union_ = Union

-- ---------------------------------------------------------------------------
-- Vertical composition

-- | Above, with overlapping. @(x $$ y)@ puts @y@ below @x@.
($$) :: Doc -> Doc -> Doc
p $$ q = above_ p False q

-- | Above, with no overlapping.
($+$) :: Doc -> Doc -> Doc
p $+$ q = above_ p True q

above_ :: Doc -> Bool -> Doc -> Doc
above_ p _ Empty = p
above_ Empty _ q = q
above_ p g q = Above p g q

above :: Doc -> Bool -> Doc -> Doc
above (Above p g1 q1) g2 q2 = above p g1 (above q1 g2 q2)
above p@(Beside _ _ _) g q = aboveNest (reduceDoc p) g 0 (reduceDoc q)
above p g q = aboveNest p g 0 (reduceDoc q)

aboveNest :: Doc -> Bool -> Int -> Doc -> Doc
aboveNest NoDoc _ _ _ = NoDoc
aboveNest (p1 `Union` p2) g k q =
  aboveNest p1 g k q `union_` aboveNest p2 g k q
aboveNest Empty _ k q = mkNest k q
aboveNest (Nest k1 p) g k q = nest_ k1 (aboveNest p g (k - k1) q)
aboveNest (NilAbove p) g k q = nilAbove_ (aboveNest p g k q)
aboveNest (TextBeside s l p) g k q = TextBeside s l rest
  where
    k1 = k - l
    rest = case p of
      Empty -> nilAboveNest g k1 q
      _ -> aboveNest p g k1 q
aboveNest _ _ _ _ = error "aboveNest on unreduced Doc"

-- Specification: @text s <> nilAboveNest g k q = text s <> (text "" $g$ nest k q)@.
nilAboveNest :: Bool -> Int -> Doc -> Doc
nilAboveNest _ _ Empty = Empty
nilAboveNest g k (Nest k1 q) = nilAboveNest g (k + k1) q
nilAboveNest g k q
  | not g && k > 0 = textBeside_ (indent k) k q
  | otherwise = nilAbove_ (mkNest k q)

-- ---------------------------------------------------------------------------
-- Horizontal composition

-- | Beside. '(<>)' is associative, with identity 'empty'.
(<>) :: Doc -> Doc -> Doc
p <> q = beside_ p False q

-- | Beside, separated by space, unless one side is 'empty'.
(<+>) :: Doc -> Doc -> Doc
p <+> q = beside_ p True q

beside_ :: Doc -> Bool -> Doc -> Doc
beside_ p _ Empty = p
beside_ Empty _ q = q
beside_ p g q = Beside p g q

beside :: Doc -> Bool -> Doc -> Doc
beside NoDoc _ _ = NoDoc
beside (p1 `Union` p2) g q = beside p1 g q `union_` beside p2 g q
beside Empty _ q = q
beside (Nest k p) g q = nest_ k (beside p g q)
beside p@(Beside p1 g1 q1) g2 q2
  | g1 == g2 = beside p1 g1 (beside q1 g2 q2)
  | otherwise = beside (reduceDoc p) g2 q2
beside p@(Above _ _ _) g q = beside (reduceDoc p) g q
beside (NilAbove p) g q = nilAbove_ (beside p g q)
beside (TextBeside s l p) g q = TextBeside s l rest
  where
    rest = case p of
      Empty -> nilBeside g q
      _ -> beside p g q

nilBeside :: Bool -> Doc -> Doc
nilBeside _ Empty = Empty
nilBeside g (Nest _ p) = nilBeside g p
nilBeside g p
  | g = textBeside_ " " 1 p
  | otherwise = p

-- ---------------------------------------------------------------------------
-- Separate ('sep' and 'cat')

-- Specification: @sep ps = oneLiner (hsep ps) `union` vcat ps@.
sep :: [Doc] -> Doc
sep = sepX True

-- Specification: @cat ps = oneLiner (hcat ps) `union` vcat ps@.
cat :: [Doc] -> Doc
cat = sepX False

sepX :: Bool -> [Doc] -> Doc
sepX _ [] = empty
sepX x (p : ps) = sep1 x (reduceDoc p) 0 ps

sep1 :: Bool -> Doc -> Int -> [Doc] -> Doc
sep1 _ NoDoc _ _ = NoDoc
sep1 g (p `Union` q) k ys =
  sep1 g p k ys `union_` aboveNest q False k (reduceDoc (vcat ys))
sep1 g Empty k ys = mkNest k (sepX g ys)
sep1 g (Nest n p) k ys = nest_ n (sep1 g p (k - n) ys)
sep1 _ (NilAbove p) k ys =
  nilAbove_ (aboveNest p False k (reduceDoc (vcat ys)))
sep1 g (TextBeside s l p) k ys =
  textBeside_ s l (sepNB g p (k - l) ys)
sep1 _ _ _ _ = error "sep1 on unreduced Doc"

sepNB :: Bool -> Doc -> Int -> [Doc] -> Doc
sepNB g (Nest _ p) k ys = sepNB g p k ys
sepNB g Empty k ys =
  oneLiner (nilBeside g (reduceDoc rest))
    `mkUnion` nilAboveNest False k (reduceDoc (vcat ys))
  where
    rest
      | g = hsep ys
      | otherwise = hcat ys
sepNB g p k ys = sep1 g p k ys

-- ---------------------------------------------------------------------------
-- Fill ('fsep')

-- | Paragraph-fill version of 'sep'.
fsep :: [Doc] -> Doc
fsep = fill True

fill :: Bool -> [Doc] -> Doc
fill _ [] = empty
fill g (p : ps) = fill1 g (reduceDoc p) 0 ps

fill1 :: Bool -> Doc -> Int -> [Doc] -> Doc
fill1 _ NoDoc _ _ = NoDoc
fill1 g (p `Union` q) k ys =
  fill1 g p k ys `union_` aboveNest q False k (fill g ys)
fill1 g Empty k ys = mkNest k (fill g ys)
fill1 g (Nest n p) k ys = nest_ n (fill1 g p (k - n) ys)
fill1 g (NilAbove p) k ys =
  nilAbove_ (aboveNest p False k (fill g ys))
fill1 g (TextBeside s l p) k ys =
  textBeside_ s l (fillNB g p (k - l) ys)
fill1 _ _ _ _ = error "fill1 on unreduced Doc"

fillNB :: Bool -> Doc -> Int -> [Doc] -> Doc
fillNB g (Nest _ p) k ys = fillNB g p k ys
fillNB _ Empty _ [] = Empty
fillNB g Empty k (Empty : ys) = fillNB g Empty k ys
fillNB g Empty k (y : ys) = fillNBE g k y ys
fillNB g p k ys = fill1 g p k ys

fillNBE :: Bool -> Int -> Doc -> [Doc] -> Doc
fillNBE g k y ys =
  nilBeside g (fill1 g (elideNest (oneLiner (reduceDoc y))) k' ys)
    `mkUnion` nilAboveNest False k (fill g (y : ys))
  where
    k'
      | g = k - 1
      | otherwise = k

elideNest :: Doc -> Doc
elideNest (Nest _ d) = d
elideNest d = d

-- | The one-line members of a set of layouts.
oneLiner :: Doc -> Doc
oneLiner NoDoc = NoDoc
oneLiner Empty = Empty
oneLiner (NilAbove _) = NoDoc
oneLiner (TextBeside s l p) = textBeside_ s l (oneLiner p)
oneLiner (Nest k p) = nest_ k (oneLiner p)
oneLiner (p `Union` _) = oneLiner p
oneLiner _ = error "oneLiner on unreduced Doc"

-- | @first@ returns its first argument if it is non-empty, else its second.
first :: Doc -> Doc -> Doc
first p q
  | nonEmptySet p = p
  | otherwise = q

nonEmptySet :: Doc -> Bool
nonEmptySet NoDoc = False
nonEmptySet (_ `Union` _) = True
nonEmptySet Empty = True
nonEmptySet (NilAbove _) = True
nonEmptySet (TextBeside _ _ p) = nonEmptySet p
nonEmptySet (Nest _ p) = nonEmptySet p
nonEmptySet _ = error "nonEmptySet on unreduced Doc"

-- ---------------------------------------------------------------------------
-- Selecting the best layout

-- | Choose the best rendering of a reduced document for the given line
-- length and ribbon length. The result contains no 'Union'.
best :: Int -> Int -> Doc -> Doc
best w0 r = get w0
  where
    get _ Empty = Empty
    get _ NoDoc = NoDoc
    get w (NilAbove p) = nilAbove_ (get w p)
    get w (TextBeside s l p) = textBeside_ s l (get1 w l p)
    get w (Nest k p) = nest_ k (get (w - k) p)
    get w (p `Union` q) = nicest w r (get w p) (get w q)
    get _ _ = error "best: unreduced Doc"

    get1 _ _ Empty = Empty
    get1 _ _ NoDoc = NoDoc
    get1 w sl (NilAbove p) = nilAbove_ (get (w - sl) p)
    get1 w sl (TextBeside s l p) = textBeside_ s l (get1 w (sl + l) p)
    get1 w sl (Nest _ p) = get1 w sl p
    get1 w sl (p `Union` q) =
      nicest1 w r sl (get1 w sl p) (get1 w sl q)
    get1 _ _ _ = error "best: unreduced Doc"

nicest :: Int -> Int -> Doc -> Doc -> Doc
nicest w r = nicest1 w r 0

nicest1 :: Int -> Int -> Int -> Doc -> Doc -> Doc
nicest1 w r sl p q
  | fits (min w r - sl) p = p
  | otherwise = q

-- | True if the first line of the document fits in the space available.
fits :: Int -> Doc -> Bool
fits n _ | n < 0 = False
fits _ NoDoc = False
fits _ Empty = True
fits _ (NilAbove _) = True
fits n (TextBeside _ l p) = fits (n - l) p
fits _ _ = error "fits on unreduced Doc"

-- ---------------------------------------------------------------------------
-- Rendering

-- | Render a document with the default 'style'.
render :: Doc -> String
render = fullRender (mode style) (lineLength style) (ribbonsPerLine style)

-- | Render a document with a given style.
renderStyle :: Style -> Doc -> String
renderStyle s = fullRender (mode s) (lineLength s) (ribbonsPerLine s)

fullRender :: Mode -> Int -> Float -> Doc -> String
fullRender OneLineMode _ _ doc = easyDisplay " " first (reduceDoc doc)
fullRender LeftMode _ _ doc = easyDisplay "\n" first (reduceDoc doc)
fullRender m lineLen ribbons doc = display m lineLen ribbonLen doc'
  where
    doc' = best bestLineLen ribbonLen (reduceDoc doc)
    ribbonLen :: Int
    ribbonLen = round (fromIntegral lineLen / ribbons)
    bestLineLen :: Int
    bestLineLen = case m of
      ZigZagMode -> maxBound
      _ -> lineLen

-- | Simple rendering for 'LeftMode' and 'OneLineMode': no fitting, breaks
-- become the given separator.
easyDisplay :: String -> (Doc -> Doc -> Doc) -> Doc -> String
easyDisplay nl choose = lay
  where
    lay NoDoc = error "easyDisplay: NoDoc"
    lay (Union p q) = lay (choose p q)
    lay (Nest _ p) = lay p
    lay Empty = ""
    lay (NilAbove p) = nl ++ lay p
    lay (TextBeside s _ p) = s ++ lay p
    lay _ = error "easyDisplay on unreduced Doc"

display :: Mode -> Int -> Int -> Doc -> String
display m pageWidth ribbonWidth doc =
  case pageWidth - ribbonWidth of
    gapWidth ->
      case gapWidth `quot` 2 of
        shift ->
          let lay k (Nest k1 p) = lay (k + k1) p
              lay _ Empty = ""
              lay k (NilAbove p) = '\n' : lay k p
              lay k (TextBeside s l p) =
                case m of
                  ZigZagMode
                    | k >= gapWidth ->
                        '\n'
                          : ( replicate shift '/'
                                ++ ('\n' : lay1 (k - shift) s l p)
                            )
                    | k < 0 ->
                        '\n'
                          : ( replicate shift '\\'
                                ++ ('\n' : lay1 (k + shift) s l p)
                            )
                  _ -> lay1 k s l p
              lay _ _ = error "display: unreduced Doc"
              lay1 k s l p = indent k ++ s ++ lay2 (k + l) p
              lay2 k (NilAbove p) = '\n' : lay k p
              lay2 k (TextBeside s l p) = s ++ lay2 (k + l) p
              lay2 k (Nest _ p) = lay2 k p
              lay2 _ Empty = ""
              lay2 _ _ = error "display: unreduced Doc"
           in lay 0 doc

indent :: Int -> String
indent n = replicate n ' '
