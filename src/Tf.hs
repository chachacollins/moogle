module Tf (  tf,
             Bow(..),
             newBag,
             tokenize,
             Term,
             Document,
             newDocument,
             newDocuments,
           )
where
import Data.Char
import qualified Data.HashMap.Strict as M

tokenize :: String -> [String]
tokenize str = map (map toLower . filter isAlphaNum) $ words str

newtype Bow = Bow 
  { textFreqs :: M.HashMap String Float
  } deriving (Show, Eq)

instance Semigroup Bow where
 (Bow a) <> (Bow b) = Bow ( M.unionWith (+) a b)

instance Monoid Bow where
  mempty = Bow M.empty

newBag :: String -> Bow
newBag str = Bow {textFreqs = M.singleton str 1}

newDocument :: String -> Bow
newDocument = foldMap newBag . tokenize

newDocuments :: [String] -> [Bow]
newDocuments = map newDocument

type Term = String

type Document = Bow

type TermFreq = Float

-- https://en.wikipedia.org/wiki/Tf%E2%80%93idf#Term_frequency
tf :: Term -> Document -> TermFreq
tf term doc = maybe 0 (/ docSize) (M.lookup term (textFreqs doc))
  where
    docSize = let (_, n) = unzip $ M.toList (textFreqs doc) in sum n
