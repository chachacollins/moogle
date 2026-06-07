module Idf (idf) where
import qualified Data.HashMap.Strict as M
import Tf

appearsIn :: Term -> [Document] -> Int
appearsIn term docs = sum (map aux docs)
  where
    aux doc = maybe 0 (const 1) (M.lookup term (textFreqs doc))

idf :: Term -> [Document] -> Float
idf term docs = log (n1 / n2)
  where
    n1 = fromIntegral ((length docs) + 1)
    n2 = fromIntegral ((term `appearsIn` docs) + 1)
