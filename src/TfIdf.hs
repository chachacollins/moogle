module TfIdf(tfidf) where
import Tf
import Idf

tfidf :: Term -> [Document] -> [Float]
tfidf term docs =
  let t_idf = idf term docs in
  map ((* t_idf) . tf term) docs
