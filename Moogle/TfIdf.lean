import Moogle.Tf
import Moogle.Idf

def tfidf (term: String) (docs: List BoW) : List Float :=
  let t_idf := (idf term docs)
  docs.map ((· * t_idf) ∘ tf term)
