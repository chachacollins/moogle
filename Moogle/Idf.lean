import Moogle.Tf

def termAppearsInNoDocs (term: String) (docs: List BoW) : Nat :=
  sum (docs.map fun doc => if doc.textFreq.contains term then 1 else 0)

def idf (term: String) (docs: List BoW) : Float :=
  Float.log (n1.toFloat / n2.toFloat) + 1
  where
    n1 := List.length docs
    n2 := termAppearsInNoDocs term docs + 1
