import Moogle.Tf

def termAppearsInNoDocs (term: String) (docs: List BoW) : Nat :=
  sum (docs.map fun doc => if doc.textFreq.contains term then 1 else 0)

def idf (term: String) (docs: List BoW) : Float :=
  Float.log (n1 / n2).toFloat
  where
    n1 := List.length docs + 1
    n2 := termAppearsInNoDocs term docs + 1
