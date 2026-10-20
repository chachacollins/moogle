import Std.Data.HashMap
open Std

structure BoW: Type where
  textFreq: HashMap String Nat
  deriving Repr

def BoW.empty: BoW := {textFreq := {}}

def BoW.new (word: String): BoW :=
  {textFreq := HashMap.ofList [(word, 1)]}

def BoW.merge (left right : BoW) : BoW :=
  { textFreq := right.textFreq.fold (init := left.textFreq)
      (fun acc word n => acc.insert word (acc.getD word 0 + n)) }

def BoW.mergeAll (bows: List BoW) : BoW :=
  bows.foldl (BoW.merge) (BoW.empty)

def sum (xs: List Nat) : Nat :=
  xs.foldl (fun acc x => x + acc) 0

def BoW.totalFreq (bow: BoW) : Nat :=
  sum (bow.textFreq.fold (init := []) (fun acc _ v => [v] ++ acc))

def List.mergeBoWs (bows: List BoW): BoW :=
  BoW.mergeAll bows

def tf (term: String) (doc: BoW) : Float :=
  (doc.textFreq.getD term 0).toFloat / doc.totalFreq.toFloat
