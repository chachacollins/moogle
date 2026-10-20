import Moogle.TfIdf

def tokenize (str : String) : List String :=
  (str.split Char.isWhitespace).toList.filterMap fun w =>
    let t := (String.ofList (w.copy.toList.filter Char.isAlphanum)).toLower
    if t.isEmpty then none else some t

def printUsage : IO Unit :=
  IO.println "[USAGE]: moogle <directory path> <term>"

def listDirectory (dir : String) : IO (List String) := do
  let entries ← System.FilePath.readDir dir
  return entries.toList.map IO.FS.DirEntry.fileName

def printResult : List (Float × String) → IO Unit
| [] => pure ()
| (score, file) :: xs => do
      IO.println s!"{file}: {score}"
      printResult xs

def main (args: List String) : IO Unit :=
  if List.length args != 2 then
    printUsage
  else do
    let dir  := args[0]!
    let term := args[1]!
    let files <- List.filter (·.endsWith ".md") <$> listDirectory dir
    let readFiles ← files.mapM fun x =>
       IO.FS.readFile (System.FilePath.mk (dir ++ "/" ++ x))
    -- Tokenize file content into words and create BoW from the tokens
    let docs := readFiles.map (BoW.fromList ∘ tokenize)
    let result := (List.zip (tfidf term docs) files).mergeSort
                    fun a b => a.1 > b.1 || (a.1 == b.1 && a.2 ≥ b.2)
    printResult result
