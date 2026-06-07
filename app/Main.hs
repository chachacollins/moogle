module Main (main) where

import qualified Data.Ord as Ord
import qualified Data.List as L
import qualified System.Directory as D
import qualified System.Environment as Env
import qualified Tf
import qualified TfIdf

data CliArgs = CliArgs
  {
    dirPath :: String
  , term    :: String
  } deriving (Show)

parseCliArgs :: [String] -> CliArgs
parseCliArgs args = CliArgs { dirPath = args !! 0, term = args !! 1}

printUsage :: IO ()
printUsage = putStrLn "[USAGE]: moogle <directory path> <term>"

printResult :: [(Float, String)] -> IO ()
printResult [] = return ()
printResult ((tfidf, file):xs) = do
  putStrLn (file <> " -> " <> show tfidf)
  printResult xs

main :: IO ()
main = do
  args <- Env.getArgs
  if length args < 2 then
    printUsage
  else do
    let cliArgs = parseCliArgs args
    let dir     = dirPath cliArgs
    files     <- filter (L.isSuffixOf ".md") <$> D.listDirectory dir
    readFiles <- mapM (\x -> readFile (dir <> x)) files
    let docs   = Tf.newDocuments readFiles
    let result = L.sortBy (Ord.comparing Ord.Down) (zip (TfIdf.tfidf (term cliArgs) docs) files)
    putStrLn "======= Results ======="
    printResult result
