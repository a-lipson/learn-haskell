-- advanced exercises part three
-- case-insensitive word search

-- using: readFile, putStr, putStrLn, getLine

import           Control.Monad
import           Data.Char     (isLetter, isSpace, toLower)
import           Data.List     (words)
import           System.IO

-- words = []

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  -- putStr "Words to search for: "
  searchWords <- getSearchWords
  putStr "File to search in: "
  path <- getLine
  text <- readFile path
  let found = findStrings searchWords text
  let notfound = [ w | w <- searchWords, w `notElem` found ]
  mapM_ (\s -> putStrLn $ "\"" ++ s ++ "\" found") found
  mapM_ (\s -> putStrLn $ "\"" ++ s ++ "\" NOT found") notfound

getSearchWords :: IO [String]
getSearchWords = do
  putStrLn "Words to search for:"
  aux
    where
      aux = do
        putStr "> "
        line <- getLine
        if line == "" then return [] else do
          xs <- aux
          return $ line:xs

findStrings :: [String] -> String -> [String]
findStrings searchWords text = [ w | w <- searchWords, lower w `elem` textWords ]
  where
    textWords = map lower $ words fText
    fText = filter (\x -> isLetter x || isSpace x) text
    lower = map toLower


-- main :: IO ()
-- main = do
--   hSetBuffering stdout NoBuffering
--   putStr "Please input words to search for: "
--   word <- getLine . toLower
--
--   let mut searchWords = []
--
--   when (word /= "") $ do
--     word : searchWords
--     main
--
--   filePath <- getLine
--   fileData <- readFile filePath -- can put this into one line?
--
--   -- needs to accept a regular string, how to take out of IO?
--   let fileWords = words fileData
--
--   map (\word -> putStrLn "\"" <> word <> "\"" ++ if search fileWords word then " " else " NOT " ++ "found") searchWords
--
--
-- search :: [String] -> String -> Bool
-- search words word = word == head words && search (tail words) word



