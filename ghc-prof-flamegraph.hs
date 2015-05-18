{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Arrow (second)
import           Data.Char (isSpace)
import           Data.List (intercalate)

parseLine :: String -> (Int, (String, String, String))
parseLine s = case words s of
  [costCentre, module_, _no, _entries, indTime, _indAlloc, _inhTime, _inhALloc] ->
    let simulatedEntries :: Int = round $ 10 * (read indTime :: Double)
    in (simulatedEntries, (costCentre, module_, show simulatedEntries))
  _ ->
    error $ "parseLine: malformed .prof file line:\n" ++ s

processLines :: [String] -> [String]
processLines ls =
  let (entries, ls') = go 0 [] ls
      unknown = 1000 - entries
  in if unknown < 0
    then error "processLines: malformed .prof file, percentages greater than 100%"
    else if unknown > 0
      then ("UNKNOWN " ++ show unknown) : ls'
      else ls'
  where
    go :: Int -> [String] -> [String] -> (Int, [String])
    go totalEntries _stack [] = (totalEntries, [])
    go totalEntries stack0 (line : lines') =
      let (spaces, rest) = break (not . isSpace) line
          stack = drop (length stack0 - length spaces) stack0
          (entriesInt, (costCentre, module_, entries)) = parseLine rest
          symbol = module_ ++ "." ++ costCentre
          stack' = symbol : stack
          frame = intercalate ";" (reverse stack') ++ " " ++ entries
      in second (frame :) $ go (totalEntries + entriesInt) stack' lines'

firstLine :: [String]
firstLine = ["COST", "CENTRE", "MODULE", "no.", "entries", "%time", "%alloc", "%time", "%alloc"]

findStart :: [String] -> [String]
findStart [] = error "findStart: malformed .prof file"
findStart (line : _empty : lines') | words line == firstLine = lines'
findStart (_line : lines') = findStart lines'

main :: IO ()
main = do
  s <- getContents
  putStr $ unlines $ processLines $ findStart $ lines s
