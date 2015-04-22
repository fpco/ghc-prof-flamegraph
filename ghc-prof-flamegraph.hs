{-# LANGUAGE ScopedTypeVariables #-}
import           Data.List (intercalate)
import           Data.Char (isSpace)

parseLine :: String -> (String, String, String)
parseLine s = case words s of
  [costCentre, module_, _no, _entries, indTime, _indAlloc, _inhTime, _inhALloc] ->
    let simulatedEntries :: Int = round $ 10 * (read indTime :: Double)
    in (costCentre, module_, show simulatedEntries)
  _ ->
    error $ "parseLine: malformed .prof file line:\n" ++ s

processLines :: [String] -> [String]
processLines = go [] []
  where
    go :: [String] -> [String] -> [String] -> [String]
    go _stack frames [] = reverse frames
    go stack0 frames (line : lines') =
      let (spaces, rest) = break (not . isSpace) line
          stack = drop (length stack0 - length spaces) stack0
          (costCentre, module_, entries) = parseLine rest
          symbol = module_ ++ "." ++ costCentre
          stack' = symbol : stack
          frame = intercalate ";" (reverse stack') ++ " " ++ entries
      in go stack' (frame : frames) lines'

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
