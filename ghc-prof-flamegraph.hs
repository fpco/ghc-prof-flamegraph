import           Data.List (intercalate, isPrefixOf)
import           Data.Char (isSpace)
import           Debug.Trace

parseLine :: String -> (String, String, String)
parseLine s = case words s of
  [costCentre, module_, _no, entries, _indTime, _indAlloc, _inhTime, _inhALloc] ->
    (costCentre, module_, entries)
  _ ->
    error $ "parseLine: malformed .prof file line:\n" ++ s

processLines :: [String] -> [String]
processLines = go [] []
  where
    go :: [String] -> [String] -> [String] -> [String]
    go _stack frames [] = reverse frames
    go stack0 frames (line : lines) = 
      let (spaces, rest) = break (not . isSpace) line
          stack = take (length spaces) stack0
          (costCentre, module_, entries) = parseLine rest
          symbol = module_ ++ "." ++ costCentre
          stack' = symbol : stack
          frame = intercalate ";" (reverse stack') ++ " " ++ entries
      in go stack' (frame : frames) lines

findStart :: [String] -> [String]
findStart [] = error "findStart: malformed .prof file"
findStart (line1 : line2 : lines) | words line1 == firstLine = lines
  where
    firstLine = ["COST", "CENTRE", "MODULE", "no.", "entries", "%time", "%alloc", "%time", "%alloc"]
findStart (line : lines) = findStart lines

main :: IO ()
main = do
  s <- getContents
  putStr $ unlines $ processLines $ findStart $ lines s
