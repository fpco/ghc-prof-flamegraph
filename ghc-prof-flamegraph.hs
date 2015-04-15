import           Data.List (intercalate)
import           Data.Char (isSpace)

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

main :: IO ()
main = do
  s <- getContents
  -- 27 is the number of lines before the actual data starts.
  putStr $ unlines $ processLines $ drop 27 $ lines s
