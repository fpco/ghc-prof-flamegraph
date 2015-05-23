{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.List (intercalate)
import qualified ProfFile as Prof

generateFrames :: [Prof.Line] -> [String]
generateFrames lines0 =
  let (entries, frames) = go [] lines0
      unknown = 1000 - entries
  in if unknown > 0
    then ("UNKNOWN " ++ show unknown) : frames
    else frames
  where
    go :: [String] -> [Prof.Line] -> (Int, [String])
    go _stack [] =
      (0, [])
    go stack (line : lines') =
      let entries :: Int = round $ 10 * (Prof.lInheritedTime line)
          symbol = Prof.lModule line ++ "." ++ Prof.lCostCentre line
          frame = intercalate ";" (reverse (symbol : stack)) ++ " " ++ show entries
          (childrenEntries, childrenFrames) = go (symbol : stack) (Prof.lChildren line)
          (restEntries, restFrames) = go stack lines'
      in (entries + childrenEntries + restEntries, frame : childrenFrames ++ restFrames)

main :: IO ()
main = do
  s <- getContents
  case Prof.parse s of
    Left err -> error err
    Right ls -> putStr $ unlines $ generateFrames ls
