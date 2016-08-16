{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Applicative ((<*>))
import           Data.Functor ((<$>))
import           Data.List (intercalate)
import           Data.Monoid ((<>))
import qualified Options.Applicative as Opts
import qualified ProfFile as Prof

data Options = Options
  { optionsAlloc :: Bool
  } deriving (Eq, Show)

optionsParser :: Opts.Parser Options
optionsParser = Options
  <$> Opts.switch (Opts.long "alloc" <> Opts.help "Uses the allocation measurements instead of time measurements")

generateFrames :: Options -> [Prof.Line] -> [String]
generateFrames options lines0 =
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
      let entries :: Int = round $ 10 * (individualMeasure line)
          symbol = Prof.lModule line ++ "." ++ Prof.lCostCentre line
          frame = intercalate ";" (reverse (symbol : stack)) ++ " " ++ show entries
          (childrenEntries, childrenFrames) = go (symbol : stack) (Prof.lChildren line)
          (restEntries, restFrames) = go stack lines'
      in (entries + childrenEntries + restEntries, frame : childrenFrames ++ restFrames)

    individualMeasure = if optionsAlloc options
      then Prof.lIndividualAlloc
      else fromIntegral . Prof.lEntries

main :: IO ()
main = do
  options <- Opts.execParser $
    Opts.info (Opts.helper <*> optionsParser) Opts.fullDesc
  s <- getContents
  case Prof.parse s of
    Left err -> error err
    Right ls -> putStr $ unlines $ generateFrames options ls
