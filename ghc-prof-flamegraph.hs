{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Applicative ((<*>), (<|>), optional, many, pure)
import           Data.Foldable (traverse_)
import           Data.Functor ((<$>))
import           Data.List (intercalate)
import           Data.Monoid ((<>))
import qualified Options.Applicative as Opts
import qualified ProfFile as Prof
import           System.Exit (ExitCode(..), exitFailure)
import           System.FilePath (replaceExtension)
import           System.IO (stderr, stdout, hPutStrLn, hPutStr, hGetContents, IOMode(..), hClose, openFile)
import           System.Process (proc, createProcess, CreateProcess(..), StdStream(..), waitForProcess)

data Options = Options
  { optionsReportType      :: ReportType
  , optionsProfFile        :: Maybe FilePath
  , optionsOutputFile      :: Maybe FilePath
  , optionsFlamegraphFlags :: [String]
  } deriving (Eq, Show)

data ReportType = Alloc   -- ^ Report allocations, percent
                | Entries -- ^ Report entries, number
                | Time    -- ^ Report time spent in closure, percent
                | Ticks   -- ^ Report ticks, number
                | Bytes   -- ^ Report bytes allocated, number
                deriving (Eq, Show)

optionsParser :: Opts.Parser Options
optionsParser = Options
  <$> (Opts.flag' Alloc (Opts.long "alloc" <> Opts.help "Uses the allocation measurements instead of time measurements")
       <|> Opts.flag' Entries (Opts.long "entry" <> Opts.help "Uses entries the measurements instead of time measurements")
       <|> Opts.flag' Bytes   (Opts.long "bytes" <> Opts.help "Memory measurements in bytes (+RTS -P -RTS)")
       <|> Opts.flag' Ticks (Opts.long "ticks" <> Opts.help "Time measurements in ticks (+RTS -P -RTS)")
       <|> Opts.flag  Time Time (Opts.long "time" <> Opts.help "Uses time measurements"))
  <*> optional
        (Opts.strArgument
          (Opts.metavar "PROF-FILE" <>
           Opts.help "Profiling output to format as flame graph"))
  <*> optional
        (Opts.strOption
          (Opts.short 'o' <>
           Opts.long "output" <>
           Opts.metavar "SVG-FILE" <>
           Opts.help "Optional output file"))
  <*> many
        (Opts.strOption
          (Opts.long "flamegraph-option" <>
           Opts.metavar "STR" <>
           Opts.help "Options to pass to flamegraph.pl"))

checkNames :: ReportType -> [String] -> Maybe String
checkNames Alloc   _ = Nothing
checkNames Entries _ = Nothing
checkNames Time    _ = Nothing
checkNames Ticks   n
  | "ticks" `elem` n = Nothing
  | otherwise        = Just "No ticks information, please run program with +RTS -P"
checkNames Bytes n
  | "bytes" `elem` n = Nothing
  | otherwise        = Just "No ticks information, please run program with +RTS -P"

normalize :: ReportType -> Double -> Int
normalize Alloc = round . (10 *)
normalize Time  = round . (10 *)
normalize _     = round

addUnknown :: ReportType -> (Int, [String]) -> [String]
addUnknown Time   = \(entries, frames) ->
  let unknown = 1000 - entries
  in if unknown > 0
     then ("UNKNOWN " ++ show unknown) : frames
     else frames
addUnknown Alloc   = \(entries, frames) ->
  let unknown = 1000 - entries
  in if unknown > 0
     then ("UNKNOWN " ++ show unknown) : frames
     else frames
addUnknown _ = snd

generateFrames :: Options -> [Prof.Line] -> [String]
generateFrames options lines0 = addUnknown (optionsReportType options) $ go [] lines0
  where
    go :: [String] -> [Prof.Line] -> (Int, [String])
    go _stack [] =
      (0, [])
    go stack (line : lines') =
      let entries = normalize (optionsReportType options) (individualMeasure line)
          symbol = Prof.lModule line ++ "." ++ Prof.lCostCentre line
          frame = intercalate ";" (reverse (symbol : stack)) ++ " " ++ show entries
          (childrenEntries, childrenFrames) = go (symbol : stack) (Prof.lChildren line)
          (restEntries, restFrames) = go stack lines'
      in (entries + childrenEntries + restEntries, frame : childrenFrames ++ restFrames)

    individualMeasure = case optionsReportType options of
      Alloc   -> Prof.lIndividualAlloc
      Time    -> Prof.lIndividualTime
      Entries -> fromIntegral . Prof.lEntries
      Ticks   -> fromIntegral . Prof.lTicks
      Bytes   -> fromIntegral . Prof.lBytes

main :: IO ()
main = do
  options <- Opts.execParser $
    Opts.info (Opts.helper <*> optionsParser) Opts.fullDesc
  s <- maybe getContents readFile $ optionsProfFile options
  case Prof.parse s of
    Left err -> error err
    Right (names, ls) ->
      case checkNames (optionsReportType options) names of
        Just problem -> do
          hPutStrLn stderr problem
          exitFailure
        Nothing      -> do
          let flamegraphProc = (proc "flamegraph.pl" (optionsFlamegraphFlags options))
                { std_in  = CreatePipe
                , std_out = CreatePipe
                , std_err = Inherit
                }
          (outputHandle, outputFileName, closeOutputHandle) <-
            case (optionsOutputFile options, optionsProfFile options) of
              (Just path, _)         -> do
                h <- openFile path WriteMode
                pure (h, Just path, hClose h)
              (Nothing,   Just path) -> do
                let path' = path `replaceExtension` "svg"
                h <- openFile path' WriteMode
                pure (h, Just path', hClose h)
              _                      ->
                pure (stdout, Nothing, pure ())
          (Just input, Just flamegraphResult, Nothing, procHandle) <- createProcess flamegraphProc
          traverse_ (hPutStrLn input) $ generateFrames options ls
          hClose input
          hGetContents flamegraphResult >>= hPutStr outputHandle
          exitCode <- waitForProcess procHandle
          closeOutputHandle
          case exitCode of
            ExitSuccess   ->
              case outputFileName of
                Nothing   -> pure ()
                Just path -> putStrLn $ "Output written to " <> path
            ExitFailure{} ->
              hPutStrLn stderr $ "Call to flamegraph.pl failed"
