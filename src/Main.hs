{-# LANGUAGE OverloadedStrings #-}
module Main (main) where


import Prelude hiding (lines, maybe)
import Data.Aeson ((.:))
import qualified Data.Aeson as Json
import Data.Binary (get, put)
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Time.Clock as Time
import qualified Data.Time.Clock.System as Time
import Data.Word (Word16)
import qualified Details
import qualified Control.Monad as M
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.Information as Info
import qualified System.IO as IO
import qualified System.Process as Process



-- MAIN


main :: IO ()
main =
  do  verifyElmJson
      elm <- getElm
      root <- getRoot

      putStrLn "I am going to run a bunch of `elm` commands."
      putStrLn "I need `elm` to print to stdout to get realistic measurements."
      putStrLn "I will print out a nice summary after I am done!\n"

      os <- Info.os
      ram <- getRam
      cpus <- Info.cpus

      warmup elm root
      normal <- measure elm root []
      devnull <- measure elm root ["--output=/dev/null"]

      let result = Results (show os) ram (Info.showCPUs cpus) normal devnull
      Binary.encodeFile "results.dat" result
      render root result


warmup :: Command -> FilePath -> IO ()
warmup elm root =
  do  removeElmStuff
      _ <- _run elm ["make", root, "--output=/dev/null"]
      return ()


measure :: Command -> FilePath -> [String] -> IO FlagResult
measure elm root flags =
  do  removeElmStuff
      scratch <- _run elm ("make" : root : flags)
      paths <- Details.getLocalPaths
      incrementals <- traverse (measureFile elm root flags) paths
      return $ FlagResult scratch incrementals


measureFile :: Command -> FilePath -> [String] -> FilePath -> IO FileResult
measureFile elm root flags path =
  do  bytes <- fromIntegral <$> Dir.getFileSize path
      lines <- (+) 1 . BS.count 0x0A <$> BS.readFile path
      touch path
      time <- _run elm ("make" : root : flags)
      return (FileResult bytes lines time)


touch :: FilePath -> IO ()
touch path =
  do  time <- Dir.getModificationTime path
      Dir.setModificationTime path $
        Time.addUTCTime (Time.secondsToNominalDiffTime 1) time


removeElmStuff :: IO ()
removeElmStuff =
  do  exists <- Dir.doesDirectoryExist "elm-stuff"
      if exists
        then Dir.removeDirectoryRecursive "elm-stuff"
        else return ()



-- RESULTS


data Results =
  Results
    { _os :: String
    , _ram :: Word16
    , _cpus :: String
    , _normal :: FlagResult
    , _devnull :: FlagResult
    }


data FlagResult =
  FlagResult
    { _scratch :: Time
    , _incremental :: [FileResult]
    }


data FileResult =
  FileResult
    { _bytes :: Int
    , _lines :: Int
    , _time :: Time
    }


instance Binary.Binary Results where
  get = M.liftM5 Results get get get get get
  put (Results a b c d e) = put a >> put b >> put c >> put d >> put e


instance Binary.Binary FlagResult where
  get = M.liftM2 FlagResult get get
  put (FlagResult a b) = put a >> put b


instance Binary.Binary FileResult where
  get = M.liftM3 FileResult get get get
  put (FileResult a b c) = put a >> put b >> put c



-- TIME


newtype Time =
  Time { _milliseconds :: Integer }


diff :: Time.SystemTime -> Time.SystemTime -> Time
diff (Time.MkSystemTime startSeconds startNanos)  (Time.MkSystemTime endSeconds endNanos) =
  let
    start = 1000 * fromIntegral startSeconds + fromIntegral (div startNanos 1000000)
    end   = 1000 * fromIntegral endSeconds   + fromIntegral (div endNanos   1000000)
  in
  Time (end - start)


instance Binary.Binary Time where
  get = Time <$> get
  put (Time a) = put a



-- VERIFY ELM JSON


verifyElmJson :: IO ()
verifyElmJson =
  do  exists <- Dir.doesFileExist "elm.json"

      if exists
        then return ()
        else failure "Cannot find elm.json in this directory. Move to the project root."

      json <- LBS.readFile "elm.json"

      case Json.decode' json of
        Nothing ->
          failure "No \"elm-version\" field in your elm.json file."

        Just (Version vsn) ->
          if vsn == "0.19.1"
            then return ()
            else failure $ "This project uses Elm " ++ vsn ++ ", but I want to benchmark 0.19.1\nProject must be upgraded before I can run!"


newtype Version =
  Version String


instance Json.FromJSON Version where
  parseJSON = Json.withObject "outline" $
    \obj -> Version <$> obj .: "elm-version"



-- GET ELM


newtype Command =
  Command { _run :: [String] -> IO Time }


getElm :: IO Command
getElm =
  do  maybeElm <- Dir.findExecutable "elm"
      elm <- check maybeElm "Could not find `elm` on your PATH."
      vsn <- Process.readProcess elm ["--version"] ""

      if trim vsn == "0.19.1"
        then return ()
        else failure ("Found wrong version of `elm` on your PATH.\nFound " ++ vsn ++ " but need 0.19.1 for this data collection.")

      return $ Command $ \args ->
        do  let process = Process.proc elm args
            start <- Time.getSystemTime
            Process.withCreateProcess process $ \_ _ _ handle ->
              do  exit <- Process.waitForProcess handle
                  end <- Time.getSystemTime
                  case exit of
                    Exit.ExitSuccess ->
                      return (diff start end)

                    Exit.ExitFailure _ ->
                      failure $ "Ran into a problem when running:\n\n    elm " ++ unwords args ++ "\n\nYour project must compile successfully for these benchmarks to work."


trim :: String -> String
trim str =
  reverse $ dropWhile Char.isSpace $ reverse $ dropWhile Char.isSpace str



-- GET ROOT


getRoot :: IO FilePath
getRoot =
  do  args <- Env.getArgs
      case args of
        [path] ->
          do  exists <- Dir.doesFileExist path
              if exists
                then return path
                else failure $ "I cannot find a file named " ++ path

        _ ->
          failure "Expecting the root of your application as an argument. For example:\n\n    ./benchmark src/Main.elm\n\nBut with whatever file is the main entrypoint for your application."



-- GET RAM


getRam :: IO Word16
getRam =
  do  IO.hPutStr IO.stdout "Before we start, how many GB or RAM do you have? "
      IO.hFlush IO.stdout
      n <- IO.readLn
      putStrLn ""
      return n



-- RENDER


render :: FilePath -> Results -> IO ()
render root (Results os ram cpus normal devnull@(FlagResult _ fs)) =
  do  putStrLn "\n-- OVERVIEW -----------------------------------------------\n"
      putStrLn $ "OS:  " ++ os
      putStrLn $ "RAM: " ++ show ram ++ "GB"
      putStrLn $ "CPU: " ++ cpus

      putStrLn $ "PROJECT: " ++ show (length fs) ++ " files, " ++ show (sum (map _lines fs)) ++ " lines\n"

      renderResults normal ["elm","make",root]
      renderResults devnull ["elm","make",root,"--output=/dev/null"]

      putStrLn "-----------------------------------------------------------"
      putStrLn "Does everything look alright with these numbers?"
      putStrLn "If so, please share results.dat in the Discourse thread.\n"


renderResults :: FlagResult -> [String] -> IO ()
renderResults (FlagResult scratch incrementals) parts =
  let
    times = List.sort (map (_milliseconds . _time) incrementals)
    fresh = show (_milliseconds scratch)
    minIn = show (head times)
    medIn = show (median times)
    maxIn = show (last times)
    n     = maximum (map length [fresh, minIn, medIn, maxIn])
  in
  do  putStrLn $ "COMMAND: " ++ unwords parts ++ "\n"
      putStrLn $ pad n fresh ++ "ms  -- from scratch"
      putStrLn $ pad n maxIn ++ "ms  -- worst incremental"
      putStrLn $ pad n medIn ++ "ms  -- median incremental"
      putStrLn $ pad n minIn ++ "ms  -- best incremental\n"


pad :: Int -> String -> String
pad width str =
  replicate (2 + width - length str) ' ' ++ str


median :: [Integer] -> Integer
median times =
  let
    len = length times
    mid = div len 2
    idx = (List.!!)
  in
  if mod len 2 == 1
    then idx times mid
    else div (idx times mid + idx times (mid + 1)) 2



-- FAILURE


failure :: String -> IO a
failure message =
  do  IO.hPutStrLn IO.stderr ("ERROR: " ++ message)
      Exit.exitFailure


check :: Maybe a -> String -> IO a
check maybe message =
  case maybe of
    Nothing -> failure message
    Just a  -> return a
