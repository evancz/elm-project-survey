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
import qualified Data.Time.Clock as Time
import qualified Data.Time.Clock.System as Time
import Data.Word (Word16)
import qualified Control.Monad as M
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Exit as Exit
import System.FilePath ((</>))
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

      normal <- measure elm root []
      devnull <- measure elm root ["--output=NUL"]
      optimize <- measure elm root ["--optimize"]

      let result = Results (show os) ram (Info.showCPUs cpus) normal devnull optimize
      Binary.encodeFile "results.dat" result
      render result


measure :: Command -> FilePath -> [String] -> IO FlagResult
measure elm root flags =
  do  removeElmStuff
      scratch <- _run elm ("make" : root : flags)
      files <- getFiles
      incrementals <- traverse (measureFile elm root flags) files
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
    , _optimize :: FlagResult
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
  get = Results <$> get <*> get <*> get <*> get <*> get <*> get
  put (Results a b c d e f) = put a >> put b >> put c >> put d >> put e >> put f


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



-- GET FILES


getFiles :: IO [FilePath]
getFiles =
  do  details <- Binary.decodeFile ("elm-stuff" </> "0.19.1" </> "d.dat")
      return (error "TODO" (details :: ()))



-- RENDER


render :: Results -> IO ()
render (Results os ram cpus _normal _devnull _optimize) =
  do  putStr "\ESC[2J"
      putStrLn "-- OVERVIEW -----------------------------------------------\n"
      putStrLn $ "OS:  " ++ os
      putStrLn $ "CPU: " ++ cpus
      putStrLn $ "RAM: " ++ show ram ++ "GB\n"
      putStrLn "This data (and more) is saved in the results.dat file.\n"
      putStrLn "-----------------------------------------------------------\n"
      putStrLn "Send the results.dat file TODO"



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
