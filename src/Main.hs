{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
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
import qualified Data.Map as Map
import qualified Data.Time.Clock as Time
import qualified Data.Time.Clock.System as Time
import Data.Word (Word16)
import qualified Details
import Control.Applicative ((<|>))
import qualified Control.Exception as E
import qualified Control.Monad as M
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.Information as Info
import qualified System.IO as IO
import qualified System.Process as Process
import Text.RawString.QQ (r)



-- MAIN


main :: IO ()
main =
  do  deps <- verifyElmJson
      elm <- getElm
      root <- getRoot

      npm <- getNpm
      node <- getNode

      putStrLn "I am going to run a bunch of `elm` commands."
      putStrLn "I need `elm` to print to stdout to get realistic measurements."
      putStrLn "I will print out a nice summary after I am done!\n"

      os <- Info.os
      ram <- getRam
      cpus <- Info.cpus

      warmup elm root
      warmup elm root
      normal <- measure elm root []
      devnull <- measure elm root ["--output=/dev/null"]
      sizes <- getSizes elm root npm node

      let result = Results (show os) ram (Info.showCPUs cpus) deps normal devnull sizes
      Binary.encodeFile "summary.dat" result
      render root result


warmup :: Elm -> FilePath -> IO ()
warmup elm root =
  do  removeElmStuff
      _ <- _run elm ["make", root, "--output=/dev/null"]
      return ()


measure :: Elm -> FilePath -> [String] -> IO FlagResult
measure elm root flags =
  do  removeElmStuff
      scratch <- _run elm ("make" : root : flags)
      paths <- Details.getLocalPaths
      incrementals <- traverse (measureFile elm root flags) paths
      return $ FlagResult scratch incrementals


measureFile :: Elm -> FilePath -> [String] -> FilePath -> IO FileResult
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
    , _deps :: Deps
    , _normal :: FlagResult
    , _devnull :: FlagResult
    , _sizes :: Sizes
    }


data Deps =
  Deps
    { _direct :: Map.Map String String
    , _indirect :: Map.Map String String
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


data Sizes =
  Sizes
    { _initial :: Int
    , _minified :: Int
    , _gzipped :: Int
    }


instance Binary.Binary Results where
  get = Results <$> get <*> get <*> get <*> get <*> get <*> get <*> get
  put (Results a b c d e f g) = put a >> put b >> put c >> put d >> put e >> put f >> put g


instance Binary.Binary Deps where
  get = M.liftM2 Deps get get
  put (Deps a b) = put a >> put b


instance Binary.Binary FlagResult where
  get = M.liftM2 FlagResult get get
  put (FlagResult a b) = put a >> put b


instance Binary.Binary FileResult where
  get = M.liftM3 FileResult get get get
  put (FileResult a b c) = put a >> put b >> put c


instance Binary.Binary Sizes where
  get = M.liftM3 Sizes get get get
  put (Sizes a b c) = put a >> put b >> put c



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


verifyElmJson :: IO Deps
verifyElmJson =
  do  exists <- Dir.doesFileExist "elm.json"

      if exists
        then return ()
        else failure "Cannot find elm.json in this directory. Move to the project root."

      json <- LBS.readFile "elm.json"

      case Json.decode' json of
        Nothing ->
          failure "No \"elm-version\" field in your elm.json file."

        Just (Outline vsn deps) ->
          if vsn == "0.19.1"
            then return deps
            else failure $ "This project uses Elm " ++ vsn ++ ", but I want to benchmark 0.19.1\nProject must be upgraded before I can run!"


data Outline =
  Outline String Deps


instance Json.FromJSON Outline where
  parseJSON =
    Json.withObject "outline" $ \obj ->
      Outline
        <$> obj .: "elm-version"
        <*> obj .: "dependencies"


instance Json.FromJSON Deps where
  parseJSON =
    Json.withObject "dependencies" $ \obj ->
      Deps
        <$> obj .: "direct"
        <*> obj .: "indirect"



-- GET ELM


newtype Elm =
  Elm { _run :: [String] -> IO Time }


getElm :: IO Elm
getElm =
  do  maybeElm <- Dir.findExecutable "elm"
      elm <- check maybeElm "Could not find `elm` on your PATH."
      vsn <- Process.readProcess elm ["--version"] ""

      if trim vsn == "0.19.1"
        then return ()
        else failure ("Found wrong version of `elm` on your PATH.\nFound " ++ vsn ++ " but need 0.19.1 for this data collection.")

      return $ Elm $ \args ->
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
  do  IO.hPutStr IO.stdout "Before we start, how many GB of RAM do you have? "
      IO.hFlush IO.stdout
      n <- IO.readLn
      putStrLn ""
      return n



-- GET NPM


getNpm :: IO FilePath
getNpm =
  do  maybeNpm <- Dir.findExecutable "npm"
      check maybeNpm "Could not find `npm` on your PATH.\nI need it to measure minification sizes."


getNode :: IO FilePath
getNode =
  do  maybeNode1 <- Dir.findExecutable "node"
      maybeNode2 <- Dir.findExecutable "nodejs"
      check (maybeNode1 <|> maybeNode2) "Could not find `node` or `nodejs` on your PATH.\nI need it to measure minification sizes."



-- GET SIZES


getSizes :: Elm -> FilePath -> FilePath -> FilePath -> IO Sizes
getSizes elm root npm node =
  do  let dir = "elm-stuff" </> "0.19.1" </> "temporary"
      let path = dir </> "elm.js"

      _ <- _run elm ["make", root, "--output=" ++ path, "--optimize"]

      putStrLn "Working on estimating asset sizes."

      withinTempDir dir $
        do  BS.writeFile "index.js" indexJS
            BS.writeFile "package.json" packageJSON
            BS.writeFile "package-lock.json" packageLockJSON
            _ <- Process.readProcess npm ["install"] ""
            o <- Process.readProcess node ["index.js"] ""
            let [inital,minified,gzipped] = read o
            return (Sizes inital minified gzipped)


withinTempDir :: FilePath -> IO a -> IO a
withinTempDir dir work =
  E.bracket
    (Dir.createDirectoryIfMissing True dir)
    (\_ -> Dir.removeDirectoryRecursive dir)
    (\_ -> Dir.withCurrentDirectory dir work)



-- RENDER


render :: FilePath -> Results -> IO ()
render root (Results os ram cpus (Deps direct indirect) normal devnull@(FlagResult _ fs) (Sizes initial minified gzipped)) =
  do  putStrLn "\n-- OVERVIEW -----------------------------------------------\n"
      putStrLn $ "OS:  " ++ os
      putStrLn $ "RAM: " ++ show ram ++ "GB"
      putStrLn $ "CPU: " ++ cpus

      putStrLn $ "PROJECT: "
        ++ show (length fs) ++ " files, "
        ++ show (sum (map _lines fs)) ++ " lines, "
        ++ show (Map.size direct) ++ " direct deps, "
        ++ show (Map.size indirect) ++ " indirect deps\n"

      putStrLn $ "ASSET SIZE: "
        ++ show initial  ++ " bytes -> "
        ++ show minified ++ " bytes (minified) -> "
        ++ show gzipped  ++ " bytes (gzipped)\n"

      renderResults normal ["elm","make",root]
      renderResults devnull ["elm","make",root,"--output=/dev/null"]

      putStrLn "-----------------------------------------------------------"
      putStrLn "Does everything look alright with these numbers?"
      putStrLn "If so, please share summary.dat in the Discourse thread.\n"


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



indexJS :: BS.ByteString
indexJS = [r|

var fs = require('fs');
var UglifyJS = require("uglify-js");
var zlib = require('zlib');


// MINIFY

var phase1 = UglifyJS.minify(fs.readFileSync("elm.js", "utf8"), {
  compress: {
    pure_funcs: ["F2","F3","F4","F5","F6","F7","F8","F9","A2","A3","A4","A5","A6","A7","A8","A9"],
    pure_getters: true,
    keep_fargs: false,
    unsafe_comps: true,
    unsafe: true
  },
  mangle: false
});

var phase2 = UglifyJS.minify(phase1.code, {
  compress: false,
  mangle: true
});


// OUTPUT

var initialSize = fs.statSync("elm.js").size;
var minifiedSize = Buffer.from(phase2.code, 'utf8').length;
var gzippedSize = zlib.gzipSync(phase2.code).length;

console.log([initialSize, minifiedSize, gzippedSize]);

|]


packageJSON :: BS.ByteString
packageJSON = [r|
{
  "name": "minify",
  "version": "1.0.0",
  "repository": "https://github.com/AUTHOR/PROJECT",
  "description": "estimate asset sizes",
  "main": "index.js",
  "license": "BSD-3-Clause",
  "dependencies": {
    "uglify-js": "3.6.7"
  }
}
|]


packageLockJSON :: BS.ByteString
packageLockJSON = [r|
{
  "name": "minify",
  "version": "1.0.0",
  "lockfileVersion": 1,
  "requires": true,
  "dependencies": {
    "commander": {
      "version": "2.20.3",
      "resolved": "https://registry.npmjs.org/commander/-/commander-2.20.3.tgz",
      "integrity": "sha512-GpVkmM8vF2vQUkj2LvZmD35JxeJOLCwJ9cUkugyk2nuhbv3+mJvpLYYt+0+USMxE+oj+ey/lJEnhZw75x/OMcQ=="
    },
    "source-map": {
      "version": "0.6.1",
      "resolved": "https://registry.npmjs.org/source-map/-/source-map-0.6.1.tgz",
      "integrity": "sha512-UjgapumWlbMhkBgzT7Ykc5YXUT46F0iKu8SGXq0bcwP5dz/h0Plj6enJqjz1Zbq2l5WaqYnrVbwWOWMyF3F47g=="
    },
    "uglify-js": {
      "version": "3.6.7",
      "resolved": "https://registry.npmjs.org/uglify-js/-/uglify-js-3.6.7.tgz",
      "integrity": "sha512-4sXQDzmdnoXiO+xvmTzQsfIiwrjUCSA95rSP4SEd8tDb51W2TiDOlL76Hl+Kw0Ie42PSItCW8/t6pBNCF2R48A==",
      "requires": {
        "commander": "~2.20.3",
        "source-map": "~0.6.1"
      }
    }
  }
}
|]
