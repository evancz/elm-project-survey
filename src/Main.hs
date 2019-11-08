{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main (main) where


import Prelude hiding (lines, maybe)
import Data.Aeson ((.:))
import qualified Data.Aeson as Json
import Data.Binary (get, put)
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Time.Clock as Time
import qualified Data.Time.Clock.System as Time
import qualified Details
import Control.Applicative ((<|>))
import qualified Control.Exception as E
import qualified Control.Monad as M
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.Process as Process
import Text.RawString.QQ (r)



-- MAIN


main :: IO ()
main =
  do  deps  <- verifyElmJson
      elm   <- getElm
      node  <- getNode
      roots <- getRoots

      warmup elm roots
      normal <- measure elm roots []
      devnull <- measure elm roots ["--output=/dev/null"]
      (info,sizes) <- runJS elm roots node

      let results = Results info deps normal devnull sizes
      Binary.encodeFile "build.log" results
      render roots results


warmup :: Elm -> [FilePath] -> IO ()
warmup elm roots =
  do  removeElmStuff
      _ <- _run elm ("make" : roots ++ ["--output=/dev/null"])
      return ()


measure :: Elm -> [FilePath] -> [String] -> IO FlagResult
measure elm roots flags =
  do  removeElmStuff
      scratch <- _run elm ("make" : roots ++ flags)
      paths <- Details.getLocalPaths
      incrementals <- traverse (measureFile elm roots flags) paths
      return $ FlagResult scratch incrementals


measureFile :: Elm -> [FilePath] -> [String] -> FilePath -> IO FileResult
measureFile elm roots flags path =
  do  bytes <- fromIntegral <$> Dir.getFileSize path
      lines <- (+) 1 . BS.count 0x0A <$> BS.readFile path
      touch path
      time <- _run elm ("make" : roots ++ flags)
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
    { _info :: SystemInfo
    , _deps :: Deps
    , _normal :: FlagResult
    , _devnull :: FlagResult
    , _sizes :: Sizes
    }


data SystemInfo =
  SystemInfo
    { _platform :: String
    , _distro :: String
    , _release :: String
    , _memory :: Integer
    , _manufacturer :: String
    , _brand :: String
    , _speed :: String
    , _logicalCores :: Int
    , _physicalCores :: Int
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
  get = M.liftM5 Results get get get get get
  put (Results a b c d e) = put a >> put b >> put c >> put d >> put e


instance Binary.Binary SystemInfo where
  get = SystemInfo <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get
  put (SystemInfo a b c d e f g h i) = put a >> put b >> put c >> put d >> put e >> put f >> put g >> put h >> put i


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



-- GET ROOTS


getRoots :: IO [FilePath]
getRoots =
  do  args <- Env.getArgs
      case args of
        [] ->
          failure
            "When you call `elm make` during development, what files do you give it?\n\
            \If you call `elm make` with a single entry point, tell me which one:\n\
            \\n\
            \     # elm make src/Main.elm --debug\n\
            \    ./measure-elm-make src/Main.elm\n\
            \\n\
            \If you run `elm make` with multiple entry points during development, you\n\
            \can tell me that as well:\n\
            \\n\
            \     # elm make src/Home.elm src/Settings.elm --output=elm.js\n\
            \    ./measure-elm-make src/Home.elm src/Settings.elm\n\
            \\n\
            \DO NOT TRY TO MAXIMIZE PROJECT SIZE BY GIVING EXTRA FILES. The goal is to\n\
            \measure day-to-day compile times. Not what happens on CI or what could happen\n\
            \if your development setup was different. ONLY GIVE THE FILES YOU NORMALLY GIVE."

        _ ->
          do  mapM_ checkRoot args
              return args


checkRoot :: FilePath -> IO ()
checkRoot path =
  do  exists <- Dir.doesFileExist path
      if exists
        then return ()
        else
          do  prog <- Env.getProgName
              failure $
                "All the arguments to " ++ prog ++ " must be Elm paths.\n\
                \I cannot find a file named " ++ path ++ " though."



-- GET NODE


getNode :: IO FilePath
getNode =
  do  maybeNode1 <- Dir.findExecutable "node"
      maybeNode2 <- Dir.findExecutable "nodejs"
      check (maybeNode1 <|> maybeNode2) "Could not find `node` or `nodejs` on your PATH.\nI need it to measure minification sizes."



-- RUN JS


runJS :: Elm -> [FilePath] -> FilePath -> IO (SystemInfo, Sizes)
runJS elm roots node =
  do  let dir = "elm-stuff" </> "0.19.1" </> "temporary"
      let path = dir </> "elm.js"

      _ <- _run elm ("make" : roots ++ ["--output=" ++ path, "--optimize"])

      putStrLn "Working on estimating asset sizes."

      withinTempDir dir $
        do  BS.writeFile "index.js" indexJS
            BS.writeFile "package.json" packageJSON
            BS.writeFile "package-lock.json" packageLockJSON
            _code <- Process.system "npm install"
            json <- Process.readProcess node ["index.js"] ""
            putStrLn json
            case Json.eitherDecode' (B.toLazyByteString (B.stringUtf8 json)) of
              Left message ->
                failure $ "Something went wrong when trying to collect system data and asset sizes:\n\n" ++ message

              Right (JSOutput info sizes) ->
                return (info, sizes)


withinTempDir :: FilePath -> IO a -> IO a
withinTempDir dir work =
  E.bracket
    (Dir.createDirectoryIfMissing True dir)
    (\_ -> Dir.removeDirectoryRecursive dir)
    (\_ -> Dir.withCurrentDirectory dir work)


data JSOutput =
  JSOutput SystemInfo Sizes


instance Json.FromJSON JSOutput where
  parseJSON =
    Json.withObject "output" $ \obj ->
      JSOutput
        <$> obj .: "info"
        <*> obj .: "sizes"


instance Json.FromJSON SystemInfo where
  parseJSON =
    Json.withObject "output" $ \obj ->
      SystemInfo
        <$> obj .: "platform"
        <*> obj .: "distro"
        <*> obj .: "release"
        <*> obj .: "memory"
        <*> obj .: "manufacturer"
        <*> obj .: "brand"
        <*> obj .: "speed"
        <*> obj .: "logicalCores"
        <*> obj .: "physicalCores"


instance Json.FromJSON Sizes where
  parseJSON =
    Json.withObject "sizes" $ \obj ->
      Sizes
        <$> obj .: "initial"
        <*> obj .: "minified"
        <*> obj .: "gzipped"



-- RENDER


render :: [FilePath] -> Results -> IO ()
render roots (Results info deps normal devnull@(FlagResult _ fs) sizes) =
  do  putStrLn "\n-- OVERVIEW -----------------------------------------------\n"
      putStrLn $ "    OS:  " ++ _distro info ++ " " ++ _release info
      putStrLn $ "    RAM: " ++ show (round (fromInteger (_memory info) / 1073741824 :: Double) :: Integer) ++ "GB"
      putStrLn $ "    CPU: " ++ _manufacturer info ++ " " ++ _brand info ++ " @ " ++ _speed info ++ "GHz, " ++ show (_physicalCores info) ++ " physical cores"

      putStrLn $ "    PROJECT: "
        ++ show (length fs) ++ " files, "
        ++ show (sum (map _lines fs)) ++ " lines, "
        ++ show (Map.size (_direct deps)) ++ " direct deps, "
        ++ show (Map.size (_indirect deps)) ++ " indirect deps"

      putStrLn $ "    ASSET SIZE: "
        ++ show (_initial  sizes) ++ " bytes -> "
        ++ show (_minified sizes) ++ " bytes (minified) -> "
        ++ show (_gzipped  sizes) ++ " bytes (gzipped)\n"

      renderResults normal ("elm" : "make" : roots)
      renderResults devnull ("elm" : "make" : roots ++ ["--output=/dev/null"])

      putStrLn "-----------------------------------------------------------"
      putStrLn "Does everything look alright with these numbers?"
      putStrLn ""
      putStrLn "If so, please do the following steps:"
      putStrLn ""
      putStrLn "  1. Go to https://github.com/evancz/elm-project-survey/issues/new"
      putStrLn "  2. Use a title like \"Build times - Windows - 50622 lines\""
      putStrLn "  3. Copy the OVERVIEW information into the body of the issue."
      putStrLn "  4. MOST IMPORTANT! Attach the generated build.log file to the issue."
      putStrLn ""
      putStrLn "We can figure out what the data means from there!"



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
  do  putStrLn $ "    COMMAND: " ++ unwords parts ++ "\n"
      putStrLn $ pad n fresh ++ "ms  -- from scratch"
      putStrLn $ pad n maxIn ++ "ms  -- worst incremental"
      putStrLn $ pad n medIn ++ "ms  -- median incremental"
      putStrLn $ pad n minIn ++ "ms  -- best incremental\n"


pad :: Int -> String -> String
pad width str =
  replicate (6 + width - length str) ' ' ++ str


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
var si = require('systeminformation');
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

si.osInfo(function(info) {
  si.mem(function(memory) {
    si.cpu(function(cpu) {
      console.log(JSON.stringify({
        sizes: {
          initial: fs.statSync("elm.js").size,
          minified: Buffer.from(phase2.code, 'utf8').length,
          gzipped: zlib.gzipSync(phase2.code).length
        },
        info: {
          platform: info.platform,
          distro: info.distro,
          release: info. release,
          memory: memory.total,
          manufacturer: cpu.manufacturer,
          brand: cpu.brand,
          speed: cpu.speed,
          logicalCores: cpu.cores,
          physicalCores: cpu.physicalCores
        }
      }));
    });
  });
});

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
    "uglify-js": "3.6.7",
    "systeminformation": "4.14.17"
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
    "systeminformation": {
      "version": "4.14.17",
      "resolved": "https://registry.npmjs.org/systeminformation/-/systeminformation-4.14.17.tgz",
      "integrity": "sha512-CQbT5vnkqNb3JNl41xr8sYA8AX7GoaWP55/jnmFNQY0XQmUuoFshSNUkCkxiDdEC1qu2Vg9s0jR6LLmVSmNJUw=="
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
