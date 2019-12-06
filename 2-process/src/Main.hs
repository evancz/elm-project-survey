{-# LANGUAGE OverloadedStrings #-}
module Main (main) where


import Prelude hiding (lines)
import qualified Control.Monad as M
import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import Data.Binary (get, put)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified System.Directory as Dir
import System.FilePath ((</>))



-- MAIN
--
-- Create a directory called logs/ that contains all of the build.log files
-- submitted to https://github.com/evancz/elm-project-survey/issues
--
-- The names should be like 4.evancz.log to capture the issue number and OP.
--
-- This code will generate a json/ directory with individual results and a
-- composite file called results.json that has all the data in one place.
--
-- The goal is to get this into a format that will be easier to use in Elm.


main :: IO ()
main =
  do  logs <- Dir.listDirectory "logs"
      Dir.createDirectoryIfMissing True "json"
      json <- mapM convertLog logs
      LBS.writeFile "results.json" (Json.encode json)


convertLog :: String -> IO Json.Value
convertLog name =
  do  results <- Binary.decodeFile ("logs" </> name)
      let json = encode name results
      LBS.writeFile ("json" </> name) (Json.encode json)
      return json



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


newtype Time =
  Time { _milliseconds :: Integer }


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


instance Binary.Binary Time where
  get = Time <$> get
  put (Time a) = put a



-- JSON


encode :: String -> Results -> Json.Value
encode fileName (Results info deps normal devnull sizes) =
  let
    [issueNumber,user,_] = words (map (\c -> if c == '.' then ' ' else c) fileName)
  in
  Json.object
    [ "user" .= user
    , "issue" .= Json.toJSON ("https://github.com/evancz/elm-project-survey/issues/" ++ issueNumber)
    , "systemInfo" .= encodeSystemInfo info
    , "dependencies" .= encodeDeps deps
    , "buildTime" .=
        Json.object
          [ "normal" .= encodeFlagResult normal
          , "devnull" .= encodeFlagResult devnull
          ]
    , "assetSize" .= encodeSizes sizes
    ]


encodeSystemInfo :: SystemInfo -> Json.Value
encodeSystemInfo info =
  Json.object
    [ "platform" .= _platform info
    , "distro" .= _distro info
    , "release" .= _release info
    , "memory" .= _memory info
    , "manufacturer" .= _manufacturer info
    , "brand" .= _brand info
    , "speed" .= _speed info
    , "logicalCores" .= _logicalCores info
    , "physicalCores" .= _physicalCores info
    ]


encodeDeps :: Deps -> Json.Value
encodeDeps (Deps direct indirect) =
  Json.object
    [ "direct" .= direct
    , "indirect" .= indirect
    ]


encodeFlagResult :: FlagResult -> Json.Value
encodeFlagResult (FlagResult scratch incremental) =
  Json.object
    [ "fromScratch" .= scratch
    , "incremental" .= incremental
    ]


instance Json.ToJSON FileResult where
  toJSON (FileResult bytes lines time) =
    Json.object
      [ "bytes" .= bytes
      , "lines" .= lines
      , "time" .= time
      ]


instance Json.ToJSON Time where
  toJSON (Time millis) =
    Json.toJSON millis


encodeSizes :: Sizes -> Json.Value
encodeSizes (Sizes initial minified gzipped) =
  Json.object
    [ "initial" .= initial
    , "minified" .= minified
    , "gzipped" .= gzipped
    ]

