module Details (getLocalPaths) where


import Control.Monad (liftM, liftM2, liftM3, liftM5, replicateM_)
import Data.Binary (Binary, get, put, getWord8, putWord8)
import Data.Binary.Get (skip)
import qualified Data.Binary as Binary
import qualified Data.Map as Map
import Data.Word (Word8, Word16, Word64)
import System.FilePath ((</>))



-- GET LOCAL PATHS


getLocalPaths :: IO [FilePath]
getLocalPaths =
  do  details <- Binary.decodeFile ("elm-stuff" </> "0.19.1" </> "d.dat")
      return $ map _path $ Map.elems $ _locals details



-- DETAILS


data Details =
  Details
    { _outlineTime :: Integer
    , _outline :: ValidOutline
    , _buildID :: BuildID
    , _locals :: Map.Map Name Local
    , _foreigns :: Map.Map Name Foreign
    }


type BuildID = Word64


data ValidOutline
  = ValidApp (NonEmptyList SrcDir)
  | ValidPkg PkgName [Name] (Map.Map PkgName Version)


data NonEmptyList a = NonEmptyList a [a]


data SrcDir
  = AbsoluteSrcDir FilePath
  | RelativeSrcDir FilePath


data Local =
  Local
    { _path :: FilePath
    , _time :: Integer
    , _deps :: [Name]
    , _main :: Bool
    , _lastChange :: BuildID
    , _lastCompile :: BuildID
    }


data Foreign =
  Foreign PkgName [PkgName]


data PkgName = PkgName Name Name
data Name = Name Word8


data Version =
  Version Word16 Word16 Word16



-- BINARY



instance Binary Details where
  put (Details a b c d e) = put a >> put b >> put c >> put d >> put e
  get = liftM5 Details get get get get get


instance Binary ValidOutline where
  put outline =
    case outline of
      ValidApp a     -> putWord8 0 >> put a
      ValidPkg a b c -> putWord8 1 >> put a >> put b >> put c

  get =
    do  n <- getWord8
        case n of
          0 -> liftM  ValidApp get
          1 -> liftM3 ValidPkg get get get
          _ -> fail "binary encoding of ValidOutline was corrupted"


instance Binary Local where
  put (Local a b c d e f) = put a >> put b >> put c >> put d >> put e >> put f
  get =
    do  a <- get
        b <- get
        c <- get
        d <- get
        e <- get
        f <- get
        return (Local a b c d e f)


instance Binary Foreign where
  get = liftM2 Foreign get get
  put (Foreign a b) = put a >> put b


instance (Binary a) => Binary (NonEmptyList a) where
  put (NonEmptyList x xs) = put x >> put xs
  get = liftM2 NonEmptyList get get


instance Binary SrcDir where
  put outline =
    case outline of
      AbsoluteSrcDir a -> putWord8 0 >> put a
      RelativeSrcDir a -> putWord8 1 >> put a

  get =
    do  n <- getWord8
        case n of
          0 -> liftM AbsoluteSrcDir get
          1 -> liftM RelativeSrcDir get
          _ -> fail "binary encoding of SrcDir was corrupted"


instance Binary PkgName where
  get = liftM2 PkgName get get
  put (PkgName a b) = put a >> put b


instance Binary Name where
  get =
    do  size <- getWord8
        skip (fromIntegral size)
        return (Name size)

  put (Name size) =
    do  putWord8 size
        replicateM_ (fromIntegral size) (putWord8 0)


instance Binary Version where
  get =
    do  word <- getWord8
        if word == 255
          then liftM3 Version get get get
          else
            do  minor <- getWord8
                patch <- getWord8
                return (Version (fromIntegral word) (fromIntegral minor) (fromIntegral patch))

  put (Version major minor patch) =
    if major < 255 && minor < 256 && patch < 256 then
      do  putWord8 (fromIntegral major)
          putWord8 (fromIntegral minor)
          putWord8 (fromIntegral patch)
    else
      do  putWord8 255
          put major
          put minor
          put patch
