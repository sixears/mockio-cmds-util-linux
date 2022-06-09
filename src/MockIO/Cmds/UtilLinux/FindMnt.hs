{-| Interface to the @findmnt@ cmdline utility. -}
module MockIO.Cmds.UtilLinux.FindMnt
  ( FindMntData, FindMntDatum, FSType(..)
  , filesystems, findmnt, findMntSource, findMntTarget
  , source, target, fsroot, fstype, size, used, avail, label, partlabel
  )
where

import Base1T

-- aeson -------------------------------

import Data.Aeson        ( FromJSON( parseJSON ), Options( fieldLabelModifier
                                                         , rejectUnknownFields )
                         , Value( String )
                         , defaultOptions, genericParseJSON
                         )
import Data.Aeson.Types  ( prependFailure, typeMismatch )

-- base --------------------------------

import Control.Monad    ( fail )
import Data.Char        ( isSpace )
import Data.List        ( find )
import Data.Monoid      ( Monoid( mempty ) )
import GHC.Generics     ( Generic )

-- bytestring --------------------------

import Data.ByteString.Lazy  ( fromStrict )

-- fpath -------------------------------

import FPath.AbsDir            ( AbsDir, parseAbsDirP )
import FPath.Error.FPathError  ( AsFPathError, FPathError )

-- lens --------------------------------

import Control.Lens.Getter  ( view )

-- log-plus ----------------------------

import Log  ( Log )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity )

-- mockio ------------------------------

import MockIO.DoMock  ( HasDoMock( doMock ) )

-- mockio-log --------------------------

import MockIO.IOClass      ( IOClass( IOCmdR ), ioClass )
import MockIO.Log          ( logResult )
import MockIO.MockIOClass  ( MockIOClass )

-- mockio-plus -------------------------

import MockIO.Process            ( ꙩ )

-- monadio-plus ------------------------

import MonadIO.Error.CreateProcError  ( AsCreateProcError )
import MonadIO.Error.ProcExitError    ( AsProcExitError )

-- mtl ---------------------------------

import Control.Monad.Reader  ( MonadReader, asks )

-- parsec ------------------------------

import Text.Parsec.Prim  ( parserFail )

-- parsec-plus -------------------------

import ParsecPlus  ( Parsecable( parsec, parser ), ParseError )

-- parsers -----------------------------

import Text.Parser.Char  ( CharParsing, satisfy )

-- safe --------------------------------

import Safe  ( succSafe )

-- stdmain -----------------------------

import StdMain                      ( jsonParse )
import StdMain.ProcOutputParseError ( AsProcOutputParseError )

-- text --------------------------------

import Data.Text           ( drop, intercalate, pack, unlines )
import Data.Text.Encoding  ( encodeUtf8 )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified MockIO.Cmds.UtilLinux.Paths  as  Paths

--------------------------------------------------------------------------------

newtype MyAbsDir = MyAbsDir AbsDir
  deriving Show

--------------------

instance Printable MyAbsDir where
  print (MyAbsDir d) = print d

-- This isn't part of fpath, because it's unclear whether the 'true' instance
-- should require a trailing '/' or not
instance FromJSON MyAbsDir where
  parseJSON (String t) = case parseAbsDirP @FPathError t of
                           𝕽 f → return $ MyAbsDir f
                           𝕷 e → fail (show e)
  parseJSON invalid    = prependFailure "json parsing AbsDir failed: "
                                        (typeMismatch "String" invalid)


nonSpaces ∷ CharParsing η ⇒ η 𝕊
nonSpaces = many (satisfy (ﬧ ∘ isSpace))

------------------------------------------------------------

{-| Filesystem Type -}
data FSType = FS_BPF | FS_CGroup2 | FS_Config | FS_Debug | FS_DevTmp | FS_DevPTS
            | FS_EFIVar | FS_Ext2 | FS_Fusectl | FS_HugeTLB | FS_MQueue
            | FS_Proc | FS_PStore | FS_RAM | FS_Security | FS_Sys | FS_Tmp
            | FS_VFat | FS_XFS
  deriving (Eq,Show)

instance Parsecable FSType where
  parser =
    nonSpaces ≫ \ case "bpf"        → return FS_BPF
                       "cgroup2"    → return FS_CGroup2
                       "configfs"   → return FS_Config
                       "debugfs"    → return FS_Debug
                       "devtmpfs"   → return FS_DevTmp
                       "devpts"     → return FS_DevPTS
                       "efivarfs"   → return FS_EFIVar
                       "ext2"       → return FS_Ext2
                       "fusectl"    → return FS_Fusectl
                       "hugetlbfs"  → return FS_HugeTLB
                       "mqueue"     → return FS_MQueue
                       "proc"       → return FS_Proc
                       "pstore"     → return FS_PStore
                       "ramfs"      → return FS_RAM
                       "securityfs" → return FS_Security
                       "sysfs"      → return FS_Sys
                       "tmpfs"      → return FS_Tmp
                       "vfat"       → return FS_VFat
                       "xfs"        → return FS_XFS
                       e            → parserFail $ [fmt|unknown FSType: '%s'|] e

instance FromJSON FSType where
  parseJSON (String t) = case parsec @_ @ParseError ("json"∷𝕋) t of
                           𝕽 f → return f
                           𝕷 e → fail (show e)
  parseJSON invalid    = prependFailure "json parsing FSType failed: "
                                        (typeMismatch "String" invalid)

----------------------------------------

{-| Drop a (single) leading underscore, if any. -}
chop_ ∷ 𝕊 → 𝕊
chop_ ('_' : x) = x
chop_ x         = x

------------------------------------------------------------

{-| The detailed data for a single filesystem as reported by @findmnt@ -}
data FindMntDatum = FindMntDatum { _source ∷ 𝕋
                                 , _target ∷ MyAbsDir
                                 , _fstype ∷ FSType
                                 , _used ∷ ℕ
                                 , _size ∷ ℕ
                                 , _avail ∷ ℕ
                                 , _fsroot ∷ MyAbsDir
                                 , _label ∷ 𝕄 𝕋
                                 , _partlabel ∷ 𝕄 𝕋
                                 }
  deriving (Generic,Show)

--------------------

instance Printable FindMntDatum where
  print d = P.text $ [fmt|[%t] %T → %T (%Y/%Y)|]
                          (drop 3 ∘ pack ∘ show $ _fstype d)
                          (_source d) (_target d)
                          (toInteger $ _used d) (toInteger $ _size d)

--------------------

instance FromJSON FindMntDatum where
  parseJSON =
    let labelF' ∷ 𝕊 → 𝕊
        labelF' [] = []
        labelF' ('_':'_':xs) = '%' : labelF' xs
        labelF' ('_':xs) = '-' : labelF' xs
        labelF' ('\'':xs) = ':' : labelF' xs
        labelF' (x:xs)   = x : labelF' xs
        labelF ∷ 𝕊 → 𝕊
        labelF ('_' : xs) = labelF' xs
        labelF xs = labelF' xs
    in genericParseJSON (defaultOptions { fieldLabelModifier = labelF
                                        , rejectUnknownFields = 𝕿
                                        } )

----------------------------------------

{-| Where the filesystem is mounted from (e.g., a device file) -}
source ∷ Lens' FindMntDatum 𝕋
source = lens _source (\ dtm s → dtm { _source = s })

----------------------------------------

{-| Where the filesystem is mounted -}
target ∷ Lens' FindMntDatum AbsDir
target = lens (\ dtm → let (MyAbsDir dir) = _target dtm in dir)
              (\ dtm d → dtm { _target = MyAbsDir d })

----------------------------------------

{-| Filesystem type -}
fstype ∷ Lens' FindMntDatum FSType
fstype = lens _fstype (\ dtm t → dtm { _fstype = t })

----------------------------------------

{-| Filesystem space used, in bytes -}
used ∷ Lens' FindMntDatum ℕ
used = lens _used (\ dtm u → dtm { _used = u })

----------------------------------------

{-| Filesystem size, in bytes -}
size ∷ Lens' FindMntDatum ℕ
size = lens _size (\ dtm z → dtm { _size = z })

----------------------------------------

{-| Filesystem space available, in bytes.  This might not be size - used,
    depending -}
avail ∷ Lens' FindMntDatum ℕ
avail = lens _avail (\ dtm a → dtm { _avail = a })

----------------------------------------

{-| Where in the source filesystem we mount (normally /). -}
fsroot ∷ Lens' FindMntDatum AbsDir
fsroot = lens (\ dtm → let (MyAbsDir root) = _fsroot dtm in root)
              (\ dtm r → dtm { _fsroot = MyAbsDir r })

----------------------------------------

{-| Filesystem label (if it has one) -}
label ∷ Lens' FindMntDatum (𝕄 𝕋)
label = lens _label (\ dtm l → dtm { _label = l })

----------------------------------------

{-| Filesystem partition label (if it has one) -}
partlabel ∷ Lens' FindMntDatum (𝕄 𝕋)
partlabel = lens _partlabel (\ dtm l → dtm { _partlabel = l })

------------------------------------------------------------

{-| Full data set as returned by @findmnt@ -}
newtype FindMntData = FindMntData { _filesystems :: [FindMntDatum] }
  deriving (Generic,Show)

instance Semigroup FindMntData where
  (FindMntData fs) <> (FindMntData fs') = FindMntData (fs <> fs')

instance Monoid FindMntData where
  mempty = FindMntData []

instance FromJSON FindMntData where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = chop_ } )

{-| The filesystems of a @findmnt@ return dataset -}
filesystems ∷ Lens' FindMntData [FindMntDatum]
filesystems = lens _filesystems (\ d fss → d { _filesystems = fss })

{-| look up a `FindMntDatum` by source field -}
findMntSource ∷ FindMntData → 𝕋 → 𝕄 FindMntDatum
findMntSource f s = find (\ d → s ≡ _source d) (f ⊣ filesystems)

{-| look up a `FindMntDatum` by target field -}
findMntTarget ∷ FindMntData → AbsDir → 𝕄 FindMntDatum
findMntTarget f s = find (\ d → s ≡ d ⊣ target) (f ⊣ filesystems)

------------------------------------------------------------

{-| Run @findmnt@, parse the data. -}
findmnt ∷ ∀ ε ρ μ .
          (MonadIO μ,
           AsProcExitError ε, AsProcOutputParseError ε, AsCreateProcError ε,
           AsFPathError ε, AsIOError ε, Printable ε, MonadError ε μ,
           HasDoMock ρ, MonadReader ρ μ,
           MonadLog (Log MockIOClass) μ) ⇒
          Severity → μ FindMntData
findmnt sev = do
  do_mock ← asks (view doMock)
  let msg = "fndmt"
      vmsg ∷ 𝕄 (FindMntData → [𝕋])
      vmsg = 𝕵 $ \ findmntdata → [ [fmt|%T|] d | d ← _filesystems findmntdata ]
      log_attr = def & ioClass ⊢ IOCmdR & doMock ⊢ do_mock

      mock_text = unlines [ "{"
                          , "   \"filesystems\": ["
                          , "      {"
                          , "         \"source\": \"/dev/disk/by-label/root\","
                          , "         \"target\": \"/\","
                          , "         \"fstype\": \"ext2\","
                          , "         \"partlabel\": \"root\","
                          , "         \"size\": 50763776,"
                          , "         \"used\": 38767616,"
                          , "         \"fsroot\": \"/\","
                          , "         \"label\": \"root\","
                          , "         \"avail\": 9374720"
                          , "      }"
                          , "   ]"
                          , "}"
                          ]

      outputs = intercalate "," [ "source", "target", "fstype", "partlabel"
                                , "size", "used", "fsroot", "label", "avail" ]
      args    = [ "--real"∷𝕋, "--bytes", "--json", "--list"
                , "--notruncate", "--submounts", "--output", outputs ]
  ls ← snd ⊳ ꙩ (Paths.findmnt, args, succSafe sev, encodeUtf8 mock_text)
  result ← jsonParse (fromStrict ls)
  logResult sev log_attr do_mock msg vmsg (𝕽 result)

-- that's all, folks! ----------------------------------------------------------
