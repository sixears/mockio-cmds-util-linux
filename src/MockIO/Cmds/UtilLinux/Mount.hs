{-| Interface to the @mount@ cmdline utility.  It probably requires rootness,
    but not always; so that's not checked.  However, that's not a guaranteed
    requirement, so this interface does not check or enforce that.
 -}
module MockIO.Cmds.UtilLinux.Mount
  ( mount, mounts )
where

import Base1T

-- fpath -------------------------------

import FPath.AbsDir            ( AbsDir )
import FPath.AbsFile           ( AbsFile )
import FPath.Error.FPathError  ( AsFPathError )

-- lens --------------------------------

import Control.Lens.Getter  ( view )

-- log-plus ----------------------------

import Log  ( Log )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity )

-- mockio ------------------------------

import MockIO.DoMock  ( HasDoMock( doMock ) )

-- mockio-log --------------------------

import MockIO.Log          ( logio )
import MockIO.MockIOClass  ( MockIOClass )

-- mockio-plus -------------------------

import MockIO.Process  ( ꙩ )

-- monadio-plus ------------------------

import MonadIO.Error.CreateProcError  ( AsCreateProcError )
import MonadIO.Error.ProcExitError    ( AsProcExitError )

-- mtl ---------------------------------

import Control.Monad.Reader  ( MonadReader, asks )

-- safe --------------------------------

import Safe  ( succSafe )

-- stdmain -----------------------------

import StdMain.ProcOutputParseError ( AsProcOutputParseError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified MockIO.Cmds.UtilLinux.Paths  as  Paths

--------------------------------------------------------------------------------

{-| Possible ways to request a mount -}
class ToMountArgs α where
  {-| Convert to arguments to @mount@ -}
  toMountArgs ∷ α → [𝕋]
  {-| Write a msg to the log that we're about to mount -}
  mountMsg ∷ α → 𝕋

{-| Mount a partition by file (e.g., a device file) and mount target
    directory. -}
instance ToMountArgs (AbsFile, AbsDir) where
  toMountArgs (f,d) = [toText f, toText d]
  mountMsg (f,d) = [fmtT|mounting %T at %T|] f d

{-| Mount a partition. -}
mount ∷ ∀ ε α ρ μ .
        (MonadIO μ, ToMountArgs α, HasDoMock ρ, MonadReader ρ μ,
         AsProcOutputParseError ε, AsProcExitError ε, AsCreateProcError ε,
         AsFPathError ε, AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
         MonadLog (Log MockIOClass) μ) ⇒
        Severity → α → μ ()
mount sev args = snd ⊳ ꙩ (Paths.mount, toMountArgs args, sev)

{-| Mounts several partitions, emitting a log message at the given severity
    for each. -}
mounts ∷ ∀ ε κ ν ψ ρ μ .
         (MonadIO μ, Traversable ψ, ToMountArgs (κ,ν), Printable κ, Printable ν,
          HasDoMock ρ, MonadReader ρ μ,
          AsProcOutputParseError ε, AsProcExitError ε, AsCreateProcError ε,
          AsFPathError ε, AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
          MonadLog (Log MockIOClass) μ) ⇒
         Severity → ψ (κ,ν) → μ ()
mounts sev argss = do
  do_mock ← asks (view doMock)
  forM_ argss $ \ m → logio sev (mountMsg m) do_mock ⪼ mount (succSafe sev) m

-- that's all, folks! ----------------------------------------------------------
