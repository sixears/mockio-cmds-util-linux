{-| Interface to the @umount@ cmdline utility.  It probably requires rootness,
    but not always; so that's not checked.  However, that's not a guaranteed
    requirement, so this interface does not check or enforce that.
 -}
module MockIO.Cmds.UtilLinux.Umount
  ( umount, umounts )
where

import Base1T

-- fpath -------------------------------

import FPath.AbsDir            ( AbsDir )
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

{-| Possible ways to request an unmount -}
class ToUmountArgs α where
  toUmountArgs ∷ α → [𝕋]
  {-| Write a msg to the log that we're about to unmount -}
  unmountMsg ∷ α → 𝕋

{-| Unmount a partition by mount target directory -}
instance ToUmountArgs AbsDir where
  toUmountArgs d = [toText d]
  unmountMsg d = [fmtT|unmounting %T|] d

{-| Unmount a partition. -}
umount ∷ ∀ ε α ρ μ .
         (MonadIO μ, ToUmountArgs α, HasDoMock ρ, MonadReader ρ μ,
          AsProcOutputParseError ε, AsProcExitError ε, AsCreateProcError ε,
          AsFPathError ε, AsIOError ε, Printable ε, MonadError ε μ,HasCallStack,
          MonadLog (Log MockIOClass) μ) ⇒
         Severity → α → μ ()
umount sev args = snd ⊳ ꙩ (Paths.umount,toUmountArgs args,sev)

{-| Unmounts several partitions, emitting a log message at the given severity
    for each. -}
umounts ∷ ∀ ε ν ψ ρ μ .
          (MonadIO μ, Traversable ψ, ToUmountArgs ν,
           HasDoMock ρ, MonadReader ρ μ, MonadLog (Log MockIOClass) μ,
           AsProcOutputParseError ε, AsProcExitError ε, AsCreateProcError ε,
           AsFPathError ε, AsIOError ε,Printable ε,MonadError ε μ,HasCallStack)⇒
          Severity → ψ ν → μ ()
umounts sev argss = do
  do_mock ← asks (view doMock)
  forM_ argss $ \ d → do
    logio sev (unmountMsg d) do_mock
    umount (succSafe sev) d

-- that's all, folks! ----------------------------------------------------------
