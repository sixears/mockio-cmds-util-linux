{-# LANGUAGE QuasiQuotes #-}

{-| Filesystem paths to external programs; to be filled in at build time. -}
module MockIO.Cmds.UtilLinux.Paths where

import FPath.AbsFile  ( AbsFile, absfile )

{-| Path to @findmnt@ -}
findmnt :: AbsFile
findmnt = [absfile|__util-linux__/bin/findmnt|]

{-| Path to @mount@ -}
mount :: AbsFile
mount = [absfile|__util-linux__/bin/mount|]

{-| Path to @umount@ -}
umount :: AbsFile
umount = [absfile|__util-linux__/bin/umount|]
