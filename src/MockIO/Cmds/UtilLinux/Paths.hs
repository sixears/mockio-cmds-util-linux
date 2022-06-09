{-# LANGUAGE QuasiQuotes #-}

{-| Filesystem paths to external programs; to be filled in at build time. -}
module MockIO.Cmds.UtilLinux.Paths where

import FPath.AbsFile  ( AbsFile, absfile )

{-| Path to @findmnt@ -}
findmnt :: AbsFile
findmnt = [absfile|/nix/store/sh9vs60dwybm08h0fz8pq5w2194dmdn4-util-linux-2.37.4-bin/bin/findmnt|]

{-| Path to @mount@ -}
mount :: AbsFile
mount = [absfile|/nix/store/sh9vs60dwybm08h0fz8pq5w2194dmdn4-util-linux-2.37.4-bin/bin/mount|]

{-| Path to @umount@ -}
umount :: AbsFile
umount = [absfile|/nix/store/sh9vs60dwybm08h0fz8pq5w2194dmdn4-util-linux-2.37.4-bin/bin/umount|]
