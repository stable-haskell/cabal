The `mhs-compat-shims` package
==========================

See the [Cabal web site] for more information.

The `mhs-compat-shims` package provides lightweight pure-Haskell and
POSIX C compatibility shims used when building `Cabal`,
`cabal-install-solver`, and `cabal-install` with MicroHs (mhs).

It collects the `Data.Binary`, `Text.PrettyPrint`, `Network.URI`, and
text-encoding shims (previously under `bootstrap/mhs/shims/`), the
`Distribution.Compat` MHS helpers (`MhsInstances`, `MhsPosix`,
`MhsProcess`), the `Distribution.Client.Compat.Base16` shim, and the
small C helpers in `cbits/` (`mhs_posix`, `mhs_spawn`, `mhs_handle`,
`mhs_sha256`).

More information
================

Please see the [Cabal web site] for the [user guide] and [API
documentation]. There is additional information available on the
[development wiki].

[user guide]:        http://www.haskell.org/cabal/users-guide
[API documentation]: http://www.haskell.org/cabal/release/cabal-latest/doc/API/Cabal/Distribution-Simple.html
[development wiki]:  https://github.com/haskell/cabal/wiki
