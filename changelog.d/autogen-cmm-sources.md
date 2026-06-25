---
synopsis: Add 'autogen-cmm-sources' build-info field for generated C-- sources
packages: [Cabal, Cabal-syntax]
significance: significant
---

Cabal can now compile and link C-- (`.cmm`) sources that are *generated* into
the build directory, e.g. by a custom `Setup.hs` or a build hook, just like it
already handles generated C sources.

A new build-info field `autogen-cmm-sources` lists such files. Unlike
`cmm-sources`, whose paths are relative to the package source tree, the paths in
`autogen-cmm-sources` are relative to the build directory (`RelativePath Build
File`), so they are resolved against the build directory rather than the package
root before being passed to the compiler.
