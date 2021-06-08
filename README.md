# Haskell implementation of the Futhark data format

[![Hackage](https://img.shields.io/hackage/v/futhark-data.svg?style=flat)][https://hackage.haskell.org/package/futhark-data][![CI](https://github.com/diku-dk/futhark-data-haskell/workflows/CI/badge.svg)](https://github.com/diku-dk/futhark-data-haskell/actions)

This is a Haskell library that provides an implementation of the
[Futhark](https://futhark-lang.org) data format.  The [binary data
format](https://futhark.readthedocs.io/en/latest/binary-data-format.html)
is used in some of the Futhark tooling, particularly `futhark bench`
and `futhark test`, but more importantly it is the data interchange
format used by [the server
protocol](https://futhark.readthedocs.io/en/latest/server-protocol.html).
