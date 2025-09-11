# lifted-threads

[![CI](https://github.com/scrive/lifted-threads/actions/workflows/haskell-ci.yml/badge.svg?branch=master)](https://github.com/scrive/lifted-threads/actions/workflows/haskell-ci.yml)
[![Hackage](https://img.shields.io/hackage/v/lifted-threads.svg)](https://hackage.haskell.org/package/lifted-threads)
[![Stackage LTS](https://www.stackage.org/package/lifted-threads/badge/lts)](https://www.stackage.org/lts/package/lifted-threads)
[![Stackage Nightly](https://www.stackage.org/package/lifted-threads/badge/nightly)](https://www.stackage.org/nightly/package/lifted-threads)

Lifted IO operations from the threads library.

`lifted-threads` exports IO operations from the base library lifted to
any instance of `MonadBase` or `MonadBaseControl`.
