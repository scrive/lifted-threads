# lifted-threads [![Hackage version](https://img.shields.io/hackage/v/lifted-threads.svg?label=Hackage)](https://hackage.haskell.org/package/lifted-threads)

Lifted IO operations from the threads library.

`lifted-threads` exports IO operations from the base library lifted to
any instance of `MonadBase` or `MonadBaseControl`.
