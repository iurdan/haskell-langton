Langton's ant implemented in Haskell using repa and SDL
==========

About
----------

This is a Haskell implementation of Langton's ant that uses repa for 
efficient arrays and parallelization, and SDL for graphics.

Usage
----------

Build this package with

~~~~
cabal configure
cabal build
~~~~

And run it with

~~~~
cabal run <cell size> +RTS -Nx
~~~~

Where x is the number of cores of your computer.

Press RETURN to exit.
