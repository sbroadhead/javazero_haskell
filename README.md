javazero_haskell
================

A simplistic compiler for a Java-like language, written in Haskell for a compilers course.
Never intended for public consumption, all sources as-is. There is no documentation about what subset of
the language is implemented, so read the few example files provided and the source code to figure it out.

The `compile` function in JavaZero.hs compiles the file to a `.cil` (Microsoft .NET) intermediate file, which 
then needs to be compiled using `ilasm`.
