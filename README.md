# Conway's Game of Life

An implementation of Conway's Game of Life in Haskell using
comonads and functional reactive programming.

This package consists of two parts:

- A library that can be used to simulate Conway's Game of Life on an arbitrary space
  using arbitrary rules. It is based on a comonad transformer stack.

- An executable that uses [Yampa](https://hackage.haskell.org/package/Yampa) and
  [Gloss](https://hackage.haskell.org/package/gloss-1.13.0.1) to graphically display
  a 100x100 simulation of Conway's Game of Life. The simulation can be controlled by the keys:
  
  `Space` – Play/Pause
  
  `-` – Slow down the tick rate

  `+` (hold shift) – Speed up the tick rate

This package is a work in progress and may not be fully functional.
