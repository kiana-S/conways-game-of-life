module Main where

import Control.Monad.Representable.Reader
import GOL.Space
import Graphics.Engine (initialSpace, run)
import Graphics.Gloss
import Graphics.GlossUtils (playYampa)

space :: ToroidalSpace Bool
space = tabulate (\(x, y) -> (x + y `mod` 5) * 10 + x - y > 30)

main :: IO ()
main =
  playYampa
    (InWindow "Conway's Game of Life" (200, 200) (10, 10))
    black
    30
    (run space)