module Graphics.Config where

import FRP.Yampa (Time)
import GOL.Rule (Rule)
import Graphics.Gloss (Color)

data Config = Config
  { cellColor :: Color,
    rule :: Rule,
    tickRate :: Time,
    windowSize :: (Int, Int)
  }