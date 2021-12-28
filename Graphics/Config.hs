module Graphics.Config where

import FRP.Yampa (Time)
import GOL.Rule
import Graphics.Gloss (Color, white)

data Config = Config
  { rule :: Rule,
    tickPeriod :: Time,
    cellColor :: Color,
    windowSize :: (Int, Int)
  }

defaultConfig :: Config
defaultConfig = Config standardRule 1.0 white (500, 500)