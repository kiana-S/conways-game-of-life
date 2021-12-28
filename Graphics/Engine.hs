{-# LANGUAGE LambdaCase #-}
module Graphics.Engine where

import Control.Arrow
import Control.Monad.Representable.Reader
import FRP.Yampa
import GOL.Engine
import GOL.Space
import Graphics.Config
import Graphics.Gloss
import Graphics.GlossUtils (InputEvent)
import Graphics.Gloss.Interface.IO.Game hiding (Event)
import qualified Graphics.Gloss.Interface.IO.Game as G

data Tick = Tick

initialSpace :: Space f => f Bool
initialSpace = tabulate $ const False

engine :: Space f => GOL f Bool -> SF (Event Tick) (GOL f Bool)
engine = accumHoldBy (\s _ -> tick s)

defaultPeriod = 0.5

repeatTick :: Time -> SF a (Event Tick)
repeatTick t = repeatedly t Tick

tickSignal :: SF (Event Time) (Event Tick)
tickSignal =
  arr (\ev -> ((), fmap repeatTick ev))
    >>> drSwitch (repeatTick defaultPeriod)
