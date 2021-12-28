{-# LANGUAGE LambdaCase #-}

module Graphics.Engine where

import Control.Arrow
import Control.Monad.Representable.Reader
import FRP.Yampa
import GOL.Engine
import GOL.Space
import Graphics.Config
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game hiding (Event)
import qualified Graphics.Gloss.Interface.IO.Game as G
import Graphics.GlossUtils (InputEvent)

type TickEvent = Event ()

initialSpace :: Space f => f Bool
initialSpace = tabulate $ const False

engine :: Space f => GOL f Bool -> SF TickEvent (GOL f Bool)
engine = accumHoldBy (\s _ -> tick s)

defaultPeriod = 0.5

repeatTick :: Time -> SF a TickEvent
repeatTick t = repeatedly t ()

tickSignal :: SF (Event Time) TickEvent
tickSignal =
  arr (\ev -> ((), fmap repeatTick ev))
    >>> drSwitch (repeatTick defaultPeriod)

run :: SF InputEvent Picture
run = arr (fmap processEvent) >>> accum defaultConfig >>> ()
  where
    processEvent :: G.Event -> Config -> Config
    processEvent (EventKey (Char '-') Down _ _) c = c {tickPeriod = tickPeriod c * 0.8}
    processEvent (EventResize s) c = c {windowSize = s}
    processEvent _ c = c