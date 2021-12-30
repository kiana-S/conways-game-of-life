{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Engine where

import Control.Arrow
import Control.Monad.Representable.Reader
import FRP.Yampa
import GOL.Engine
import GOL.Rule
import GOL.Space
import Graphics.Command
import Graphics.Display (drawGrid)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game hiding (Event)
import qualified Graphics.Gloss.Interface.IO.Game as G
import Graphics.GlossUtils (InputEvent)

type TickEvent = Event ()

initialSpace :: Space f => f Bool
initialSpace = tabulate $ const False

engine :: Space f => GOL f Bool -> SF TickEvent (GOL f Bool)
engine = accumHoldBy (\s _ -> tick s)

repeatTick :: Time -> SF a TickEvent
repeatTick t = repeatedly t ()

tickSignal :: SF (Event Time) TickEvent
tickSignal =
  arr (\ev -> ((), fmap repeatTick ev))
    >>> drSwitch (repeatTick 0.2)

processEvent :: InputEvent -> CommandEvent
processEvent = event noEvent $ \case
  EventKey (Char '-') Down _ _ -> Event $ ChangeSpeed (* 1.25)
  EventKey (Char '+') Down _ _ -> Event $ ChangeSpeed (* 0.8)
  EventKey (SpecialKey KeySpace) Down _ _ -> Event PlayPause
  EventKey {} -> noEvent
  EventMotion {} -> noEvent
  EventResize size -> Event $ Resize size

run :: forall f. DisplayableSpace f => f Bool -> SF InputEvent Picture
run st =
  let initSpace = gol' standardRule st
   in proc inp -> do
        let cmdev = processEvent inp

        playing <- accumHoldBy (const . not) True -< filterE isPlayPause cmdev
        time <- accum 0.2 -< mapFilterE getChangeSpeed cmdev
        windowSize <- hold (100, 100) -< mapFilterE getResize cmdev

        tick <- tickSignal -< time
        space <- engine initSpace -< gate tick playing

        returnA -< drawGrid windowSize white (getSpace space)