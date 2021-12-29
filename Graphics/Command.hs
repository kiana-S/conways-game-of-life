module Graphics.Command where

import FRP.Yampa (Time, Event)

data Command
  = Resize (Int, Int)
  | ChangeSpeed (Time -> Time)

getResize :: Command -> Maybe (Int, Int)
getResize (Resize size) = Just size
getResize _ = Nothing

getChangeSpeed :: Command -> Maybe (Time -> Time)
getChangeSpeed (ChangeSpeed f) = Just f
getChangeSpeed _ = Nothing

type CommandEvent = Event Command