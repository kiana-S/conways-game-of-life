module Graphics.Command where

import FRP.Yampa (Event, Time)

-- | A datatype representing all possible commands the user
-- can give through UI.
data Command
  = -- | A command to resize the window to the given dimensions.
    Resize (Int, Int)
  | -- | A command to change the tick speed of the simulation,
    -- given by a function on the period between ticks.
    ChangeSpeed (Time -> Time)

getResize :: Command -> Maybe (Int, Int)
getResize (Resize size) = Just size
getResize _ = Nothing

getChangeSpeed :: Command -> Maybe (Time -> Time)
getChangeSpeed (ChangeSpeed f) = Just f
getChangeSpeed _ = Nothing

-- | An event signalling that a command has been given by the user.
type CommandEvent = Event Command