module Graphics.Engine where

import Control.Arrow
import Control.Monad.Representable.Reader
import FRP.Yampa
import GOL.Engine
import GOL.Space
import Graphics.GlossUtils (InputEvent)

tickEvent :: SF a (Event ())
tickEvent = repeatedly 1.0 ()

initialSpace :: Space f => f Bool
initialSpace = tabulate $ const False

engine :: Space f => GOL f Bool -> SF a (GOL f Bool)
engine start = tickEvent >>> accumHoldBy (\s _ -> tick s) start