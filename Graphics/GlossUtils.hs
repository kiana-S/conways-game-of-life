module Graphics.GlossUtils (playYampa, InputEvent) where

-- A utility function to connect the FRP library Yampa to the
-- graphics library Gloss, from the library yampa-gloss

import Control.Monad (when)
import Data.IORef
  ( newIORef,
    readIORef,
    writeIORef,
  )
import FRP.Yampa
  ( Event (..),
    SF,
    react,
    reactInit,
  )
import Graphics.Gloss
  ( Color,
    Display,
    Picture,
    blank,
  )
import qualified Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (playIO)
import qualified Graphics.Gloss.Interface.IO.Game as G

type InputEvent = G.Event

playYampa ::
  -- | The display method
  Display ->
  -- | The background color
  Color ->
  -- | The refresh rate, in Hertz
  Int ->
  SF (Event InputEvent) Picture ->
  IO ()
playYampa display color frequency mainSF = do
  picRef <- newIORef blank
  handle <-
    reactInit
      (return NoEvent)
      ( \_ updated pic ->
          when updated (picRef `writeIORef` pic) >> return False
      )
      mainSF
  let delta = 0.01 / fromIntegral frequency
      toPic = const $ readIORef picRef
      handleInput = (\e t -> react handle (delta, Just (Event e)) >> return (t + delta))
      stepWorld =
        ( \d t ->
            let delta' = realToFrac d - t
             in if delta' > 0
                  then react handle (delta', Just NoEvent) >> return 0.0
                  else return (- delta')
        )
  playIO display color frequency 0 toPic handleInput stepWorld
