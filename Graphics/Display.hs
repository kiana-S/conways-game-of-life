{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Graphics.Display where

import Control.Monad.Representable.Reader
import Data.Maybe (mapMaybe)
import GOL.Space
import Graphics.Config
import Graphics.Gloss

-- * Drawing the display grid

square :: Float -> Point -> Picture
square size (x, y) = translate (x + size / 2) y (rectangleUpperSolid size size)

squareAtIndex :: (Int, Int) -> Float -> Picture
squareAtIndex (x, y) size = square size (fromIntegral x * size, fromIntegral y * size)

drawCell :: DisplayableSpace f => f Bool -> (Int, Int) -> Float -> Maybe Picture
drawCell xs pos size =
  if index xs pos
    then Just $ squareAtIndex pos size
    else Nothing

drawCells :: forall f. DisplayableSpace f => f Bool -> Float -> [Picture]
drawCells xs size =
  let poss = (,) <$> [0 .. sizex @f -1] <*> [0 .. sizey @f -1]
   in mapMaybe (\pos -> drawCell xs pos size) poss

drawGrid :: forall f. DisplayableSpace f => Config -> f Bool -> Picture
drawGrid config xs =
  let (w, h) = windowSize config
      size = fromIntegral $ if w > h then h `div` sizey @f else w `div` sizex @f
   in color (cellColor config) $ pictures $ drawCells xs size