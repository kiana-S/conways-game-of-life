{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Graphics.Display where

import Control.Monad.Representable.Reader
import Data.Maybe (mapMaybe)
import GOL.Space
import Graphics.Gloss

square :: Float -> Point -> Picture
square size (x, y) = translate (x + size / 2) y (rectangleUpperSolid size size)

squareAtIndex :: (Int, Int) -> Float -> Picture
squareAtIndex (x, y) size = square size (fromIntegral x * size, fromIntegral y * size)

-- * Calculating the display grid

drawCell :: DisplayableSpace f => f Bool -> (Int, Int) -> Float -> Maybe Picture
drawCell xs pos size =
  if index xs pos
    then Just $ squareAtIndex pos size
    else Nothing

drawGrid :: forall f. DisplayableSpace f => f Bool -> Float -> [Picture]
drawGrid xs size =
  let poss = (,) <$> [0 .. sizex @f -1] <*> [0 .. sizey @f -1]
   in mapMaybe (\pos -> drawCell xs pos size) poss