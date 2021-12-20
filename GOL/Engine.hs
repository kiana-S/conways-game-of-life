{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GOL.Engine where

import Control.Comonad.Env
import Control.Comonad.Representable.Store
import Data.Bool (bool)
import GOL.Rule
import GOL.Space

-- | The comonad stack used in the Game of Life engine.
-- It consists of a store comonad over the space @f@,
-- with an environment containing a rule.
type GOL f = EnvT Rule (Store f)

getNeighbors :: forall f a. Space f => GOL f a -> [a]
getNeighbors = experiment $ neighbors @f

nextState :: Space f => GOL f Bool -> Bool
nextState = do
  neighborStates <- getNeighbors
  let count = sum . fmap (bool 0 1) $ neighborStates

  selfState <- extract
  Rule survive birth <- ask
  return $
    if selfState
      then survive count
      else birth count

tick :: Space f => GOL f Bool -> GOL f Bool
tick = extend nextState