{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GOL.Engine where

import Control.Comonad.Env
import Control.Comonad.Identity (Identity (Identity))
import Control.Comonad.Representable.Store
import Data.Bool (bool)
import Data.Functor.Rep
import GOL.Rule
import GOL.Space

-- | The comonad stack used in the Game of Life engine.
-- It consists of a store comonad over the space @f@,
-- with an environment containing a rule.
type GOL f = EnvT Rule (Store f)

-- | Construct a 'GOL' value given a rule, a board
-- state, and an initial position.
gol :: Rule -> Rep f -> f a -> GOL f a
gol r p s = EnvT r $ StoreT (Identity s) p

-- | Construct a 'GOL' value on a displayable space,
-- defaulting to an initial position of @(0, 0)@.
gol' :: DisplayableSpace f => Rule -> f a -> GOL f a
gol' r = gol r (0,0)



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