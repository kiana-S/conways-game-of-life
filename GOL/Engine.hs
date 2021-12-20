{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GOL.Engine where

import Control.Comonad.Env
import Control.Comonad.Representable.Store
import Data.Bool (bool)
import GOL.Rule
import GOL.Space

type GOL f = EnvT Rule (Store f)

getNeighbors :: forall s f a. Space s f => GOL f a -> [a]
getNeighbors = experiment $ neighbors @s @f

nextState :: Space s f => GOL f Bool -> Bool
nextState = do
  selfState <- extract
  neighborStates <- getNeighbors
  let count = sum . fmap (bool 0 1) $ neighborStates
  
  Rule survive birth <- ask
  return $ if selfState
            then survive count
            else birth count


tick :: Space s f => GOL f Bool -> GOL f Bool
tick = extend nextState