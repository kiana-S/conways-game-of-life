{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GOL.Engine where

import Control.Comonad.Env
import Control.Comonad.Representable.Store
import Data.Bool (bool)
import GOL.Rule
import GOL.Space

type GOL f = EnvT Rule (Store f)

