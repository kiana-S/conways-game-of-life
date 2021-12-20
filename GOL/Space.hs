{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module GOL.Space where

import Data.Functor.Rep

class (Representable f, Rep f ~ s) => Space s f where
  neighbors :: s -> [s]

class (Space (Int, Int) f) => DisplayableSpace f where
  size :: (Int, Int)