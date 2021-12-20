{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module GOL.Space where

import Data.Functor.Rep

class Representable f => Space f where
  neighbors :: Rep f -> [Rep f]

class (Space f, Rep f ~ (Int, Int)) => DisplayableSpace f where
  size :: (Int, Int)