{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module GOL.Space where

import Data.Functor.Rep

-- | A space in which a Conway's Game of Life simulation
-- takes place, with a notion of "neighbors to a cell" defined.
--
-- More specifically, a space is a representable functor @f@ such
-- that @'Rep' f@ is a graph. 'neighbors' then takes a node of
-- that graph and returns all nodes that are adjacent.
--
-- Instances should satisfy:
--
-- * Symmetry: if @x '`elem`' 'neighbors' y@, then @y '`elem`' 'neighbors' x@.
--
-- * Irreflexivity: @x '`elem`' 'neighbors' x@ is always false.
class Representable f => Space f where
  neighbors :: Rep f -> [Rep f]

-- | A space is _displayable_ if it is representable over a 2D
-- grid. The graphical system requires a displayable space.
class (Space f, Rep f ~ (Int, Int)) => DisplayableSpace f where
  sizex :: Int
  sizey :: Int