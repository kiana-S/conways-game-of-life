{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module GOL.Space where

import Data.Bifunctor
import Data.Distributive
import Data.Functor.Rep
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

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

-- * Standard spaces

-- Unfortunately, due to the fact that Haskell doesn't have dependent types,
-- defining a GOL space to have an arbitrary size specified at runtime by
-- the user is completely impossible. There's simply no way to do it without
-- features that Haskell doesn't have.
-- So until we get generalized pi types in GHC, we'll have to make do with
-- hard-coded space sizes.

-- | A 100x100 2D grid space. This space is _toroidal_, which means that
-- cells on the opposite edges are considered neighbors of each other.
newtype ToroidalSpace a = TS (Vector (Vector a))
  deriving (Show, Functor)

instance Distributive ToroidalSpace where
  distribute xs = tabulate (\p -> fmap (`index` p) xs)

instance Representable ToroidalSpace where
  type Rep ToroidalSpace = (Int, Int)
  index (TS s) (i, j) = s ! j ! i
  tabulate f = TS $ V.generate 100 (\x -> V.generate 100 $ \y -> f (x, y))

instance Space ToroidalSpace where
  neighbors (i, j) =
    bimap ((`mod` 100).(+i)) ((`mod` 100).(+j)) <$>
    [
      (-1,-1), (0,-1), (1,-1),
      (-1, 0),         (1, 0),
      (-1, 1), (0, 1), (1, 1)
    ]

instance DisplayableSpace ToroidalSpace where
  sizex = 100
  sizey = 100