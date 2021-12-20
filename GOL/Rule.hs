module GOL.Rule where

-- | The rule of a Conway's Game of Life simulation
-- describes how a cell changes based on its surrounding
-- cells. The official GOL uses one standard rule, but
-- this engine allows for an arbitrary rule to be applied.
--
-- A rule consists of two predicates that take a value
-- in the range [0..8], the count of alive cells neighboring
-- the current cell, and returns whether that cell will be
-- alive in the next generation.
data Rule = Rule
  { -- | A predicate deciding if an alive cell
    -- will be alive in the next generation.
    survive :: Int -> Bool,
    -- | A predicate deciding if a dead cell
    -- will be alive in the next generation.
    birth :: Int -> Bool
  }

-- | Construct a rule by giving lists of
-- values that satisfy each predicate.
rule :: [Int] -> [Int] -> Rule
rule s b = Rule (`elem` s) (`elem` b)

-- | The standard GOL rule (S23/B3).
standardRule :: Rule
standardRule = rule [2, 3] [3]
