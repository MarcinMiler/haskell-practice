import Data.Semigroup as S
import Data.List.NonEmpty as N

--class Semigroup a where
--    (<>) :: a -> a -> a

--data NonEmpty a = a :| [a]
--    deriving (Eq, Ord, Show)

xs = 1 :| [2,3]
ys = 4 :| [5,6]

xy = xs <> ys
-- 1 :| [2,3,4,5,6]

-- #TODO exercises with QuickCheck
