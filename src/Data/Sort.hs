
module Data.Sort
  ( L.sort
  , L.sortBy
  , sortON

  , monoidSortAssocs
  , monoidSortAssocsBy
  , groupSortAssocs
  , groupSortAssocsBy

  , monoidSort
  , monoidSortOn
  , monoidSortBy

  , groupSort
  , groupSortOn
  , groupSortBy
  ) where

import qualified Data.List                as L
import           Data.Monoid
import           Data.Ord


sortON :: Ord b => (a->b) -> [a] -> [a]
sortON chg = project . sortBy cmp_key . inject chg


monoidSortAssocs :: (Monoid a,Ord k)
                 => [(k,a)]
                 -> [(k,a)]
monoidSortAssocs = monoidSortAssocsBy compare

monoidSortAssocsBy :: (Monoid a)
                   => (k->k->Ordering)
                   -> [(k,a)]
                   -> [(k,a)]
monoidSortAssocsBy cmp = groupSortAssocsBy cmp monoid_group

groupSortAssocs :: Ord k
                => (a->[a]->b)
                -> [(k,a)]
                -> [(k,b)]
groupSortAssocs = groupSortAssocsBy compare

groupSortAssocsBy :: (k->k->Ordering)
                  -> (a->[a]->b)
                  -> [(k,a)]
                  -> [(k,b)]
groupSortAssocsBy cmp0 grp0 = groupSortBy cmp grp
  where
    cmp (k,_) (k',_) = cmp0 k k'

    grp (k,y) ps     = (,) k $ grp0 y $ map snd ps



monoidSort :: (Monoid a,Ord a) => [a] -> [a]
monoidSort = monoidSortBy compare

monoidSortOn :: (Monoid a,Ord k) => (a->k) -> [a] -> [a]
monoidSortOn chg = groupSortOn chg monoid_group

monoidSortBy :: Monoid a => (a->a->Ordering) -> [a] -> [a]
monoidSortBy cmp = groupSortBy cmp monoid_group



-- | sort a list of elements with a stable sort, grouping together the
-- equal elements with the argument grouping function
groupSort :: (Ord a) => (a->[a]->b) -> [a] -> [b]
groupSort = groupSortBy compare

-- | sort a list of elements with a stable sort, using the argument
-- @compare@ function determine the ordering, grouping together the
-- equal elements with the grouping function
groupSortOn :: Ord k
            => (a->k)
            -> (a->[a]->b)
            -> [a]
            -> [b]
groupSortOn chg grp =
    project . groupSortBy cmp_key grp_val . inject chg
  where
    grp_val a as = Assoc (_a_key a) $ grp (_a_value a) $ map _a_value as

groupSortBy :: (a->a->Ordering)
            -> (a->[a]->b)
            -> [a]
            -> [b]
groupSortBy cmp grp = aggregate . sortBy cmp
  where
    aggregate []    = []
    aggregate (h:t) = seq g $ g : aggregate rst
      where
        g         = grp h eqs
        (eqs,rst) = span is_le t

        is_le x   = case cmp x h of
          LT -> True
          EQ -> True
          GT -> False



sortBy :: (a->a->Ordering) -> [a] -> [a]
sortBy = L.sortBy


data Assoc k a =
  Assoc
    { _a_key   :: !k
    , _a_value :: a
    }
  deriving (Show)

inject :: (a->k) -> [a] -> [Assoc k a]
inject chg = map $ \x -> Assoc (chg x) x

project :: [Assoc k a] -> [a]
project = map _a_value

cmp_key :: Ord k => Assoc k a -> Assoc k a -> Ordering
cmp_key = comparing _a_key




monoid_group :: Monoid a => a -> [a] -> a
monoid_group x xs = x <> mconcat xs
