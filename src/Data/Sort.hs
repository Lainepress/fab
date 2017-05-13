
module Data.Sort
  ( naturalSort

  , monoidSortAssocs
  , monoidSortAssocsBy
  , groupSortAssocs
  , groupSortAssocsBy

  , monoidSort
  , monoidSortBy

  , groupSort
  , groupSortBy

  , sortOn

  , SortConfig
  , nativeSort
  , fabSort
  , sortBy_

  , monoidSortAssocs_
  , monoidSortAssocsBy_
  , groupSortAssocs_
  , groupSortAssocsBy_
  , monoidSort_
  , monoidSortBy_
  , groupSort_
  , groupSortBy_
  , sortOn_

  , naturalSortBy
  , runs
  , foldb
  , merge
  ) where

import qualified Data.List                as L
import           Data.Monoid
import           Data.Ord


naturalSort :: Ord a => [a] -> [a]
naturalSort = naturalSortBy compare


monoidSortAssocs :: (Monoid a,Ord k) => [(k,a)] -> [(k,a)]
monoidSortAssocs = monoidSortAssocs_ fabSort

monoidSortAssocsBy :: (Monoid a)
                   => (k->k->Ordering)
                   -> [(k,a)]
                   -> [(k,a)]
monoidSortAssocsBy = monoidSortAssocsBy_ fabSort

groupSortAssocs :: Ord k => (a->[a]->b) -> [(k,a)] -> [(k,b)]
groupSortAssocs = groupSortAssocs_ fabSort

groupSortAssocsBy :: (k->k->Ordering)
                  -> (a->[a]->b)
                  -> [(k,a)]
                  -> [(k,b)]
groupSortAssocsBy = groupSortAssocsBy_ fabSort

monoidSortAssocs_ :: (Monoid a,Ord k)
                  => SortConfig
                  -> [(k,a)]
                  -> [(k,a)]
monoidSortAssocs_ sc = monoidSortAssocsBy_ sc compare

monoidSortAssocsBy_ :: (Monoid a)
                    => SortConfig
                    -> (k->k->Ordering)
                    -> [(k,a)]
                    -> [(k,a)]
monoidSortAssocsBy_ sc cmp = groupSortAssocsBy_ sc cmp monoid_group

groupSortAssocs_ :: Ord k
                 => SortConfig
                 -> (a->[a]->b)
                 -> [(k,a)]
                 -> [(k,b)]
groupSortAssocs_ sc = groupSortAssocsBy_ sc compare

groupSortAssocsBy_ :: SortConfig -> (k->k->Ordering) -> (a->[a]->b) -> [(k,a)] -> [(k,b)]
groupSortAssocsBy_ sc cmp0 grp0 = groupSortBy_ sc cmp grp
  where
    cmp (k,_) (k',_) = cmp0 k k'

    grp (k,y) ps     = (,) k $ grp0 y $ map snd ps



monoidSort :: (Monoid a,Ord a) => [a] -> [a]
monoidSort = monoidSort_ fabSort

monoidSortBy :: Monoid a => (a->a->Ordering) -> [a] -> [a]
monoidSortBy = monoidSortBy_ fabSort

monoidSort_ :: (Monoid a,Ord a) => SortConfig -> [a] -> [a]
monoidSort_ sc = monoidSortBy_ sc compare

monoidSortBy_ :: Monoid a => SortConfig -> (a->a->Ordering) -> [a] -> [a]
monoidSortBy_ sc cmp = groupSortBy_ sc cmp (\x xs->x <> mconcat xs)



-- | sort a list of elements with a stable sort, grouping together the
-- equal elements with the argument grouping function
groupSort :: (Ord a) => (a->[a]->b) -> [a] -> [b]
groupSort = groupSort_ fabSort

-- | sort a list of elements with a stable sort, using the argument
-- @compare@ function determine the ordering, grouping together the
-- equal elements with the grouping function
groupSortBy :: (a->a->Ordering)
            -> (a->[a]->b)
            -> [a]
            -> [b]
groupSortBy = groupSortBy_ fabSort

groupSort_ :: (Ord a) => SortConfig -> (a->[a]->b) -> [a] -> [b]
groupSort_ sc = groupSortBy_ sc compare

groupSortBy_ :: SortConfig
             -> (a->a->Ordering)
             -> (a->[a]->b)
             -> [a]
             -> [b]
groupSortBy_ sc cmp grp = aggregate . sortBy_ sc cmp
  where
    aggregate []    = []
    aggregate (h:t) = grp h eqs : aggregate rst
      where
        (eqs,rst) = span is_le t

        is_le x   = case cmp x h of
          LT -> True
          EQ -> True
          GT -> False



sortOn :: Ord b => (a->b) -> [a] -> [a]
sortOn = sortOn_ fabSort

sortOn_ :: Ord b => SortConfig -> (a->b) -> [a] -> [a]
sortOn_ sc f =
  map snd
    . sortBy_ sc (comparing fst)
    . map (\x -> let y = f x in y `seq` (y, x))







data SortConfig = Native | Natural
  deriving (Eq,Show)

nativeSort, fabSort :: SortConfig
nativeSort  = Native
fabSort     = Natural

sortBy_ :: SortConfig -> (a->a->Ordering) -> [a] -> [a]
sortBy_ Native  = L.sortBy
sortBy_ Natural = naturalSortBy




naturalSortBy :: (a->a->Ordering) -> [a] -> [a]
naturalSortBy _   [] = []              -- (foldb f []) is undefined
naturalSortBy cmp xs = foldb (merge cmp) $ runs cmp xs

runs :: (a->a->Ordering) -> [a] -> [[a]]
runs cmp xs0 = foldr op [] xs0
  where
    op z xss@(xs@(x:_):xss') | z `le` x  = (z:xs):xss'
                             | otherwise = [z]:xss
    op z xss                             = [z]:xss

    le x y = case cmp x y of
      LT -> True
      EQ -> True
      GT -> False

foldb :: (a->a->a) -> [a] -> a
foldb _ [x] = x
foldb f xs0 = foldb f (fold xs0)
      where
        fold (x1:x2:xs) = f x1 x2 : fold xs
        fold xs         = xs

merge:: (a->a->Ordering) -> [a] -> [a] -> [a]
merge _   []      l  = l
merge _   l@(_:_) [] = l
merge cmp l1@(h1:t1) l2@(h2:t2) =
    case h1 `le` h2 of
      True  -> h1 : merge cmp t1 l2
      False -> h2 : merge cmp l1 t2
  where
    le x y = case cmp x y of
      LT -> True
      EQ -> True
      GT -> False



monoid_group :: Monoid a => a -> [a] -> a
monoid_group x xs = x <> mconcat xs
