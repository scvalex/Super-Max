module Data.PriorityQueue (
        empty, insert, extractMin, peekMin
    ) where

data SkewHeap a = Empty
                | SkewNode a (SkewHeap a) (SkewHeap a)
                deriving (Show)

(+++) :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
heap1@(SkewNode x1 l1 r1) +++ heap2@(SkewNode x2 l2 r2)
    | x1 <= x2    = SkewNode x1 (heap2 +++ r1) l1
    | otherwise = SkewNode x2 (heap1 +++ r2) l2
Empty +++ heap = heap
heap +++ Empty = heap

extractMin :: (Ord a) => SkewHeap a -> Maybe (a, SkewHeap a)
extractMin Empty            = Nothing
extractMin (SkewNode x l r) = Just (x , l +++ r )

peekMin :: (Ord a) => SkewHeap a -> Maybe a
peekMin Empty            = Nothing
peekMin (SkewNode x _ _) = Just x

insert :: (Ord a) => SkewHeap a -> a -> SkewHeap a
insert heap x = heap +++ (SkewNode x Empty Empty)

empty :: SkewHeap a
empty = Empty
