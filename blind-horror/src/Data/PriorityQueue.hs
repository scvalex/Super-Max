-- | A simple priority queue implemented in terms of a functional heap.
module Data.PriorityQueue (
        PriorityQueue, empty, isEmpty, insert, extractMin, peekMin
    ) where

type PriorityQueue a = SkewHeap a

data SkewHeap a = Empty
                | SkewNode a (SkewHeap a) (SkewHeap a)
                deriving (Show)

(+++) :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
heap1@(SkewNode x1 l1 r1) +++ heap2@(SkewNode x2 l2 r2)
    | x1 <= x2    = SkewNode x1 (heap2 +++ r1) l1
    | otherwise = SkewNode x2 (heap1 +++ r2) l2
Empty +++ heap = heap
heap +++ Empty = heap

-- | Get and remove the minimum element from the priority queue.
extractMin :: (Ord a) => SkewHeap a -> Maybe (a, SkewHeap a)
extractMin Empty            = Nothing
extractMin (SkewNode x l r) = Just (x , l +++ r )

-- | Get the minimum element from the priority queue, without removing it.
peekMin :: (Ord a) => SkewHeap a -> Maybe a
peekMin Empty            = Nothing
peekMin (SkewNode x _ _) = Just x

-- | Add an element to the priority queue.
insert :: (Ord a) => SkewHeap a -> a -> SkewHeap a
insert heap x = heap +++ (SkewNode x Empty Empty)

-- | An empty priority queue.
empty :: SkewHeap a
empty = Empty

-- | Is the given priority queue empty?
isEmpty :: SkewHeap a -> Bool
isEmpty Empty = True
isEmpty _     = False
