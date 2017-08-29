{-# LANGUAGE RecordWildCards #-}
module Database.Haskey.Utils.Intervals where

import Data.Foldable (Foldable(..))

-- | Value wrapper that can go up to PInf, but with lower bound of the
-- underlying type.
data R v = PInf
         | R !v
         deriving (Eq, Show)

instance Ord v => Ord (R v) where
    _    <= PInf = True
    PInf <= _    = False

    R x <= R y = x <= y

instance Num v => Num (R v) where
    R x + R y = R (x + y)
    _   + _   = error "R: +: not supported on infinity"

    R x * R y = R (x * y)
    _   * _   = error "R: *: not supported on infinity"

    R x - R y = R (x - y)
    _   - _   = error "R: -: not supported on infinity"

    abs (R x) = R (abs x)
    abs _     = error "R: abs: not supported on infinity"

    signum (R x) = R (signum x)
    signum _     = error "R: signum: not supported on infinity"

    fromInteger x = R (fromInteger x)

-- | Interval of a certain type.
data Interval a = Interval {
    low :: R a
  , high :: R a
  } deriving (Show)

-- | Size of the interval.
intervalSize :: Num a => Interval a -> R a
intervalSize (Interval (R l) (R h)) = R $ h - l + 1
intervalSize (Interval _ _)         = PInf

-- | Default interval, ranging from [0, infinity].
defInterval :: Num a => Interval a
defInterval = Interval 0 PInf

-- | Basic binary tree data type.
data Tree a = Leaf !a
            | Branch !(Tree a) !(Tree a)
            deriving (Show)

instance Foldable Tree where
    foldr f b (Leaf i) = f i b
    foldr f b (Branch left right) = foldr f (foldr f b right) left

-- | Collection of intervals, which is a tree of intervals.
type Collection a = Tree (Interval a)

collection :: Interval a -> Collection a
collection = Leaf

-- | Invert the collection, by returing all intervals that are not contained in
-- the collection.
--
-- This function assumes the lower bound of @a@ is @0@.
invertCollection :: (Eq a, Num a) => Collection a -> [Interval a]
invertCollection c =
    case foldr go (Nothing, []) c of
        (Nothing, xs) -> xs
        (Just 0 , xs) -> xs
        (Just l , xs) -> Interval 0 (R l - 1) : xs
  where
    go :: Num a
       => Interval a
       -> (Maybe a, [Interval a])
       -> (Maybe a, [Interval a])
    go (Interval PInf  _    ) _ = error "invertCollection: PInf as lower bound"
    go (Interval (R p) PInf ) _ = (Just p, [])
    go (Interval (R l) h    ) (Just l', xs) = (Just l, Interval (h + 1) (R l' - 1) : xs)
    go (Interval (R _) (R _)) (Nothing, _ ) = error "invertCollection: state violation"

-- | Remove a point from the collection.
--
-- This will split and remove intervals as necessary.
puncture :: (Num a, Ord a) => a -> Collection a -> Maybe (Collection a)
puncture v (Leaf Interval{..})
    | R v < low || R v > high   = Just $! Leaf $ Interval low high
    | R v == low && R v == high = Nothing
    | R v == low                = Just $! Leaf $ Interval (low + 1) high
    | R v == high               = Just $! Leaf $ Interval low (high - 1)
    | otherwise                 = Just $! Branch (Leaf $ Interval low    (v'-1))
                                                 (Leaf $ Interval (v'+1) high  )
  where
    v' = R v

puncture v (Branch left right) =
    case (left', right') of
        (Nothing, Nothing) -> Nothing
        (Just l , Nothing) -> Just l
        (Nothing, Just r ) -> Just r
        (Just l , Just r ) -> Just $! Branch l r
  where
    left'  = puncture v left
    right' = puncture v right
