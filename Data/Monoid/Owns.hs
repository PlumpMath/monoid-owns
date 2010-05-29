module Data.Monoid.Owns where

import Prelude hiding ((+))
import qualified Prelude as Prelude

class Monoid a where
        mempty  :: a
        -- ^ Identity of 'mappend'
        mappend :: a -> a -> a
        -- ^ An associative operation
        mconcat :: [a] -> a

        -- ^ Fold a list using the monoid.
        -- For most types, the default definition for 'mconcat' will be
        -- used, but the function is included in the class definition so
        -- that an optimized version can be provided for specific types.

        mconcat = foldr mappend mempty


(+) :: (Monoid a) => a -> a -> a
(+) = mappend

infixl 6 +


-- Monoid instances.

instance Monoid [a] where
        mempty  = []
        mappend = (++)

instance Monoid b => Monoid (a -> b) where
        mempty _ = mempty
        mappend f g x = f x `mappend` g x

instance Monoid () where
        -- Should it be strict?
        mempty        = ()
        _ `mappend` _ = ()
        mconcat _     = ()

instance (Monoid a, Monoid b) => Monoid (a,b) where
        mempty = (mempty, mempty)
        (a1,b1) `mappend` (a2,b2) =
                (a1 `mappend` a2, b1 `mappend` b2)

instance (Monoid a, Monoid b, Monoid c) => Monoid (a,b,c) where
        mempty = (mempty, mempty, mempty)
        (a1,b1,c1) `mappend` (a2,b2,c2) =
                (a1 `mappend` a2, b1 `mappend` b2, c1 `mappend` c2)

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (a,b,c,d) where
        mempty = (mempty, mempty, mempty, mempty)
        (a1,b1,c1,d1) `mappend` (a2,b2,c2,d2) =
                (a1 `mappend` a2, b1 `mappend` b2,
                 c1 `mappend` c2, d1 `mappend` d2)

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) =>
                Monoid (a,b,c,d,e) where
        mempty = (mempty, mempty, mempty, mempty, mempty)
        (a1,b1,c1,d1,e1) `mappend` (a2,b2,c2,d2,e2) =
                (a1 `mappend` a2, b1 `mappend` b2, c1 `mappend` c2,
                 d1 `mappend` d2, e1 `mappend` e2)

-- lexicographical ordering
instance Monoid Ordering where
        mempty         = EQ
        LT `mappend` _ = LT
        EQ `mappend` y = y
        GT `mappend` _ = GT


instance Num a => Monoid (a) where
        mempty = Sum 0
        Sum x `mappend` Sum y = Sum (x + y)

instance Monoid (Maybe a) where
  mempty = Nothing
  Nothing `mappend` m = m
  Just m `mappend` _ = m


