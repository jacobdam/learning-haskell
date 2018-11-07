newtype Identity a = Identity {getValue :: a}
                deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
    Identity x <> Identity y = Identity (x <> y)

instance (Monoid a) => Monoid (Identity a) where
    mempty  = Identity mempty

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
    pure x = Identity x
    Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
    Identity x >>= f = f x
