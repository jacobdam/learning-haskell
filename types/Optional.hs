data Optional a         = Null
                        | Some a
                        deriving (Eq, Show)

instance (Semigroup a)  => Semigroup (Optional a) where
    Null <> _           = Null
    _ <> Null           = Null
    Some x <> Some y    = Some (x <> y)

instance Functor Optional where
    fmap f (Some a)     = Some (f a)
    fmap f Null         = Null

instance Applicative Optional where
    pure x              = Some x
    Some f <*> Some x   = Some (f x)
    _ <*> _             = Null

instance Monad Optional where
    Some x >>= f        = f x
    Null >>= f          = Null
