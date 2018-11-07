data Nope   = Nope
            deriving (Eq, Show)

instance Semigroup Nope where
    _ <> _ = Nope

instance Monoid Nope where
    mempty = Nope
