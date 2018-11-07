data Array a            = Empty | Array {first :: a, rest :: Array a}
                        deriving (Eq, Show)

class ArrayConvertible t where
    arrayFrom :: t a -> Array a
    fromArray :: Array a -> a

amap                                    :: (a -> b) -> Array a -> Array b
amap f Empty                            = Empty
amap f Array {first = x, rest = xs }    = Array {first = f x, rest = fmap f xs}

aappend                                     :: Array a -> Array a -> Array a
aappend Empty ys                            = ys
aappend Array {first = x, rest = xs } ys    = Array {first = x, rest = (aappend xs ys)}

aconcat                                 :: Array (Array a) -> Array a
aconcat Empty                           = Empty
aconcat Array {first = xs, rest = xss } = aappend xs (aconcat xss)

instance ArrayConvertible [] where
    arrayFrom [] = Empty
    arrayFrom (x : xs) = Array {first = x, rest = arrayFrom xs}

    fromArray Empty = []
    fromArray { first = x, rest = xs } = x : fromArray xs

instance Semigroup (Array a) where
    (<>) = aappend

instance Monoid (Array a) where
    mempty = Empty

instance Functor Array where
    fmap = amap

instance Applicative Array where
    pure x      = Array {first = x, rest = Empty}
    fs <*> xs   = aconcat (amap (\f -> amap f xs) fs)

instance Monad Array where
    xs >>= f    = aconcat (amap f xs)
    data Array a            = Empty | Array {first :: a, rest :: Array a}
    deriving (Eq, Show)
