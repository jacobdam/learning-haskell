data Tree a = Leaf a | Branch { left: Tree a, right: Tree a }

instance Functor Tree where
    fmap f (Leaf x)                     = Leaf (f a)
    fmap f Branch {left: xs, right: ys} = Branch {left: f xs, right: f ys}

instance Monad Tree where
    Leaf x >>= f                        = f x
    Branch {left: xs, right: ys} >>= f  = Branch {left: f xs, right: f ys}
