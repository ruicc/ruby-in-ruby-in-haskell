module IOClass where



class ShowIO a where
    showIO :: a -> IO String

class EqIO a where
    eqIO :: a -> a -> IO Bool
    neqIO :: a -> a -> IO Bool

    eqIO x y = not <$> neqIO x y
    neqIO x y = not <$> eqIO x y

