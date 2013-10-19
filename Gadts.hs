data Term a where
  Lit :: Int ->  Term Int
  Pair :: Term a -> Term b -> Term (a,b)


data Lam :: * -> * where
  Lift :: a                     -> Lam a
  Tup  :: Lam a -> Lam b        -> Lam (a, b)
  Lam  :: (Lam a -> Lam b)      -> Lam (a -> b)
  App  :: Lam (a -> b) -> Lam a -> Lam b
  Fix  :: Lam (a -> a)          -> Lam a
