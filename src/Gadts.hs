data Term a where
  Lit :: Int ->  Term Int
  Pair :: Term a -> Term b -> Term (a,b)

{- Phantom Types -}
data Expr a = Concat (Expr String) (Expr String) | Add (Expr Int) (Expr Int) | Val a deriving (Show)

eval :: Expr Int -> Int
eval (Val i) = i
eval (Add e1 e2) = (eval e1) + (eval e2)

evalS :: Expr String -> String
evalS (Val s) = s
evalS (Concat e1 e2) = (evalS e1) ++ (evalS e2)





data Lam :: * -> * where
  Lift :: a                     -> Lam a
  Tup  :: Lam a -> Lam b        -> Lam (a, b)
  Lam  :: (Lam a -> Lam b)      -> Lam (a -> b)
  App  :: Lam (a -> b) -> Lam a -> Lam b
  Fix  :: Lam (a -> a)          -> Lam a
