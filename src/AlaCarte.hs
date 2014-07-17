module AlaCarte where

--- Haskell encoding of Runars talk on Monad Coproducts and Free Monads
--- https://dl.dropboxusercontent.com/u/4588997/ReasonablyPriced.pdf

data Interact a where
  Ask  :: String -> Interact String
  Tell :: String -> Interact ()

data User = User

data Auth a where
  Login :: (String, String) -> Auth User
  Access :: User -> Auth Bool

type Coproduct f g a = Either (f a) (g a)

data Free f a = Return a | forall i. Bind (f i) (i -> Free f a)

instance Monad (Free f) where
  return            = Return
  (Return a) >>= g  = g a
  (Bind fi g) >>= h = Bind (fi) (\j -> (g j) >>= h)

type NatTrans f g = forall a. f a -> g a

or :: NatTrans f g -> NatTrans h g -> NatTrans (Coproduct f h) g
or fg hg = foldE fg hg

foldE ::  (a -> c) -> (b -> c) -> Either a b -> c
foldE f g e = case e of
                Left a  -> f a
                Right b -> g b

left :: f a -> Coproduct f g a
left  = Left

right :: g a -> Coproduct f g a
right = Right

liftF :: f a -> Free f a
liftF fa = Bind fa Return

ask :: String -> Free Interact String
ask = liftF . Ask

tell :: String -> Free Interact ()
tell = liftF . Tell

