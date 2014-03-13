data Foo = forall a. MkFoo a (a -> Bool) | Nil


x = [MkFoo 3 (== 3), MkFoo "foobar" ((== 3) . length)]

data Baz = forall a. Eq a => Baz1 a a | forall b. Show b => Baz2 b (b -> b)


--- Not Allowed:
--- bar :: (a -> a) -> (Char, Bool)

--- Allowed Rank-2 Polymorphism
foo :: (forall a. a -> a) -> (Char,Bool)
foo f = (f 'c', f True)

main :: IO ()
main = do print $ foo id
