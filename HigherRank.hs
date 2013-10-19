foo :: (forall a. a -> a) -> (Char,Bool)
foo f = (f 'c', f True)

bar :: forall a. ((a -> a) -> (Char, Bool))
bar f = ('c', True)
