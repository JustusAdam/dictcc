module DictCC.Util
  ( beIf
  , (-?-)
  , fillZip2
  , slice
  , uncurry3
  ) where


beIf :: a -> Bool -> Maybe a
beIf a True = Just a
beIf _ False = Nothing


(-?-) :: a -> Bool -> Maybe a
(-?-) = beIf


uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
{-# INLINE uncurry3 #-}


fillZip2 :: [a] -> [b] -> [(Maybe a, Maybe b)]
fillZip2 [] [] = []
fillZip2 a [] = zip (map Just a) (repeat Nothing)
fillZip2 [] b = zip (repeat Nothing) (map Just b)
fillZip2 (a:as) (b:bs) = (Just a, Just b) : fillZip2 as bs


slice :: Int -> Int -> [a] -> [a]
slice start end = take (end - start) . drop start
