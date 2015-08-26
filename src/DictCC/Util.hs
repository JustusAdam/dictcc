module DictCC.Util
  ( beIf
  , (-?-)
  , fillZip2
  , slice
  , uncurry3
  ) where


{-|
  Guard the presence of any value depending on a boolean.
-}
beIf :: a -> Bool -> Maybe a
beIf a True = Just a
beIf _ False = Nothing


(-?-) :: a -> Bool -> Maybe a
(-?-) = beIf


{-|
  Like `uncurry` but for 3-tuples.
-}
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
{-# INLINE uncurry3 #-}


{-|
  Like 'zip' but zips until the end of the longer of the two lists, filling non existent entries with 'Nothing'.
-}
fillZip2 :: [a] -> [b] -> [(Maybe a, Maybe b)]
fillZip2 [] [] = []
fillZip2 a [] = zip (map Just a) (repeat Nothing)
fillZip2 [] b = zip (repeat Nothing) (map Just b)
fillZip2 (a:as) (b:bs) = (Just a, Just b) : fillZip2 as bs


{-|
  Slice a list.

  Returns [] or a shortened result list if the indexes are out of bounds.
  As such there is no guarantee that the resulting list will be of length end - start.
-}
slice :: Int -> Int -> [a] -> [a]
slice start end = take (end - start) . drop start
