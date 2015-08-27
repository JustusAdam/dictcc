{-# LANGUAGE UnicodeSyntax #-}
module DictCC.Util
  ( beIf
  , fillZip2
  , slice
  , uncurry3
  , stuff2
  , joinWith
  ) where


import           Data.Monoid
import           Data.Monoid.Unicode
import           Prelude.Unicode


{-|
  Guard the presence of any value depending on a boolean.
-}
beIf ∷ α → Bool → Maybe α
beIf a True  = Just a
beIf _ False = Nothing


{-|
  Like `uncurry` but for 3-tuples.
-}
uncurry3 ∷ (α → β → γ → δ) → (α, β, γ) → δ
uncurry3 f (a, b, c) = f a b c
{-# INLINE uncurry3 #-}


{-|
  Like 'zip' but zips until the end of the longer of the two lists, filling non existent entries with 'Nothing'.
-}
fillZip2 ∷ [α] → [β] → [(Maybe α, Maybe β)]
fillZip2 [] [] = []
fillZip2 a [] = zip (map Just a) (repeat Nothing)
fillZip2 [] b = zip (repeat Nothing) (map Just b)
fillZip2 (a:as) (b:bs) = (Just a, Just b) : fillZip2 as bs


{-|
  Slice a list.

  Returns [] or a shortened result list if the indexes are out of bounds.
  As such there is no guarantee that the resulting list will be of length end - start.
-}
slice ∷ Int → Int → [α] → [α]
slice start end = take (end - start) ∘ drop start


{-|
  Call a function that expects two arguments of the same type with a single value in both places.
-}
stuff2 ∷ (α → α → β) → α → β
stuff2 f a = f a a


{-|
  Join two monoidal values using a third one in their middle. The first argument to this function is the interspersed one.
-}
joinWith ∷ Monoid α ⇒ α → α → α → α
joinWith = ((⊕) ∘) ∘ flip (⊕)
{-# INLINE joinWith #-}
