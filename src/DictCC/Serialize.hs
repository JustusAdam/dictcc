{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module DictCC.Serialize (handlePage) where


import           Control.Applicative.Unicode
import           Control.Arrow
import           Control.Monad
import           Data.Bool
import qualified Data.Text                   as T
import           DictCC.Util                 (slice)
import           Prelude.Unicode
import           Text.XML.Cursor             hiding (bool)


type TranslationTable = ((T.Text, T.Text), [(T.Text, T.Text)])


{-|
  Extract translations from the central <table> element on the HTML page.
-}
findTranslations ∷ Cursor → Either T.Text TranslationTable
findTranslations =
  ( bool
    <$> return ∘
        ( listToTuple
          ∘ join
          ∘ fmap ($/ contentsOfBTags)
          ∘ slice 1 3
          ∘ ($/ element "td")
          ∘ head
        &&&
          fmap
            ( listToTuple
            ∘ fmap (T.intercalate " " ∘ ($/ contentsOfAAndNestedBTags))
            ∘ slice 1 3
            ∘ child
            )
          ∘ contentRows
        )
    ⊛ const (Left "No translations found, sorry.")
    ⊛ null ∘ contentRows
  )
  ∘ ($// element "tr")
  where
    contentRows = join ∘ fmap ($| hasAttribute "id")
    contentsOfBTags = (element "b" &/ orSelf (element "a" >=> child)) >=> content
    contentsOfAAndNestedBTags = (element "a" &/ orSelf (element "b" >=> child)) >=> content
    listToTuple [a, b] = (a, b)


{-|
  Attempts to parse the provided HTML and extract a list of translations.
-}
handlePage ∷ Cursor → Either T.Text TranslationTable
handlePage c =
  let
    main = attributeIs "id" "maincontent" &/ element "table"
  in
    case c $// main of
      [_, transTable, _] →
        findTranslations transTable
      _ → Left "No translations found"
