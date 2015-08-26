{-# LANGUAGE OverloadedStrings #-}
module DictCC.Serialize (handlePage) where


import           Control.Arrow
import           Control.Monad
import           Data.Bool
import qualified Data.Text       as T
import           DictCC.Util     (slice)
import           Text.XML.Cursor hiding (bool)


type TranslationTable = ((T.Text, T.Text), [(T.Text, T.Text)])


{-|
  Extract translations from the central <table> element on the HTML page.
-}
findTranslations :: Cursor -> TranslationTable
findTranslations =
  ( listToTuple
    . slice 1 3
    . ($// contentsOfBTags)
    . head
  &&&
    fmap
      ( listToTuple
      . fmap (T.intercalate " " . ($/ contentsOfAAndNestedBTags))
      . slice 1 3
      . child
      )
    . join
    . fmap ($| hasAttribute "id")
  )
  . ($// element "tr")
  where
    contentsOfBTags = (element "b" >=> child) >=> content
    contentsOfAAndNestedBTags = (element "a" &/ orSelf (element "b" >=> child)) >=> content
    listToTuple [a, b] = (a, b)


{-|
  Attempts to parse the provided HTML and extract a list of translations.
-}
handlePage :: Cursor -> Either T.Text TranslationTable
handlePage c =
  let
    main = attributeIs "id" "maincontent" &/ element "table"
  in
    case c $// main of
      [_, transTable, _] ->
        bool (Left "No translations found") <$> return <*> not . null $ findTranslations transTable
      _ -> Left "No translations found"
