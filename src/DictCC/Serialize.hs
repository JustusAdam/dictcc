{-# LANGUAGE OverloadedStrings #-}
module DictCC.Serialize (handlePage) where


import           Data.Bool
import qualified Data.Text       as T
import           DictCC.Util     (slice)
import           Text.XML.Cursor hiding (bool)


{-|
  Extract translations from the central <table> element on the HTML page.
-}
findTranslations :: Cursor -> [(T.Text, T.Text)]
findTranslations =
  fmap
    ( (\[a, b] -> (a, b))
    . fmap (T.intercalate " " . ($/ ((element "a" &/ orSelf (element "b" >=> child)) >=> content)))
    . slice 1 3
    . child
    )
    . ($// (element "tr" >=> hasAttribute "id"))


{-|
  Attempts to parse the provided HTML and extract a list of translations.
-}
handlePage :: Cursor -> Either T.Text [(T.Text, T.Text)]
handlePage c =
  let
    main = attributeIs "id" "maincontent" &/ element "table"
  in
    case c $// main of
      [_, transTable, _] ->
        bool (Left "No translations found") <$> return <*> not . null $ findTranslations transTable
      _ -> Left "No translations found"
