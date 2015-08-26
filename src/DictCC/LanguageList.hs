{-# LANGUAGE OverloadedStrings #-}
module DictCC.LanguageList (languages) where


import           Data.Text


{-|
  A (still incomplete) list of the available languages on dict.cc.

  Used for displaying the language(s) in the header of the result screen.
-}
languages :: [(String, Text)]
languages =
  [ ("en", "English")
  , ("de", "Deutsch")
  , ("fr", "Français")
  , ("eo", "Esperanto")
  , ("bg", "български")
  , ("da", "Dansk")
  , ("el", "ελληνικά")
  , ("is", "Íslennsk")
  , ("sq", "Shqip")
  ]
