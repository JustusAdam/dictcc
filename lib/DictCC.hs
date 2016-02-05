{-# LANGUAGE OverloadedStrings #-}
module DictCC (translate) where

import           Control.Arrow
import           Data.Char
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import           DictCC.Serialize
import           DictCC.Util
import           Network.HTTP.Base           (urlEncode, urlEncodeVars)
import           Network.HTTP.Conduit        (simpleHttp)
import           Text.HTML.DOM               (parseLBS)
import           Text.XML.Cursor             (fromDocument)



{-|
  Width of a display cell in the result screen.
-}
formattedChunkLength = 30


translate :: [Char] -> [Char] -> String -> IO ()
translate from to vocab =
  (parseLBS <$> simpleHttp url) >>=
  either
    TIO.putStrLn
    ( TIO.putStr . T.concat . uncurry (:) .
      ( uncurry formatVocabPair
      ***
        ( (stuff2 formatVocabPair headerUnderline :)
        . map (uncurry formatVocabPair)
        )
      )
    )
    . handlePage . fromDocument
  where
    headerUnderline = T.replicate formattedChunkLength "="
    fm = fromMaybe (T.pack $ replicate formattedChunkLength ' ')
    url =
      "http://"
      <> urlEncode (map toLower (from <> to))
      <> ".dict.cc/?"
      <> urlEncodeVars [("s", vocab)]

    formatOneVocab =
      map (T.justifyLeft formattedChunkLength ' ')
      . T.chunksOf formattedChunkLength
    formatVocabPair v1 v2 =
      T.unlines
      . fmap (uncurry (joinWith " | ") . (fm *** fm))
      $ fillZip2 (formatOneVocab v1) (formatOneVocab v2)
