{-# LANGUAGE OverloadedStrings #-}
module Main (main) where


import           Control.Arrow
import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Debug.Trace
import           Network.HTTP.Base    (urlEncode, urlEncodeVars)
import           Network.HTTP.Conduit (simpleHttp)
import           System.Environment
import           Text.HTML.DOM        (parseLBS)
import           Text.XML.Cursor


languages =
  [ ("en", "English")
  , ("de", "Deutsch")
  , ("sq", "Albanian")]


formattedChunkLength = 30


fillZip2 :: [a] -> [b] -> [(Maybe a, Maybe b)]
fillZip2 [] [] = []
fillZip2 a [] = zip (map Just a) (repeat Nothing)
fillZip2 [] b = zip (repeat Nothing) (map Just b)
fillZip2 (a:as) (b:bs) = (Just a, Just b) : fillZip2 as bs


slice :: Int -> Int -> [a] -> [a]
slice start end = take (end - start) . drop start


findTranslations :: Cursor -> [(T.Text, T.Text)]
findTranslations =
  fmap
    ( (\[a, b] -> (a, b))
      . fmap (T.intercalate " " . ($/ ((element "a" &/ orSelf (element "b" >=> child)) >=> content)))
      . slice 1 3
      . child
    )
    . ($// (element "tr" >=> hasAttribute "id"))


handlePage :: Cursor -> Either T.Text [(T.Text, T.Text)]
handlePage c =
  let
    main = attributeIs "id" "maincontent" &/ element "table"
  in
    case c $// main of
      [_, transTable, _] ->
        return $ findTranslations transTable
      _ -> Left "No translations found"


main :: IO ()
main = do
  args <- getArgs

  case args of
    [from, to, vocab] -> main' from to vocab
    [fromto, vocab] ->
      let
        (from, to) = splitAt 2 fromto
      in
        main' from to vocab
    [vocab] -> main' "en" "de" vocab
    _ -> putStrLn "Wrong number of arguments"

  where
    main' from to vocab = do
      page <- parseLBS <$> simpleHttp url
      either
        TIO.putStrLn
        ( TIO.putStr
          . T.concat
          . (formatVocabPair (getLang from) (getLang to) :)
          . (T.unlines [T.pack (replicate (formattedChunkLength * 2) '=')] :)
          . map (uncurry formatVocabPair))
        $ handlePage (fromDocument page)
      where
        getLang lang = fromMaybe "Unknown Language" (lookup lang languages)
        fm = fromMaybe (T.pack $ replicate formattedChunkLength ' ')
        url = "http://" <> urlEncode (map toLower (from <> to)) <> ".dict.cc/?" <> urlEncodeVars [("s", vocab)]
        formatOneVocab = map (T.justifyLeft formattedChunkLength ' ') . T.chunksOf formattedChunkLength
        formatVocabPair v1 v2 = T.unlines . fmap (uncurry (<>) . (fm *** fm)) $ fillZip2 (formatOneVocab v1) (formatOneVocab v2)
