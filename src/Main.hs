{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Main (main) where


import           Control.Applicative.Unicode
import           Control.Arrow
import           Control.Arrow.Unicode
import           Control.Monad.Unicode
import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Monoid.Unicode
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import           DictCC.Serialize
import           DictCC.Util
import           Network.HTTP.Base           (urlEncode, urlEncodeVars)
import           Network.HTTP.Conduit        (simpleHttp)
import           Prelude.Unicode
import           System.Environment
import           Text.HTML.DOM               (parseLBS)
import           Text.XML.Cursor             (fromDocument)


{-|
  Width of a display cell in the result screen.
-}
formattedChunkLength = 30


{-|
  Main function.
-}
main ∷ IO ()
main = do
  args ← getArgs

  case args of
    [from, to, vocab] → verifyInputs from to vocab
    [fromto, vocab]   →
      let
        (from, to) = splitAt 2 fromto
      in
        verifyInputs from to vocab

    [vocab]           → verifyInputs "en" "de" vocab
    _                 → putStrLn "Wrong number of arguments"

  where
    verifyShorthand = (∧) <$> isLetter ⊛ isAscii

    verify message pred = beIf message ∘ not ∘ all pred


    {-|
      Verify data input by user and either start the search or report errors.
    -}
    verifyInputs from to vocab =
      let
        verified = mapMaybe (uncurry3 verify)
          [ ("Source language shorthand can only contain ascii letter characters", verifyShorthand, from)
          , ("Target language shorthand can only contain ascii letter characters", verifyShorthand, to)
          , ("Word to search for can only contain letter characters", (∨) <$> isLetter ⊛ isSpace, vocab)
          ]
      in
        if null verified
          then mainProcedure from to vocab
          else
            putStrLn "There was an issue with the input data:" ≫
            traverse_ putStrLn verified

{-|
  Main IO.
-}
mainProcedure from to vocab =
  (parseLBS <$> simpleHttp url) ≫=
  either
    TIO.putStrLn
    ( TIO.putStr ∘ T.concat ∘ uncurry (:) ∘
      ( uncurry formatVocabPair
        ⁂
        ( (stuff2 formatVocabPair headerUnderline :)
        ∘ map (uncurry formatVocabPair)
        )
      )
    )
    ∘ handlePage ∘ fromDocument
  where
    headerUnderline = T.replicate formattedChunkLength "="
    fm = fromMaybe (T.pack $ replicate formattedChunkLength ' ')
    url =
      "http://"
      ⊕ urlEncode (map toLower (from ⊕ to))
      ⊕ ".dict.cc/?"
      ⊕ urlEncodeVars [("s", vocab)]

    formatOneVocab =
      map (T.justifyLeft formattedChunkLength ' ')
      ∘ T.chunksOf formattedChunkLength
    formatVocabPair v1 v2 =
      T.unlines
      ∘ fmap (uncurry (joinWith " | ") ∘ (fm ⁂ fm))
      $ fillZip2 (formatOneVocab v1) (formatOneVocab v2)
