{-# LANGUAGE OverloadedStrings #-}
module Main (main) where


import           Data.Char
import           Data.Foldable
import           Data.Maybe
import DictCC
import System.Environment
import DictCC.Util


{-|
  Main function.
-}
main :: IO ()
main = do
  args <- getArgs

  case args of
    [from, to, vocab] -> verifyInputs from to vocab
    [fromto, vocab]   ->
      let
        (from, to) = splitAt 2 fromto
      in
        verifyInputs from to vocab

    [vocab]           -> verifyInputs "en" "de" vocab
    _                 -> putStrLn "Wrong number of arguments"

  where
    verifyShorthand = (&&) <$> isLetter <*> isAscii

    verify message pred = beIf message . not . all pred


    {-|
      Verify data input by user and either start the search or report errors.
    -}
    verifyInputs from to vocab =
      let
        verified = mapMaybe (uncurry3 verify)
          [ ("Source language shorthand can only contain ascii letter characters", verifyShorthand, from)
          , ("Target language shorthand can only contain ascii letter characters", verifyShorthand, to)
          , ("Word to search for can only contain letter characters", (||) <$> isLetter  <*> isSpace, vocab)
          ]
      in
        if null verified
          then translate from to vocab
          else
            putStrLn "There was an issue with the input data:" >>
            traverse_ putStrLn verified
