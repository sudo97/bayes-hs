{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as TIO
import qualified WordMaps

testcases :: [String]
testcases = ["First.txt", "Second.txt"]

main :: IO ()
main = do
  [first, second] <- mapM TIO.readFile testcases
  let subj = WordMaps.insertSubj "second" second $ WordMaps.insertSubj "first" first WordMaps.empty
  putStr "mom to first "
  print $ WordMaps.calcProbability "mom" "first" subj
  putStr "mom to second "
  print $ WordMaps.calcProbability "mom" "second" subj
  putStr "money to first "
  print $ WordMaps.calcProbability "money" "first" subj
  putStr "money to second "
  print $ WordMaps.calcProbability "money" "second" subj
