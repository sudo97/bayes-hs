{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as TIO
import Control.Monad
import qualified Data.Text as T
import qualified WordMaps

testcases :: [(T.Text, String)]
testcases = [("first", "First.txt"), ("second", "Second.txt")]

readAndInsert :: WordMaps.SubjTree -> (T.Text, FilePath) -> IO WordMaps.SubjTree
readAndInsert tree (subjName, file) = do
  content <- TIO.readFile file 
  pure $ WordMaps.insertSubj subjName content tree

main :: IO ()
main = do
  subj <- foldM readAndInsert mempty testcases

  putStr "mom to first "
  print $ WordMaps.calcProbability "mom" "first" subj
  putStr "mom to second "
  print $ WordMaps.calcProbability "mom" "second" subj
  putStr "money to first "
  print $ WordMaps.calcProbability "money" "first" subj
  putStr "money to second "
  print $ WordMaps.calcProbability "money" "second" subj
  putStr "rich to first "
  print $ WordMaps.calcProbability "rich" "first" subj
  putStr "rich to second "
  print $ WordMaps.calcProbability "rich" "second" subj
