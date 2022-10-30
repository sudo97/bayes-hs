{-# LANGUAGE OverloadedStrings #-}

module WordMaps where

import Data.Char (isLower)
import Data.Foldable (Foldable (foldl'))
import Data.Function (on)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import GHC.Float (int2Double)

type SubjProbs = M.Map T.Text Int

insertWords :: T.Text -> SubjProbs -> SubjProbs
insertWords wrds p = foldl' go p (prettyWords wrds)
  where
    go :: SubjProbs -> T.Text -> SubjProbs
    go probs txt = M.insertWith (+) txt 1 probs
    prettyWords :: T.Text -> [T.Text]
    prettyWords = filter (/= "") . fmap (T.filter isLower . T.toLower) . T.words

type SubjTree = M.Map T.Text (Int, SubjProbs)

insertSubj :: T.Text -> T.Text -> SubjTree -> SubjTree
insertSubj key wrds tree =
  let (count, probs) = fromMaybe (0, M.empty) $ M.lookup key tree
      st' = (count + 1, insertWords wrds probs)
   in M.insert key st' tree

calcProbability :: T.Text -> T.Text -> SubjTree -> Double
calcProbability word subject subjTree = (p_ba * p_a) / p_b
  where
    p_ba = on (/) int2Double total_word_count_in_subject total_words_in_subject
    p_a = on (/) int2Double subject_count total_articles_count
    p_b = on (/) int2Double total_word_count total_words

    subj = M.lookup subject subjTree
    lookupWord = M.lookup word . snd

    total_word_count_in_subject = fromMaybe 0 $ subj >>= lookupWord
    total_words_in_subject = maybe 1 (sum . snd) subj
    subject_count = maybe 0 fst subj
    total_articles_count = sum $ fst <$> subjTree
    total_word_count = sum $ fromMaybe 0 . lookupWord <$> subjTree
    total_words = sum $ sum . snd <$> subjTree

empty :: SubjTree
empty = M.empty