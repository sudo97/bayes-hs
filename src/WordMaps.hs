{-# LANGUAGE OverloadedStrings #-}

module WordMaps where

import Data.Char (isLower)
import Data.Foldable (Foldable (foldl'))
import qualified Data.Map.Strict as M
import qualified Data.Text as T

type SubjProbs = M.Map T.Text Int

insertWords :: T.Text -> SubjProbs -> SubjProbs
insertWords wrds p = foldl' go p (prettyWords wrds)
  where
    go :: SubjProbs -> T.Text -> SubjProbs
    go pr txt = M.insertWith (+) txt 1 pr

prettyWords :: T.Text -> [T.Text]
prettyWords = filter (/= "") . fmap (T.filter isLower . T.toLower) . T.words

buildSubjProbs :: T.Text -> [(T.Text, Int)]
buildSubjProbs wrds = M.toList $ insertWords wrds mempty
