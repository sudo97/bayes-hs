{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DocsGen where

import BayesServer
  ( Article (Article),
    ArticleText,
    BayesAPI,
    Probabilities (..),
    SubjectTitle,
    TotalSubjCount (..),
  )
import qualified Data.Map as M
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Servant.Docs (API, ToSample (..), docs, markdown)

instance ToSample ArticleText where
  toSamples :: Proxy ArticleText -> [(T.Text, ArticleText)]
  toSamples _ = [("This should be just JSON-encoded strnig with an article text", "Dad is riding a bike")]

instance ToSample Probabilities where
  toSamples :: Proxy Probabilities -> [(T.Text, Probabilities)]
  toSamples _ = [("Probabilities for each subject", Probabilities . M.fromList $ [("subj1", 0.3), ("subj2", 0.8)])]

instance ToSample SubjectTitle where
  toSamples :: Proxy SubjectTitle -> [(T.Text, SubjectTitle)]
  toSamples _ = [("Subject is a unique string", "subj1")]

instance ToSample Article where
  toSamples :: Proxy Article -> [(T.Text, Article)]
  toSamples _ = [("Article Object with text and a list of subjects", Article "Dad is riding a bike" ["subj1", "subj2"])]

instance ToSample TotalSubjCount where
  toSamples :: Proxy TotalSubjCount -> [(T.Text, TotalSubjCount)]
  toSamples _ = [("This is just a number of articles per subject", TotalSubjCount . M.fromList $ [("subj1", 3), ("subj2", 5)])]

apiDocs :: API
apiDocs = docs (Proxy :: Proxy BayesAPI)

md :: String
md = markdown apiDocs