{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module BayesServer
  ( mainRun,
    BayesAPI,
    Article (..),
    ArticleText,
    SubjectTitle,
    Probabilities (..),
    TotalSubjCount (..),
  )
where

import DBPool (initConnectionPool, withDbPool)
import DBQueries (allSubjects, calcBayes, insertArticle)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Bifunctor (Bifunctor (first))
import Data.Functor (($>))
import qualified Data.Map as M
import Data.Pool (Pool)
import Data.String (IsString)
import qualified Data.Text as T
import qualified Database.SQLite.Simple as S
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant
  ( Get,
    Handler,
    JSON,
    NoContent (..),
    Post,
    PostNoContent,
    Proxy (Proxy),
    ReqBody,
    Server,
    serve,
    type (:<|>) (..),
    type (:>),
  )
import WordMaps (prettyWords)

newtype ArticleText = ArticleText T.Text deriving (Show, Generic, IsString)

instance FromJSON ArticleText

instance ToJSON ArticleText

newtype SubjectTitle = SubjectTitle T.Text deriving (Show, Generic, IsString, Eq, Ord)

instance FromJSON SubjectTitle

instance FromJSONKey SubjectTitle

instance ToJSONKey SubjectTitle

instance ToJSON SubjectTitle

data Article = Article {articleText :: ArticleText, articleSubjects :: [SubjectTitle]} deriving (Show, Generic)

instance FromJSON Article

instance ToJSON Article

newtype Probabilities = Probabilities (M.Map SubjectTitle Double) deriving (Show, Generic)

instance FromJSON Probabilities

instance ToJSON Probabilities

newtype TotalSubjCount = TotalSubjCount (M.Map SubjectTitle Int) deriving (Show, Generic)

instance FromJSON TotalSubjCount

instance ToJSON TotalSubjCount

type BayesAPI =
  "calculate" :> ReqBody '[JSON] ArticleText :> Post '[JSON] Probabilities
    :<|> "update" :> ReqBody '[JSON] Article :> PostNoContent
    :<|> "subjects" :> Get '[JSON] TotalSubjCount

bayesServer :: Pool S.Connection -> Server BayesAPI
bayesServer dbPool = calculate :<|> update :<|> subjects
  where
    calculate :: ArticleText -> Handler Probabilities
    calculate (ArticleText text) =
      let prettified = prettyWords text
       in Probabilities . M.fromList
            <$> withDbPool
              dbPool
              ( allSubjects >>= traverse (\(subj, _) -> (SubjectTitle subj,) . sum <$> traverse (calcBayes subj) prettified)
              )
    update (Article (ArticleText text) subjs) =
      let subjNames = (\(SubjectTitle t) -> t) <$> subjs
       in withDbPool dbPool $ traverse (insertArticle text) subjNames $> NoContent
    subjects :: Handler TotalSubjCount
    subjects = withDbPool dbPool $ TotalSubjCount . M.fromList . fmap (first SubjectTitle) <$> allSubjects

mainRun :: IO ()
mainRun = do
  putStrLn "Starting a server..."
  initConnectionPool "bayes.db" >>= run 8081 . serve @BayesAPI Proxy . bayesServer
