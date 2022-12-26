{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module BayesServer (mainRun) where

import DBPool (initConnectionPool, withDbPool)
import DBQueries (allSubjects, calcBayes, insertArticle)
import Data.Aeson (FromJSON)
import Data.Functor (($>))
import Data.Pool (Pool)
import qualified Data.Text as T
import qualified Database.SQLite.Simple as S
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant
  ( Get,
    JSON,
    NoContent (..),
    Post,
    Proxy (Proxy),
    ReqBody,
    Server,
    serve,
    type (:<|>) (..),
    type (:>),
  )
import WordMaps (prettyWords)

data Article = Article {articleText :: T.Text, articleSubjects :: [T.Text]} deriving (Show, Generic)

instance FromJSON Article

type BayesAPI =
  "calculate" :> ReqBody '[JSON] T.Text :> Get '[JSON] [(T.Text, Double)]
    :<|> "update" :> ReqBody '[JSON] Article :> Post '[JSON] NoContent
    :<|> "subjects" :> Get '[JSON] [(T.Text, Int)]

bayesServer :: Pool S.Connection -> Server BayesAPI
bayesServer dbPool = calculate :<|> update :<|> subjects
  where
    calculate text =
      let prettified = prettyWords text
       in withDbPool dbPool $
            allSubjects >>= traverse (\(subj, _) -> (subj,) . sum <$> traverse (calcBayes subj) prettified)
    update (Article text subjs) =
      withDbPool dbPool $ traverse (insertArticle text) subjs $> NoContent
    subjects = withDbPool dbPool allSubjects

mainRun :: IO ()
mainRun = do
  putStrLn "Starting a server..."
  initConnectionPool "bayes.db" >>= run 8081 . serve @BayesAPI Proxy . bayesServer
