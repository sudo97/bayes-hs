{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Control.Applicative ((<|>))
import Control.Exception (try)
import Control.Lens
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO (..))
import DBPool (initConnectionPool, withDbPool)
import DBQueries (allSubjects, calcBayes, insertArticle)
import Data.Aeson (toJSON)
import Data.Bifunctor (Bifunctor (first))
import Data.Functor (($>))
import Data.List (sortOn)
import qualified Data.Map as M
import Data.Ord (Down (..))
import Data.Pool (Pool)
import qualified Data.Text as T
import qualified Database.SQLite.Simple as S
import Network.HTTP.Client (HttpException (..))
import Network.Wai.Handler.Warp (run)
import Network.Wreq (asJSON, post, responseBody)
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
import System.Environment (getEnv)
import Types
import WordMaps (prettyWords)

type BayesAPI =
  "calculate" :> ReqBody '[JSON] ArticleText :> Post '[JSON] Probabilities
    :<|> "update" :> ReqBody '[JSON] Article :> PostNoContent
    :<|> "subjects" :> Get '[JSON] TotalSubjCount

fetchFromNormalizer :: String -> [T.Text] -> IO [T.Text]
fetchFromNormalizer address wrds = do
  result <- try @HttpException $ post address (toJSON wrds) >>= asJSON <&> (^. responseBody)
  case result of
    Left _ -> fail "Network error"
    Right r -> pure r

prettifyText :: MonadIO io => String -> T.Text -> io [T.Text]
prettifyText address text =
  let wrds = prettyWords text
   in liftIO $ fetchFromNormalizer address wrds <|> pure wrds

bayesServer :: String -> Pool S.Connection -> Server BayesAPI
bayesServer normalizerUrl dbPool = calculate :<|> update :<|> subjects
  where
    withDb = withDbPool dbPool
    calculate :: ArticleText -> Handler Probabilities
    calculate article = do
      prettified <- prettifyText normalizerUrl $ article ^. articleInnerText
      lst <- withDb $ do
        allSubj <- fmap fst <$> allSubjects
        forM allSubj $ \subj -> do
          probs <- traverse (calcBayes subj) prettified
          pure (SubjectTitle subj, sum probs)
      pure . Probabilities . take 20 . sortOn (Down . snd) $ lst
    update :: Article -> Handler NoContent
    update article = do
      prettified <- prettifyText normalizerUrl $ article ^. articleTextLens . articleInnerText
      withDb $
        traverse
          (insertArticle prettified)
          (article ^. articleSubjectsLens <&> (^. subjectInnerTitle))
          $> NoContent
    subjects :: Handler TotalSubjCount
    subjects = withDb $ TotalSubjCount . M.fromList . fmap (first SubjectTitle) <$> allSubjects

mainRun :: IO ()
mainRun = do
  putStrLn "Starting a server..."
  bayesServer <$> getEnv "NORMALIZER_URL" <*> initConnectionPool "bayes.db"
    >>= run 8081 . serve @BayesAPI Proxy
