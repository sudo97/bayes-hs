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

import DBWork
  ( DBWork,
    Only (Only),
    execute,
    ifNoChanges,
    query,
    query_,
    transaction,
    withDefault,
  )
import Data.Aeson (FromJSON)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Pool (Pool)
import qualified Data.Text as T
import qualified Database.SQLite.Simple as S
import DbPool (initConnectionPool, withDbPool)
import GHC.Float (int2Double)
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
import WordMaps (buildSubjProbs, prettyWords)

data Article = Article {articleText :: T.Text, articleSubjects :: [T.Text]} deriving (Show, Generic)

instance FromJSON Article

type BayesAPI =
  "calculate" :> ReqBody '[JSON] T.Text :> Get '[JSON] [(T.Text, Double)]
    :<|> "update" :> ReqBody '[JSON] Article :> Post '[JSON] NoContent
    :<|> "subjects" :> Get '[JSON] [(T.Text, Int)]

allSubjects :: DBWork [(T.Text, Int)]
allSubjects = query_ "SELECT name, qty FROM subjects"

bayesServer :: Pool S.Connection -> Server BayesAPI
bayesServer dbPool = calculate :<|> update :<|> subjects
  where
    calculate text =
      withDbPool dbPool $
        allSubjects >>= traverse (\(subj, _) -> (subj,) . sum <$> traverse (calcBayes subj) (prettyWords text))
    update (Article text subjs) =
      withDbPool dbPool $ traverse (`insertArticle` text) subjs $> NoContent
    subjects = withDbPool dbPool allSubjects

mainRun :: IO ()
mainRun = do
  putStrLn "Starting a server..."
  initConnectionPool "bayes.db" >>= run 8081 . serve @BayesAPI Proxy . bayesServer

calcBayes :: T.Text -> T.Text -> DBWork Double
calcBayes subj word =
  go
    <$> totalWordCountInSubject subj word
    <*> totalWordsInSubject subj
    <*> subjectCount subj
    <*> totalArticlesCount
    <*> totalWordCount word
    <*> totalWords
  where
    go total_word_count_in_subject total_words_in_subject subject_count total_articles_count total_word_count total_words =
      let p_ba = total_word_count_in_subject / total_words_in_subject
          p_a = subject_count / total_articles_count
          p_b = total_word_count / total_words
       in if total_word_count == 0 then 0 else (p_ba * p_a) / p_b

totalWordCountInSubject :: T.Text -> T.Text -> DBWork Double
totalWordCountInSubject subj word =
  int2Double . withDefault 0
    <$> query "SELECT word_subj.qty from word_subj INNER JOIN subjects ON subjects.id = word_subj.subj_id WHERE subjects.name = ? AND word_subj.word = ?" (subj, word)

totalWordsInSubject :: T.Text -> DBWork Double
totalWordsInSubject subj =
  int2Double . withDefault 0
    <$> query "SELECT SUM(word_subj.qty) from word_subj INNER JOIN subjects ON subjects.id = word_subj.subj_id WHERE subjects.name = ?" (Only subj)

subjectCount :: T.Text -> DBWork Double
subjectCount subj =
  int2Double . withDefault 0
    <$> query "SELECT qty FROM subjects WHERE name = ?" (Only subj)

totalArticlesCount :: DBWork Double
totalArticlesCount = int2Double . withDefault 0 <$> query_ "SELECT SUM(qty) FROM subjects"

totalWordCount :: T.Text -> DBWork Double
totalWordCount word = int2Double . withDefault 0 <$> query "SELECT SUM(word_subj.qty) from word_subj INNER JOIN subjects ON subjects.id = word_subj.subj_id WHERE word_subj.word = ?" (Only word)

totalWords :: DBWork Double
totalWords = int2Double . withDefault 0 <$> query_ "SELECT SUM(word_subj.qty) from word_subj INNER JOIN subjects ON subjects.id = word_subj.subj_id"

insertArticle :: T.Text -> T.Text -> DBWork ()
insertArticle subj text = transaction $ do
  subjId <- incrementSubject subj
  traverse_ (insertOrUpdateWordSubj subjId) . buildSubjProbs $ text

incrementSubject :: T.Text -> DBWork Int
incrementSubject subj = do
  execute "UPDATE subjects SET qty = qty + 1 WHERE name = ?" $ Only subj
  ifNoChanges $ execute "INSERT INTO subjects (name, qty) values (?, 1)" $ Only subj
  [[val]] <- query "SELECT id FROM subjects where name = ? " (Only subj)
  pure val

insertOrUpdateWordSubj :: Int -> (T.Text, Int) -> DBWork ()
insertOrUpdateWordSubj subjId (word, qty) = do
  execute "UPDATE word_subj SET qty = qty + ? WHERE word = ? AND subj_id = ?" (qty, word, subjId)
  ifNoChanges $ execute "INSERT INTO word_subj (word, subj_id, qty) values (?, ?, ?)" (word, subjId, qty)
