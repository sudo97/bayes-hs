{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import BayesServer
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import DBWork
import Data.Foldable (traverse_)
import qualified Data.Text as T
import GHC.Float (int2Double)
import WordMaps (buildSubjProbs)

incrementSubject :: T.Text -> DBWork ()
incrementSubject subj = do
  execute "UPDATE subjects SET qty = qty + 1 WHERE name = ?" $ Only subj
  number <- changes
  when (number == 0) $ execute "INSERT INTO subjects (name, qty) values (?, 1)" $ Only subj

printSubjects :: DBWork ()
printSubjects = query_ @(T.Text, Int) "SELECT name, qty FROM subjects" >>= traverse_ (liftIO . print)

getSubjCount :: T.Text -> DBWork Int
getSubjCount subj = withDefault 0 <$> query "SELECT qty FROM subjects WHERE name = ?" (Only subj)

getSubjId :: T.Text -> DBWork (Maybe Int)
getSubjId subj = do
  [[val]] <- query "SELECT id FROM subjects where name = ?" (Only subj)
  pure val

getAllSubjCount :: DBWork Int
getAllSubjCount = withDefault 0 <$> query_ "SELECT SUM(qty) FROM subjects"

insertOrUpdateWordSubj :: Int -> (T.Text, Int) -> DBWork ()
insertOrUpdateWordSubj subjId (word, qty) = do
  execute "UPDATE word_subj SET qty = qty + ? WHERE word = ? AND subj_id = ?" (qty, word, subjId)
  number <- changes
  when (number == 0) $ execute "INSERT INTO word_subj (word, subj_id, qty) values (?, ?, ?)" (word, subjId, qty)

insertArticle :: T.Text -> T.Text -> DBWork ()
insertArticle subj t = transaction $ do
  incrementSubject subj
  -- will always pattern-match as incrementSubject always does insert, if unable to update
  (Just subjId) <- getSubjId subj
  traverse_ (insertOrUpdateWordSubj subjId) . buildSubjProbs $ t

main :: IO ()
main = mainRun

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