{-# LANGUAGE OverloadedStrings #-}

module DBQueries (allSubjects, calcBayes, insertArticle) where

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
import Data.Foldable (traverse_)
import qualified Data.Text as T
import GHC.Float (int2Double)
import WordMaps (buildSubjProbs)

allSubjects :: DBWork [(T.Text, Int)]
allSubjects = query_ "SELECT name, qty FROM subjects"

insertArticle :: [T.Text] -> T.Text -> DBWork ()
insertArticle text subj = transaction $ do
  subjId <- incrementSubject
  traverse_ (insertOrUpdateWordSubj subjId) . buildSubjProbs $ text
  where
    insertOrUpdateWordSubj :: Int -> (T.Text, Int) -> DBWork ()
    insertOrUpdateWordSubj subjId (word, qty) = do
      execute "UPDATE word_subj SET qty = qty + ? WHERE word = ? AND subj_id = ?" (qty, word, subjId)
      ifNoChanges $ execute "INSERT INTO word_subj (word, subj_id, qty) values (?, ?, ?)" (word, subjId, qty)
    incrementSubject :: DBWork Int
    incrementSubject = do
      execute "UPDATE subjects SET qty = qty + 1 WHERE name = ?" $ Only subj
      ifNoChanges $ execute "INSERT INTO subjects (name, qty) values (?, 1)" $ Only subj
      [[val]] <- query "SELECT id FROM subjects where name = ? " (Only subj)
      pure val

calcBayes :: T.Text -> T.Text -> DBWork Double
calcBayes subj word =
  go
    <$> totalWordCountInSubject
    <*> totalWordsInSubject
    <*> subjectCount
    <*> totalArticlesCount
    <*> totalWordCount
    <*> totalWords
  where
    go total_word_count_in_subject total_words_in_subject subject_count total_articles_count total_word_count total_words =
      let p_ba = total_word_count_in_subject / total_words_in_subject
          p_a = subject_count / total_articles_count
          p_b = total_word_count / total_words
       in if total_word_count == 0 then 0 else (p_ba * p_a) / p_b
    totalWords :: DBWork Double
    totalWords =
      int2Double . withDefault 0
        <$> query_ "SELECT SUM(word_subj.qty) FROM word_subj INNER JOIN subjects ON subjects.id = word_subj.subj_id"

    totalWordCountInSubject :: DBWork Double
    totalWordCountInSubject =
      int2Double . withDefault 0
        <$> query "SELECT word_subj.qty FROM word_subj INNER JOIN subjects ON subjects.id = word_subj.subj_id WHERE subjects.name = ? AND word_subj.word = ?" (subj, word)

    totalWordsInSubject :: DBWork Double
    totalWordsInSubject =
      int2Double . withDefault 0
        <$> query "SELECT SUM(word_subj.qty) FROM word_subj INNER JOIN subjects ON subjects.id = word_subj.subj_id WHERE subjects.name = ?" (Only subj)

    subjectCount :: DBWork Double
    subjectCount =
      int2Double . withDefault 0
        <$> query "SELECT qty FROM subjects WHERE name = ?" (Only subj)

    totalArticlesCount :: DBWork Double
    totalArticlesCount =
      int2Double . withDefault 0
        <$> query_ "SELECT SUM(qty) FROM subjects"

    totalWordCount :: DBWork Double
    totalWordCount =
      int2Double . withDefault 0
        <$> query "SELECT SUM(word_subj.qty) FROM word_subj INNER JOIN subjects ON subjects.id = word_subj.subj_id WHERE word_subj.word = ?" (Only word)
