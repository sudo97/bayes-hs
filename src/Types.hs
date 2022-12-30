{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Control.Lens (Lens')
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import qualified Data.Map as M
import Data.String (IsString)
import qualified Data.Text as T
import GHC.Generics (Generic)

newtype ArticleText = ArticleText T.Text deriving (Show, Generic, IsString)

articleInnerText :: Lens' ArticleText T.Text
articleInnerText f (ArticleText t) = ArticleText <$> f t

instance FromJSON ArticleText

instance ToJSON ArticleText

newtype SubjectTitle = SubjectTitle T.Text deriving (Show, Generic, IsString, Eq, Ord)

subjectInnerTitle :: Lens' SubjectTitle T.Text
subjectInnerTitle f (SubjectTitle t) = SubjectTitle <$> f t

instance FromJSON SubjectTitle

instance FromJSONKey SubjectTitle

instance ToJSONKey SubjectTitle

instance ToJSON SubjectTitle

data Article = Article {articleText :: ArticleText, articleSubjects :: [SubjectTitle]} deriving (Show, Generic)

articleTextLens :: Lens' Article ArticleText
articleTextLens f a = (\txt -> a {articleText = txt}) <$> f (articleText a)

articleSubjectsLens :: Lens' Article [SubjectTitle]
articleSubjectsLens f a = (\subjects -> a {articleSubjects = subjects}) <$> f (articleSubjects a)

instance FromJSON Article

instance ToJSON Article

newtype Probabilities = Probabilities (M.Map SubjectTitle Double) deriving (Show, Generic)

instance FromJSON Probabilities

instance ToJSON Probabilities

newtype TotalSubjCount = TotalSubjCount (M.Map SubjectTitle Int) deriving (Show, Generic)

instance FromJSON TotalSubjCount

instance ToJSON TotalSubjCount