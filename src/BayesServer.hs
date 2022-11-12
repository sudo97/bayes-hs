{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module BayesServer (mainRun) where

-- import Data.Data (Proxy)
-- import Data.Text
-- import Data.Time (UTCTime)

-- import Network.HTTP.Media ((//), (/:))

import Control.Monad.IO.Class
import DBWork
import Data.Aeson
import Data.Functor (($>))
import Data.List (intercalate)
import qualified Data.Text as T
import Database.SQLite.Simple.Types
import GHC.Generics (Generic)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

type BayesAPI =
  "calculate" :> ReqBody '[JSON] T.Text :> Get '[JSON] [(T.Text, Double)]
    :<|> "update" :> ReqBody '[JSON] (T.Text, T.Text) :> Post '[JSON] NoContent
    :<|> "subjects" :> Get '[JSON] [(T.Text, Int)]

bayesServer :: FilePath -> Server BayesAPI
bayesServer dbfile = calculate :<|> update :<|> subjects
  where
    calculate _text = pure []
    update (_, _) = pure NoContent
    subjects =
      liftIO . openDb dbfile $ do
        query_ "SELECT name, qty FROM subjects"

mainRun :: IO ()
mainRun = do
  let dbfile = "bayes.db"
  run 8081 . serve @BayesAPI Proxy . bayesServer $ dbfile