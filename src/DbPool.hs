module DbPool where

import Control.Monad.IO.Class (MonadIO, liftIO)
import DBWork (DBWork, runDb)
import Data.Pool
  ( Pool,
    PoolConfig (PoolConfig),
    newPool,
    withResource,
  )
import Database.SQLite.Simple (Connection, close, open)

initConnectionPool :: String -> IO (Pool Connection)
initConnectionPool file = newPool (PoolConfig (open file) close 3 10)

withDbPool :: MonadIO io => Pool Connection -> DBWork a -> io a
withDbPool dbconnection = liftIO . withResource dbconnection . runDb