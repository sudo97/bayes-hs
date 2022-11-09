module DBWork (openDb, query_, query, execute, changes, DBWork, Only (..), transaction) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Database.SQLite.Simple (Only (..))
import qualified Database.SQLite.Simple as S

type DBWork a = ReaderT S.Connection IO a

query_ :: S.FromRow a => S.Query -> DBWork [a]
query_ q = do
  conn <- ask
  liftIO $ S.query_ conn q

query :: (S.FromRow a, S.ToRow b) => S.Query -> b -> DBWork [a]
query q arg = do
  conn <- ask
  liftIO $ S.query conn q arg

execute :: S.ToRow q => S.Query -> q -> DBWork ()
execute q arg = do
  conn <- ask
  liftIO $ S.execute conn q arg

changes :: DBWork Int
changes = ask >>= liftIO . S.changes

openDb :: String -> DBWork () -> IO ()
openDb s work = S.open s >>= runReaderT work

transaction :: DBWork a -> DBWork a
transaction action = do
  conn <- ask
  liftIO $ S.withTransaction conn $ runReaderT action conn
