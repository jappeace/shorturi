{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StrictData     #-}
{-# LANGUAGE TypeOperators  #-}

module Lib
  ( main
  , appServer
  , appProxy
  , webServiceToHandler
  , makeSettings
  , insertUriSql
  , retrieveUriSql
  , ApiSettings
  , Endpoint
  , shortenEndpoint
  , followEndpoint
  , destroySettings
  )
where

import           Control.Monad.Error.Class
import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Pool
import           Data.Text                 (Text)
import           Database.Persist.Sqlite
import           Model
import           Network.Wai.Handler.Warp
import           Sanitization
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic
import           Shortened                 (Shortened)
import qualified Shortened
import           Uri

makeSettings :: Text -> IO ApiSettings
makeSettings dbname = do
  pool <- runNoLoggingT $ createSqlitePool dbname 5
  runSqlPool (runMigration migrateAll) pool
  pure $ MkApiSettings { apiPool = pool}

destroySettings :: ApiSettings -> IO ()
destroySettings (MkApiSettings pool) = destroyAllResources pool

insertUriSql :: MonadIO m => Shortened 'Checked -> Uri 'Checked -> ReaderT SqlBackend m ()
insertUriSql a b = void $ insert $ Mapping a b

retrieveUriSql :: MonadIO m => Shortened 'Checked ->  ReaderT SqlBackend m (Maybe (Entity Mapping))
retrieveUriSql x = getBy $ UniqueShortened x

main :: IO ()
main = do
  apiSettings <- makeSettings "shortner-prod"

  run 7777 $ writerApp apiSettings


-- normaly I'd put these routes under v1/,
--  so the api can change while being stable
--  but the assignment was explicit for root.
data App route = App
  { appShorten :: route :- ReqBody '[JSON] (Uri 'Incoming) :> Post '[JSON] (Shortened 'Checked)
  -- | servant already provides capability to check this in the instance
  , appFollow  :: route :- Capture "uri" (Shortened 'Checked) :> Get '[JSON] (Uri 'Checked)
  } deriving Generic

appProxy :: Proxy (ToServant App AsApi)
appProxy = Proxy

webServiceToHandler :: ApiSettings -> Endpoint a -> Handler a
webServiceToHandler b m = Handler $ runReaderT (unEndpoint m) b

writerApp :: ApiSettings -> Application
writerApp settings = serve appProxy $
  hoistServer appProxy (webServiceToHandler settings) appServer

-- here there used to be areaderT to stub out db functions.
-- the app is to trivial to test anything meaningfull with that however.
-- at supercede I used that trick to get rid of s3 connection problems
-- but generally it doesn't make as much sense for a db, becase db
-- contains a lot of logic anyway
data ApiSettings = MkApiSettings
  { apiPool :: Pool SqlBackend
  }


newtype Endpoint a = MkEndpoint {unEndpoint :: ReaderT ApiSettings (ExceptT ServerError IO) a }
  deriving newtype (Functor, Monad, Applicative, MonadIO, MonadReader ApiSettings, MonadError ServerError)

appServer :: ToServant App (AsServerT Endpoint)
appServer = genericServerT App
  { appShorten = shortenEndpoint
  , appFollow  = followEndpoint
  }

runDB :: ReaderT SqlBackend IO a -> Endpoint a
runDB monad = do
  pool <- asks apiPool
  liftIO $ runSqlPool monad pool

shortenEndpoint :: Uri 'Incoming -> Endpoint (Shortened 'Checked)
shortenEndpoint urix = do
  liftIO $ putStrLn "entering shortener"
  shortened <- liftIO $ Shortened.genShortened
  maybe (throwError $ err422) (runDB . insertUriSql shortened) $ validateUri urix
  pure shortened

followEndpoint :: Shortened 'Checked -> Endpoint (Uri 'Checked)
followEndpoint incoming = do
  liftIO $ putStrLn "entering follow"
  retrieved <- runDB $ retrieveUriSql incoming
  case retrieved of
    Just x  -> pure $ mappingOriginal $ entityVal x
    Nothing -> throwError $ err404

