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
  )
where

import           Control.Monad.Error.Class
import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Database.Persist.Sqlite
import           Model
import           Network.Wai.Handler.Warp
import           Sanitization
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic
import qualified Shortened
import           Shortened(Shortened)
import           Uri
import           Data.Text(Text)

makeSettings :: Text -> IO ApiSettings
makeSettings dbname = do
  pool <- runNoLoggingT $ createSqlitePool dbname 5
  runSqlPool (runMigration migrateAll) pool

  pure $ MkApiSettings
        { insertUri = \a b -> runSqlPool (insertUriSql a b) pool
        , retrieveUri = \x -> runSqlPool (retrieveUriSql x) pool
        , genShortened = Shortened.genShortened
        }

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

-- crummy readerT
data ApiSettings = MkApiSettings
  { insertUri    :: Shortened 'Checked -> Uri 'Checked -> IO ()
  , genShortened :: IO (Shortened 'Checked)
  , retrieveUri  :: Shortened 'Checked -> IO (Maybe (Entity Mapping))
  }

newtype Endpoint a = MkEndpoint {unEndpoint :: ReaderT ApiSettings (ExceptT ServerError IO) a }
  deriving newtype (Functor, Monad, Applicative, MonadIO, MonadReader ApiSettings, MonadError ServerError)

appServer :: ToServant App (AsServerT Endpoint)
appServer = genericServerT App
  { appShorten = shorten
  , appFollow  = follow
  }

shorten :: Uri 'Incoming -> Endpoint (Shortened 'Checked)
shorten urix = do
  liftIO $ putStrLn "entering shortener"
  generator <- asks genShortened
  shortened <- liftIO $ generator
  inserter <- asks insertUri
  maybe (throwError $ err422) (liftIO . inserter shortened) $ validateUri urix
  pure shortened

follow :: Shortened 'Checked -> Endpoint (Uri 'Checked)
follow incoming = do
  liftIO $ putStrLn "entering follow"
  retiever <- asks retrieveUri
  retrieved <- liftIO $ retiever incoming
  case retrieved of
    Just x -> pure $ mappingOriginal $ entityVal x
    Nothing -> throwError $ err404
