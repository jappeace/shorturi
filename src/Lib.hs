{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}

module Lib
  ( main
  , appServer
  , appProxy
  , webServiceToHandler
  , makeSettings
  )
where

import           Control.Monad.IO.Class
import           Servant.API.Generic
import           Servant
import Control.Monad.Reader
import Network.Wai.Handler.Warp
import           Servant.Server.Generic
import Control.Exception(throwIO)
import Shortened
import Sanitization
import Uri
import Data.Coerce

makeSettings :: IO ApiSettings
makeSettings = pure $ MkApiSettings
        { insertUri = const $ const $ pure ()
        , retrieveUri = const $ pure $ coerce $ makeUri ""
        , genShortened = pure $ coerce $ makeShortened ""
        }

main :: IO ()
main = do
  apiSettings <- makeSettings

  run 7777 $ writerApp apiSettings


-- normaly I'd put these routes under v1/,
--  so the api can change while being stable
--  but the assignment was explicit for root.
data App route = App
  { appShorten :: route :- ReqBody '[JSON] (Uri 'Incoming) :> Post '[JSON] (Shortened 'Checked)
  -- | the shortened checked in the capture looks dubious, but servant
  --   already provides capability to check this in the instance
  , appFollow  :: route :- Capture "uri" (Shortened 'Checked) :> Get '[JSON] (Uri 'Checked)
  } deriving Generic

appProxy :: Proxy (ToServant App AsApi)
appProxy = Proxy

webServiceToHandler :: ApiSettings -> Endpoint a -> Handler a
webServiceToHandler b m = liftIO $ runReaderT (unEndpoint m) b

writerApp :: ApiSettings -> Application
writerApp settings = serve appProxy $
  hoistServer appProxy (webServiceToHandler settings) appServer

data ApiSettings = MkApiSettings
  { insertUri :: Shortened 'Checked -> Uri 'Checked -> IO ()
  , genShortened :: IO (Shortened 'Checked)
  , retrieveUri :: Shortened 'Checked -> IO (Uri 'Checked)
  }

newtype Endpoint a = MkEndpoint {unEndpoint :: ReaderT ApiSettings IO a }
  deriving newtype (Functor, Monad, Applicative, MonadIO, MonadReader ApiSettings)

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
  liftIO $ maybe (throwIO $ err422) (inserter shortened) $ validateUri urix
  pure shortened

follow :: Shortened 'Checked -> Endpoint (Uri 'Checked)
follow incoming = do
  liftIO $ putStrLn "entering follow"
  retiever <- asks retrieveUri
  liftIO $ retiever incoming
