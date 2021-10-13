{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}

module Lib
  ( main
  )
where

import           Control.Monad.IO.Class
import           Servant.API.Generic
import           Servant
import           Network.URI
import Data.Text(Text)
import qualified Data.Text as T
import  Data.Aeson
import Control.Monad.Reader
import Network.Wai.Handler.Warp
import           Servant.Server.Generic
import Data.Bifunctor
import Control.Exception(throwIO)

main :: IO ()
main = do
  let apiSettings = MkApiSettings
        { insertUri = const $ const $ pure ()
        , retrieveUri = const $ pure $ MkUri ""
        , genShortened = pure $ MkShortened ""
        }

  run 7777 $ writerApp apiSettings


-- TODO write regression test should be {uri: "xxx"}
newtype Uri (a :: Sanitization) = MkUri Text
  deriving (ToJSON, FromJSON, Generic)

newtype Shortened (a :: Sanitization) = MkShortened Text
  deriving Generic

shortLength :: Int
shortLength = 5

data InputIssues = WrongLength Int

showIssue :: InputIssues -> Text
showIssue (WrongLength x) = "Expected length of 5, got " <> T.pack (show x)

validShortened :: Text -> Either InputIssues (Shortened 'Incoming)
validShortened input = do
  when (inputLength /= shortLength) $ Left $ WrongLength inputLength
  -- TODO maybe check if is base64?
  pure $ MkShortened input
  where
    inputLength :: Int
    inputLength = T.length input

instance FromHttpApiData (Shortened 'Incoming) where
  parseUrlPiece = first showIssue . validShortened

data Sanitization = Incoming
                  | Checked
                  deriving Generic

data App route = App
  { appShorten :: route :- ReqBody '[JSON] (Uri 'Incoming) :> Post '[JSON] ()
  , appFollow  :: route :- Capture "uri" (Shortened 'Incoming) :> Get '[JSON] (Uri 'Checked)
  } deriving Generic

appProxy :: Proxy (ToServant App AsApi)
appProxy = Proxy

webServiceToHandler :: ApiSettings -> Endpoint a -> Handler a
webServiceToHandler b m = liftIO $ runReaderT (unEndpoint m) b

writerApp :: ApiSettings -> Application
writerApp settings = serve appProxy $
  hoistServer appProxy (webServiceToHandler settings) appServer

validateUri :: Uri 'Incoming -> Maybe (Uri 'Checked)
validateUri (MkUri x) = MkUri x <$ parseURI (T.unpack x)

-- TODO how to do validation of URI (eg correct form)
-- TODO how to do validation of URI (eg correct form)
data ApiSettings = MkApiSettings
  { insertUri :: Shortened 'Checked -> Uri 'Checked -> IO ()
  , genShortened :: IO (Shortened 'Checked)
  , retrieveUri :: Shortened 'Incoming -> IO (Uri 'Checked)
  }

newtype Endpoint a = MkEndpoint {unEndpoint :: ReaderT ApiSettings IO a }
  deriving newtype (Functor, Monad, Applicative, MonadIO, MonadReader ApiSettings)

appServer :: ToServant App (AsServerT Endpoint)
appServer = genericServerT App
  { appShorten = shorten
  , appFollow  = follow
  }

shorten :: Uri 'Incoming -> Endpoint ()
shorten urix = do
  liftIO $ putStrLn "entering shortener"
  generator <- asks genShortened
  shortened <- liftIO $ generator
  inserter <- asks insertUri
  liftIO $ maybe (throwIO $ err422) (inserter shortened) $ validateUri urix

follow :: Shortened 'Incoming -> Endpoint (Uri 'Checked)
follow _ = do
  liftIO $ putStrLn "entering follow"
  pure $ MkUri ""
