{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE StrictData     #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | hides Uri constructor so that validation always happens
module Shortened
  ( Shortened
  , makeShortened
  , validShortened
  , shortLength
  , InputIssues
  , showIssue
  , toText
  , genShortened
  )
where

import           Control.Monad.Random.Class
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString                        as BS
import           "base64" Data.ByteString.Base64.URL    (encodeBase64)
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import           "base64" Data.Text.Encoding.Base64.URL (decodeBase64)
import           Database.Persist.Sql
import           GHC.Generics
import           Sanitization
import           Servant

newtype Shortened (a :: Sanitization) = MkShortened { short :: Text }
  deriving newtype (Show, Eq)
  deriving stock (Generic)
  deriving anyclass (ToJSON)

shortLength :: Int
shortLength = 8

data InputIssues = WrongLength Int
                 | Base64Issue Text

genShortened :: MonadRandom m => m (Shortened 'Checked)
genShortened = do
  str <- replicateM 5 getRandom
  pure $ MkShortened $ encodeBase64 $ BS.pack str

showIssue :: InputIssues -> Text
showIssue (WrongLength x) = "Expected length of 8, got: " <> T.pack (show x)
showIssue (Base64Issue x) = "Is not a base64 encoding: " <> T.pack (show x)

validShortened :: Text -> Either InputIssues (Shortened 'Checked)
validShortened input = do
  when (inputLength /= shortLength) $ Left $ WrongLength inputLength
  void $ first Base64Issue $ decodeBase64 input
  pure $ MkShortened input
  where
    inputLength :: Int
    inputLength = T.length input

instance FromHttpApiData (Shortened 'Checked) where
  parseUrlPiece = first showIssue . validShortened

makeShortened :: Text -> Shortened 'Incoming
makeShortened = MkShortened

toText :: Shortened a -> Text
toText (MkShortened x) = x

instance PersistField (Shortened 'Checked) where
  toPersistValue (MkShortened x) = toPersistValue x
  fromPersistValue (PersistText x) = Right $ MkShortened x
  fromPersistValue _ = Left "Unkown checked shortened value in persistfield"

instance  PersistFieldSql (Shortened 'Checked) where
  sqlType _ = sqlType (Proxy :: Proxy Text)
