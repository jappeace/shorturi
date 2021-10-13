{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StrictData     #-}

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

import           Control.Monad.Reader
import           Data.Aeson
import           Data.Bifunctor
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Encoding.Base64
import           Database.Persist.Sql
import           Sanitization
import           Servant
import           System.Random


newtype Shortened (a :: Sanitization) = MkShortened Text
  deriving newtype (ToJSON, Show)

shortLength :: Int
shortLength = 5

data InputIssues = WrongLength Int
                 | Base64Issue Text

genShortened :: IO (Shortened 'Checked)
genShortened = do
  str <- replicateM 5 $ (randomIO :: IO Char)
  pure $ MkShortened $ encodeBase64 $ T.pack str


showIssue :: InputIssues -> Text
showIssue (WrongLength x) = "Expected length of 5, got: " <> T.pack (show x)
showIssue (Base64Issue x) = "Is not a base64 encoding: " <> T.pack (show x)

validShortened :: Text -> Either InputIssues (Shortened 'Checked)
validShortened input = do
  when (inputLength /= shortLength) $ Left $ WrongLength inputLength
  void $ first Base64Issue $ decodeBase64 input
  -- TODO maybe check if is base64?
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
