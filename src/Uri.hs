{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StrictData     #-}

-- | hides Uri constructor so that validation always happens
module Uri
  ( Uri
  , validateUri
  , makeUri
  ) where

import           Data.Aeson
import           Data.Proxy
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Database.Persist.Sql
import           Network.URI
import           Sanitization

-- TODO write regression test should be {uri: "xxx"}
newtype Uri (a :: Sanitization) = MkUri Text
  deriving (ToJSON, FromJSON, Show)

validateUri :: Uri 'Incoming -> Maybe (Uri 'Checked)
validateUri (MkUri x) = MkUri x <$ parseURI (T.unpack x)

makeUri :: Text -> Uri 'Incoming
makeUri = MkUri

instance PersistField (Uri 'Checked) where
  toPersistValue (MkUri x) = toPersistValue x
  fromPersistValue (PersistText x) = Right $ MkUri x
  fromPersistValue _ = Left "Unkown checked uri value in persistfield"

instance  PersistFieldSql (Uri 'Checked) where
  sqlType _ = sqlType (Proxy :: Proxy Text)
