{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StrictData     #-}

-- | hides Uri constructor so that validation always happens
module Uri
  ( Uri
  , validateUri
  , makeUri
  , unmakeUri
  ) where

import           Data.Aeson
import           Data.Proxy
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Database.Persist.Sql
import           GHC.Generics
import           Network.URI
import           Sanitization

newtype Uri (a :: Sanitization) = MkUri {uri :: Text}
  deriving newtype (Show, Eq)
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

validateUri :: Uri 'Incoming -> Maybe (Uri 'Checked)
validateUri (MkUri x) = MkUri x <$ parseURI (T.unpack x)

makeUri :: Text -> Uri 'Incoming
makeUri = MkUri

unmakeUri :: Uri a -> Text
unmakeUri (MkUri x) = x

instance PersistField (Uri 'Checked) where
  toPersistValue (MkUri x) = toPersistValue x
  fromPersistValue (PersistText x) = Right $ MkUri x
  fromPersistValue _ = Left "Unkown checked uri value in persistfield"

instance  PersistFieldSql (Uri 'Checked) where
  sqlType _ = sqlType (Proxy :: Proxy Text)
