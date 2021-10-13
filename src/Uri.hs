{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StrictData     #-}
{-# LANGUAGE DataKinds #-}

-- | hides Uri constructor so that validation always happens
module Uri
  ( Uri
  , validateUri
  , makeUri
  ) where

import           Data.Aeson
import           Data.Text    (Text)
import qualified Data.Text    as T
import           Network.URI
import           Sanitization

-- TODO write regression test should be {uri: "xxx"}
newtype Uri (a :: Sanitization) = MkUri Text
  deriving (ToJSON, FromJSON)

validateUri :: Uri 'Incoming -> Maybe (Uri 'Checked)
validateUri (MkUri x) = MkUri x <$ parseURI (T.unpack x)

makeUri :: Text -> Uri 'Incoming
makeUri = MkUri
