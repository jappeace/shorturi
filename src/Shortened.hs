{-# LANGUAGE StrictData #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

-- | hides Uri constructor so that validation always happens
module Shortened
  ( Shortened
  , makeShortened
  , validShortened
  , shortLength
  , InputIssues
  , showIssue
  , toText
  )
where

import Sanitization
import           Servant
import Data.Text(Text)
import qualified Data.Text as T
import Control.Monad.Reader
import Data.Bifunctor
import           Data.Aeson

newtype Shortened (a :: Sanitization) = MkShortened Text
  deriving newtype ToJSON

shortLength :: Int
shortLength = 5

data InputIssues = WrongLength Int

showIssue :: InputIssues -> Text
showIssue (WrongLength x) = "Expected length of 5, got " <> T.pack (show x)

validShortened :: Text -> Either InputIssues (Shortened 'Checked)
validShortened input = do
  when (inputLength /= shortLength) $ Left $ WrongLength inputLength
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
