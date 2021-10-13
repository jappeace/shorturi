{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.TestSpec
  ( spec
  )
where

import           Lib
import           Test.Hspec
import Servant.QuickCheck
import           Servant
import Test.QuickCheck
import Uri
import Sanitization
import Test.QuickCheck.Instances.Text()
import Control.Exception
import Shortened
import qualified Data.Text as T

one :: Int
one = 1

deriving instance Exception InputIssues
instance Show InputIssues where
  show = T.unpack . showIssue

instance Arbitrary (Uri 'Incoming) where
  arbitrary = makeUri <$> arbitrary

instance Arbitrary (Shortened 'Checked) where
  arbitrary = do
    txt' <-  T.pack <$> resize shortLength (arbitrary :: Gen [Char])
    pure $ either throw id $ -- better to crash the test
      validShortened txt'

instance ToHttpApiData (Shortened 'Checked) where
  toUrlPiece = toText

spec :: Spec
spec = do
  describe "The sanity of our test setup" $ do
    it "should satisfy equality" $
      one `shouldBe` 1
  describe "Servant quickheck, this will grow with the api over time" $ do
    it "no 500" $
      withServantServer appProxy ((\settings -> hoistServer appProxy (webServiceToHandler settings) appServer) <$> makeSettings) $ \burl ->
        serverSatisfies appProxy burl defaultArgs (not500 <%> mempty) -- I don't like this property combining mechanism, I think it's better to write a test per property
    it "only json" $
      withServantServer appProxy ((\settings -> hoistServer appProxy (webServiceToHandler settings) appServer) <$> makeSettings) $ \burl ->
        serverSatisfies appProxy burl defaultArgs (onlyJsonObjects <%> mempty)
