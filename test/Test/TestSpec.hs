{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.TestSpec
  ( spec
  )
where

import           Control.Exception
import           Control.Monad.Random.Class
import           Data.Either
import           Data.Maybe
import qualified Data.Text                      as T
import           Lib
import           Sanitization
import           Servant
import           Servant.QuickCheck
import           Shortened
import           System.Random
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Instances.Text ()
import           Test.QuickCheck.Random
import           Uri
import Data.Text(Text)

deriving instance Exception InputIssues
instance Show InputIssues where
  show = T.unpack . showIssue

instance Arbitrary (Uri 'Checked) where
  arbitrary = oneof $ pure . fromMaybe (error "expected valid uri in aribtrary") <$> [
      validateUri $ makeUri "https://jappie.me"
    , validateUri $ makeUri "https://jappieklooster.nl/"
    , validateUri $ makeUri "https://google.com"
    ]

instance Arbitrary (Uri 'Incoming) where
  arbitrary =
    oneof [
      checkedToIncoming <$> arbitrary
    , makeUri <$> arbitrary
    ]

instance Arbitrary (Shortened 'Checked) where
  arbitrary = genShortened

instance ToHttpApiData (Shortened 'Checked) where
  toUrlPiece = toText

isRightLength :: Shortened 'Checked -> Bool
isRightLength x = T.length (toText x) == shortLength

checkedToIncoming :: Uri 'Checked -> Uri 'Incoming
checkedToIncoming = makeUri . unmakeUri

-- this is just checking the arbitrary instance kindoff
-- but show knows what validate does, I just piggied backed of the lib anyway
checkedUriIsValid :: Uri 'Checked -> Bool
checkedUriIsValid = isJust . validateUri . checkedToIncoming

checkedShortenedIsValid :: Shortened 'Checked -> Bool
checkedShortenedIsValid = isRight . validShortened . toText

-- technically it's possible for 1 to pass, but incredibly unlikely for more
randomUriRejects :: Text -> Bool
randomUriRejects = isNothing . validateUri . makeUri

randomShortenedRejects :: Text -> Bool
randomShortenedRejects = isLeft . validShortened

spec :: Spec
spec = do
  describe "Uri parser" $ do
      it "accepts a checked uri's" $ property checkedUriIsValid
      it "rejects random strings" $ property randomUriRejects
  describe "Shortened parser " $ do
      it "accepts a checked shortened" $ property checkedShortenedIsValid
      it "rejects random strings" $ property randomShortenedRejects
  describe "Shortened" $ do
    it "is the right legnth " $ property isRightLength
  describe "Servant quickheck, this will grow with the api over time" $ do
    it "no 500, only json" $
      withServantServer appProxy ((\settings -> hoistServer appProxy (webServiceToHandler settings) appServer) <$> makeSettings ":memory:") $ \burl ->
        serverSatisfies appProxy burl defaultArgs (onlyJsonObjects <%> not500 <%> mempty) -- I don't like this property combining mechanism, I think it's better to write a test per property


mkGen :: (QCGen -> a) -> Gen a
mkGen f = MkGen $ \g _ -> f g

instance MonadRandom Gen where
    getRandom = mkGen (fst . random)
    getRandoms = mkGen randoms
    getRandomR range = mkGen (fst . randomR range)
    getRandomRs range = mkGen (randomRs range)
