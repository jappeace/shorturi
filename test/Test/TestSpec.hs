{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.TestSpec
  ( spec
  )
where

import           Control.Exception
import           Control.Monad.Random.Class
import           Data.Aeson
import qualified Data.ByteString.Lazy           as LBS
import           Data.Either
import           Data.Maybe
import qualified Data.Text                      as T
import           Lib
import           Network.URI
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

checkedUriIsValid :: Uri 'Checked -> Bool
checkedUriIsValid = isJust . validateUri . checkedToIncoming

checkedShortenedIsValid :: Shortened 'Checked -> Bool
checkedShortenedIsValid = isRight . validShortened . toText

runEndpoint :: ApiSettings -> Endpoint a -> IO (Either ServerError a)
runEndpoint ctx end = runHandler $ webServiceToHandler ctx end

uriRoundTrip :: Uri 'Checked -> Bool
uriRoundTrip uri = decode x == Just uri
  where
    x :: LBS.ByteString
    x = encode uri

assertBase64IsRoot :: Shortened 'Checked -> Bool
assertBase64IsRoot input =
  (length . pathSegments <$> parseURI (T.unpack $ "https://jappie.me/" <> toText input)
  ) == Just 1

spec :: Spec
spec = do
  describe "parser" $ do
    describe "Uri " $ do
        it "accepts a checked uri's" $ property checkedUriIsValid
        it "can parse my website" $
          isJust (validateUri $ makeUri "https://jappie.me") `shouldBe` True
    describe "Shortened parser " $ do
        it "accepts a checked shortened" $ property checkedShortenedIsValid
  describe "Shortened generator" $ do
    it "is the right length " $ property isRightLength
  describe "Regression" $ do
    it "uses base 64 URL module otherwise wrong signs like /" $
      property assertBase64IsRoot

  describe "Aeson" $ do
    describe "URI" $ do
      it "encodes as object goldenly" $
        encode (makeUri "https://jappie.me")
          `shouldBe` "{\"uri\":\"https://jappie.me\"}"
      it "roundtrips" $ property uriRoundTrip
    describe "Shortened" $ do
      it "encodes as object goldenly" $
        encode (makeShortened "O/h4Gmc=")
          `shouldBe` "{\"short\":\"O/h4Gmc=\"}"
  describe "Integration " $ do
    it "can retrieve an inserted uri" $
      bracket (makeSettings "test-db-1") destroySettings $ \settings -> do
        let uri = fromMaybe (error "could not parse uri") (validateUri incomingUri)
            incomingUri = makeUri "https://jappie.me"
        resUri <- runEndpoint settings $ do
            short <- shortenEndpoint incomingUri
            followEndpoint short
        resUri `shouldBe` Right uri

    it "twice insert returns the same short" $ do
      bracket (makeSettings "test-db-2") destroySettings $ \settings -> do
        let incomingUri = makeUri "https://jappie.me"
        res <- runEndpoint settings $ do
            short <- shortenEndpoint incomingUri
            short2 <- shortenEndpoint incomingUri
            pure (short, short2)
        uncurry (==) <$> res `shouldBe` Right True

    describe "Servant quickheck (smoke test check everything)" $ do
      it "no 500, only json" $
        withServantServer appProxy (bracket (makeSettings "test-db") destroySettings
                                    (\settings ->
                                      pure $ hoistServer appProxy (webServiceToHandler settings) appServer
                                    ) ) $ \burl ->
          serverSatisfies appProxy burl defaultArgs (onlyJsonObjects <%> not500 <%> mempty)


mkGen :: (QCGen -> a) -> Gen a
mkGen f = MkGen $ \g _ -> f g

instance MonadRandom Gen where
    getRandom = mkGen (fst . random)
    getRandoms = mkGen randoms
    getRandomR range = mkGen (fst . randomR range)
    getRandomRs range = mkGen (randomRs range)
