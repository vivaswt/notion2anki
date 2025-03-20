{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GoogleAPI (synthesizeFromText) where

import Control.Exception.Safe (SomeException (SomeException), catch)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Aeson
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Network.HTTP.Simple
  ( addRequestHeader,
    getResponseBody,
    getResponseStatusCode,
    httpBS,
    parseRequest_,
    setRequestBodyJSON,
    setRequestHeader,
    setRequestMethod,
  )
import System.Environment (lookupEnv)

newtype SynthesizeResponse = SynthesizeResponse
  {audioContent :: BC.ByteString}
  deriving (Show)

instance FromJSON SynthesizeResponse where
  parseJSON = withObject "SynthesizeResponse" $ \o -> do
    content <- BC.pack . T.unpack <$> (o .: "audioContent" :: Parser T.Text)
    return $ SynthesizeResponse content

getBearerToken :: ExceptT T.Text IO B.ByteString
getBearerToken = do
  token <- liftIO $ lookupEnv "GOOGLE_TTS_API_KEY"
  case token of
    Just t -> return . BC.pack $ t
    Nothing -> throwE "GOOGLE_TTS_API_KEY is not set"

synthesizeFromText :: T.Text -> ExceptT T.Text IO BC.ByteString
synthesizeFromText text = do
  token <- getBearerToken
  response <- catch (httpBS . request $ token) $
    \(SomeException e) -> throwE $ "Failed to synthesize text : " <> T.pack (show e)

  case getResponseStatusCode response of
    200 -> do
      let r = eitherDecodeStrict . getResponseBody $ response
      case r of
        Left e -> throwE $ "Failed to parse response of synthesize text : " <> T.pack e
        Right res -> return $ audioContent res
    code ->
      throwE $
        "Failed to synthesize text : "
          <> (T.pack . show $ code)
          <> " * "
          <> (T.pack . BC.unpack . getResponseBody $ response)
  where
    request t =
      addRequestHeader "X-goog-api-key" t
        . addRequestHeader "charset" "UTF-8"
        . setRequestHeader "Content-Type" ["application/json"]
        . setRequestBodyJSON requestBody
        . setRequestMethod "POST"
        $ parseRequest_ "https://texttospeech.googleapis.com/v1/text:synthesize"

    requestBody =
      [aesonQQ|{
        "input": {
          "text": #{text}
        },
        "voice": {
          "languageCode": "en-US"
        },
        "audioConfig": {
          "audioEncoding": "MP3"
        }
      }|]
