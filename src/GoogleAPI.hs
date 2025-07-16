{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GoogleAPI
  ( synthesizeFromText,
    getVoiceList,
    VoicesListResponse (..),
    Voice (..),
  )
where

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
    setRequestQueryString,
  )
import System.Environment (lookupEnv)

newtype SynthesizeResponse = SynthesizeResponse
  {audioContent :: BC.ByteString}
  deriving (Show)

instance FromJSON SynthesizeResponse where
  parseJSON = withObject "SynthesizeResponse" $ \o -> do
    content <- BC.pack . T.unpack <$> (o .: "audioContent" :: Parser T.Text)
    return $ SynthesizeResponse content

data Voice = Voice
  { voiceName :: T.Text,
    voiceGender :: T.Text
  }
  deriving (Show)

instance FromJSON Voice where
  parseJSON = withObject "Voice" $ \o -> do
    n <- o .: "name"
    g <- o .: "ssmlGender"
    return $ Voice n g

newtype VoicesListResponse = VoicesListResponse
  { voices :: [Voice]
  }
  deriving (Show)

instance FromJSON VoicesListResponse where
  parseJSON = withObject "VoicesListResponse" $ \o -> do
    vs <- o .: "voices"
    return $ VoicesListResponse vs

getBearerToken :: ExceptT T.Text IO B.ByteString
getBearerToken = do
  token <- liftIO $ lookupEnv "GOOGLE_TTS_API_KEY"
  case token of
    Just t -> return . BC.pack $ t
    Nothing -> throwE "GOOGLE_TTS_API_KEY is not set"

synthesizeFromText :: T.Text -> Voice -> ExceptT T.Text IO BC.ByteString
synthesizeFromText text voice = do
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
          "languageCode": "en-US",
          "name": #{voiceName voice},
          "ssmlGender": #{voiceGender voice}
        },
        "audioConfig": {
          "audioEncoding": "MP3",
          "volumeGainDb" : 2.0
        }
      }|]

getVoiceList :: ExceptT T.Text IO VoicesListResponse
getVoiceList = do
  token <- getBearerToken
  response <- catch (httpBS . request $ token) $
    \(SomeException e) -> throwE $ "Failed to get voice list : " <> T.pack (show e)

  case getResponseStatusCode response of
    200 -> do
      case eitherDecodeStrict (getResponseBody response) of
        Left e -> throwE $ "Failed to parse response of voice list : " <> T.pack e
        Right res -> return res
    code ->
      throwE $
        "Failed to get voice list : "
          <> (T.pack . show $ code)
          <> " * "
          <> (T.pack . BC.unpack . getResponseBody $ response)
  where
    request t =
      addRequestHeader "X-goog-api-key" t
        . addRequestHeader "charset" "UTF-8"
        . setRequestHeader "Accept" ["application/json"]
        . setRequestMethod "GET"
        . setRequestQueryString [("languageCode", Just "en-US")]
        $ parseRequest_ "https://texttospeech.googleapis.com/v1/voices"
