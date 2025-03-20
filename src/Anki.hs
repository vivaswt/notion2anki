{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Anki
  ( addNoteOfFlashCard,
    addNoteOfFlashCards,
    AnkiAddNoteResult (..),
  )
where

import Control.Exception.Safe (SomeException (SomeException), catch)
import Control.Monad.Trans.Except (ExceptT (ExceptT), throwE)
import Data.Aeson (FromJSON, Value (..), eitherDecode, withObject, (.:), (.:?))
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types (FromJSON (parseJSON), Parser)
import Data.Bifunctor (Bifunctor (first))
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified GoogleAPI as GAPI
import Network.HTTP.Simple
  ( getResponseBody,
    httpLBS,
    parseRequest_,
    setRequestBodyJSON,
    setRequestHeader,
    setRequestMethod,
  )
import Network.URI (parseURI, uriPath)
import qualified NotionFlashCard as NFC

data AnkiAddNoteResult
  = AnkiAddNoteResultSuccess Int
  | AnkiAddNoteResultFailure T.Text
  deriving (Show)

instance FromJSON AnkiAddNoteResult where
  parseJSON = parseAnkiAddNoteResult

data AnkiStoreMediaFileResult
  = AnkiStoreMediaFileResultSuccess MediaFileName
  | AnkiStoreMediaFileResultFailure T.Text
  deriving (Show)

instance FromJSON AnkiStoreMediaFileResult where
  parseJSON = parseAnkiStoreMediaFileResult

type MediaFileName = T.Text

parseAnkiAddNoteResult :: Value -> Parser AnkiAddNoteResult
parseAnkiAddNoteResult = withObject "AnkiAddNoteResult" $ \o -> do
  e <- o .:? "error" :: Parser (Maybe T.Text)
  case e of
    Nothing -> AnkiAddNoteResultSuccess <$> o .: "result"
    Just err -> return $ AnkiAddNoteResultFailure err

parseAnkiStoreMediaFileResult :: Value -> Parser AnkiStoreMediaFileResult
parseAnkiStoreMediaFileResult = withObject "AnkiStoreMediaFileResult" $ \o -> do
  e <- o .:? "error" :: Parser (Maybe T.Text)
  case e of
    Nothing -> AnkiStoreMediaFileResultSuccess <$> o .: "result"
    Just err -> return $ AnkiStoreMediaFileResultFailure err

addNoteOfFlashCards :: [NFC.FlashCard] -> ExceptT T.Text IO [AnkiAddNoteResult]
addNoteOfFlashCards = mapM addNoteOfFlashCard

addNoteOfFlashCard :: NFC.FlashCard -> ExceptT T.Text IO AnkiAddNoteResult
addNoteOfFlashCard flashCard = do
  reqBody <- addNoteRequestBody flashCard

  let request =
        setRequestHeader "Content-Type" ["application/json"]
          . setRequestBodyJSON reqBody
          . setRequestMethod "POST"
          $ parseRequest_ "http://host.docker.internal:8765"

  response <- catch (httpLBS request) $
    \(SomeException e) ->
      throwE $ "Failed to add flash card to Anki :" <> T.pack (show e)

  eitherString2EitherTText
    . eitherDecode
    . getResponseBody
    $ response

eitherString2EitherTText :: Either String a -> ExceptT T.Text IO a
eitherString2EitherTText = ExceptT . return . first T.pack

addNoteRequestBody :: NFC.FlashCard -> ExceptT T.Text IO Value
addNoteRequestBody flashCard = do
  audioReq <- audioRequestBody flashCard'
  return
    [aesonQQ| {
      "action": "addNote",
      "version": 6,
      "params": {
          "note": {
              "deckName": "英単語",
              "modelName": "Basic",
              "fields": {
                  "Front": #{NFC.flashCardAnswer flashCard'},
                  "Meaning": #{NFC.flashCardSense flashCard'},
                  "ExampleSentence": #{NFC.flashCardQuestion flashCard'},
                  "IPA": #{NFC.flashCardIPA flashCard'},
                  "OtherIPA": #{NFC.flashCardOtherIPA flashCard'},
                  "MnemonicText": #{NFC.flashCardMnemonic flashCard'}
              },
              "picture" : #{picutreRequestBody flashCard},
              "audio" : #{audioReq},
              "options": {
                  "allowDuplicate": false
              }
          }
      }
    }|]
  where
    flashCard' = arrangeFlashCard flashCard

picutreRequestBody :: NFC.FlashCard -> Value
picutreRequestBody flashCard =
  Array vs
  where
    vs =
      V.fromList $
        map
          ( \(NFC.Image u) -> fileBody (fileName u) "WordImage" u
          )
          blocks

    fileName =
      T.pack . fromMaybe "unknown_image" . getFileNameFromURL . T.unpack

    blocks = filter predicate $ NFC.flashCardBlockChildren flashCard

    predicate b = case b of
      NFC.Image _ -> True
      _ -> False

    fileBody :: T.Text -> T.Text -> T.Text -> Value
    fileBody name field url =
      [aesonQQ| {
      url : #{url},
      filename : #{name},
      fields : [#{field}]
    } |]

audioRequestBody :: NFC.FlashCard -> ExceptT T.Text IO Value
audioRequestBody flashCard = do
  wordReq <- audioPronounceWordRequest flashCard
  reqChild <- audioPronounceSentenceRequestFromChildBlock flashCard
  reqTTS <- audioPronounceSentenceRequestFromTTS flashCard
  return . Array . V.fromList $ wordReq : (if null reqChild then [reqTTS] else reqChild)

audioPronounceWordRequest :: NFC.FlashCard -> ExceptT T.Text IO Value
audioPronounceWordRequest flashCard = do
  content <- GAPI.synthesizeFromText $ NFC.flashCardAnswer flashCard
  return $ dataBody content
  where
    dataBody :: BC.ByteString -> Value
    dataBody content =
      [aesonQQ| {
        data : #{TE.decodeUtf8 content},
        filename : "pronounce_word.mp3",
        fields : ["PronounceWord"]
      } |]

audioPronounceSentenceRequestFromChildBlock :: NFC.FlashCard -> ExceptT T.Text IO [Value]
audioPronounceSentenceRequestFromChildBlock flashCard = do
  return $ map (\(NFC.Audio u) -> fileBody (fileName u) "PronounceSentence" u) blocks
  where
    fileName =
      T.pack . fromMaybe "unknown_audio" . getFileNameFromURL . T.unpack

    blocks = filter predicate $ NFC.flashCardBlockChildren flashCard

    predicate b = case b of
      NFC.Audio _ -> True
      _ -> False

    fileBody :: T.Text -> T.Text -> T.Text -> Value
    fileBody name field url =
      [aesonQQ| {
      url : #{url},
      filename : #{name},
      fields : [#{field}]
    } |]

audioPronounceSentenceRequestFromTTS :: NFC.FlashCard -> ExceptT T.Text IO Value
audioPronounceSentenceRequestFromTTS flashCard = do
  content <-
    GAPI.synthesizeFromText
      . questionText
      . NFC.flashCardQuestion
      $ flashCard

  return $ dataBody content
  where
    dataBody :: BC.ByteString -> Value
    dataBody content =
      [aesonQQ| {
        data : #{TE.decodeUtf8 content},
        filename : "pronounce_sentence.mp3",
        fields : ["PronounceSentence"]
      } |]

-- | Rearrange the flash card to fit the Anki note format.
-- The answer is placed in the question field and the question is edited.
-- Content inside parentheses is extracted and placed in the answer field.
arrangeFlashCard :: NFC.FlashCard -> NFC.FlashCard
arrangeFlashCard card =
  card
    { NFC.flashCardAnswer = base,
      NFC.flashCardQuestion = editQuestion inflected (NFC.flashCardQuestion card)
    }
  where
    (base, inflected') = splitAtParentheses . NFC.flashCardAnswer $ card
    inflected = if T.null inflected' then base else inflected'

-- | Extracts the text before the first '(' and the text inside the parentheses.
splitAtParentheses :: T.Text -> (T.Text, T.Text)
splitAtParentheses s =
  let (before, after) = T.break (== '(') s
   in (before, extractContent after)
  where
    extractContent t =
      fromMaybe "" (T.stripPrefix "(" t >>= T.stripSuffix ")")

editQuestion :: T.Text -> T.Text -> T.Text
editQuestion answer =
  T.replace "(...)" replacement
    . T.replace "(…)" replacement
  where
    replacement = "<b>" <> answer <> "</b>"

questionText :: T.Text -> T.Text
questionText = T.replace "<b>" "" . T.replace "</b>" ""

-- | Extract the file name from an HTTP URL.
-- Returns Nothing if the URL cannot be parsed.
getFileNameFromURL :: String -> Maybe String
getFileNameFromURL url = do
  uri <- parseURI url
  let path = uriPath uri
      -- Reverse the path, take characters until the first '/'
      filename = reverse $ takeWhile (/= '/') $ reverse path
  return filename
