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
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Vector as V
import Network.HTTP.Simple
  ( getResponseBody,
    httpLBS,
    parseRequest_,
    setRequestBodyJSON,
    setRequestHeader,
    setRequestMethod,
  )
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
  response <- catch (httpLBS request) $
    \(SomeException e) ->
      throwE $ "Failed to add flash card to Anki :" <> T.pack (show e)

  eitherString2EitherTText
    . eitherDecode
    . getResponseBody
    $ response
  where
    request =
      setRequestHeader "Content-Type" ["application/json"]
        . setRequestBodyJSON (addNoteRequestBody flashCard)
        . setRequestMethod "POST"
        $ parseRequest_ "http://host.docker.internal:8765"

eitherString2EitherTText :: Either String a -> ExceptT T.Text IO a
eitherString2EitherTText = ExceptT . return . first T.pack

addNoteRequestBody :: NFC.FlashCard -> Value
addNoteRequestBody flashCard =
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
            "audio" : #{audioRequestBody flashCard},
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
    vs = V.fromList $ map (\(NFC.Image u) -> fileBody "picture" "WordImage" u) blocks

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

audioRequestBody :: NFC.FlashCard -> Value
audioRequestBody flashCard =
  Array vs
  where
    vs = V.fromList $ map (\(NFC.Audio u) -> fileBody "audio" "PronounceSentence" u) blocks

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
