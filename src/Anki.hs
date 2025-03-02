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
import Data.Aeson (FromJSON, Value, eitherDecode, withObject, (.:), (.:?))
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types (FromJSON (parseJSON), Parser)
import Data.Bifunctor (Bifunctor (first))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
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

parseAnkiAddNoteResult :: Value -> Parser AnkiAddNoteResult
parseAnkiAddNoteResult = withObject "AnkiAddNoteResult" $ \o -> do
  e <- o .:? "error" :: Parser (Maybe T.Text)
  case e of
    Nothing -> AnkiAddNoteResultSuccess <$> o .: "result"
    Just err -> return $ AnkiAddNoteResultFailure err

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
            "options": {
                "allowDuplicate": false
            }
        }
    }
  }|]
  where
    flashCard' = arrangeFlashCard flashCard

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
editQuestion answer = T.replace "(…)" ("<b>" <> answer <> "</b>")