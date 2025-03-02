{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module NotionFlashCard
  ( getFlashCardsFromNotion,
    FlashCard (..),
  )
where

import Control.Exception.Safe (SomeException (SomeException), catch)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), throwE)
import Data.Aeson
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types
import Data.Bifunctor (Bifunctor (first))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as TXT
import Data.Vector as V (Vector)
import Network.HTTP.Simple
  ( addRequestHeader,
    getResponseBody,
    getResponseStatusCode,
    httpLBS,
    parseRequest_,
    setRequestBodyJSON,
    setRequestHeader,
    setRequestMethod,
  )
import System.Environment (lookupEnv)

data FlashCard = FlashCard
  { flashCardID :: TXT.Text,
    flashCardQuestion :: TXT.Text,
    flashCardAnswer :: TXT.Text,
    flashCardIPA :: TXT.Text,
    flashCardSense :: TXT.Text,
    flashCardOtherIPA :: TXT.Text,
    flashCardMnemonic :: TXT.Text
  }
  deriving (Show)

instance FromJSON FlashCard where
  parseJSON = parseFlashCard

getFlashCardsFromNotion :: ExceptT TXT.Text IO [FlashCard]
getFlashCardsFromNotion = do
  token <- getBearerToken
  let request =
        addRequestHeader "Authorization" token
          . addRequestHeader "Notion-Version" "2022-06-28"
          . setRequestHeader "Content-Type" ["application/json"]
          . setRequestBodyJSON bodyCondition
          . setRequestMethod "POST"
          $ parseRequest_ "https://api.notion.com/v1/databases/3f62546fdc9e47d1920756a6dbc55cfa/query"
  response <- catch (httpLBS request) $
    \(SomeException e) -> throwE $ "Failed to get flash cards from Notion " <> TXT.pack (show e)

  case getResponseStatusCode response of
    200 -> do
      let body = getResponseBody response
          cards = parseEither parseFlashCards =<< eitherDecode body
      -- liftIO $ TIO.putStrLn . TL.toStrict . TLE.decodeUtf8 $ body
      ExceptT . return . first TXT.pack $ cards
    code ->
      throwE $
        "Failed to get flash cards from Notion : " <> (TXT.pack . show $ code)
  where
    bodyCondition =
      [aesonQQ|{
        "filter": {
          "property" : "基準日時",
          "date" : {
            "is_not_empty" : true
          }
        },
        "sorts": [
          {
            "property": "基準日時",
            "direction": "ascending"
          }
        ],
        "page_size": 2
      }|]

parseFlashCards :: Value -> Parser [FlashCard]
parseFlashCards = withObject "FlashCards" $ parseJSONList <=< (.: "results")

parseFlashCard :: Value -> Parser FlashCard
parseFlashCard = withObject "FlashCards" $ \o -> do
  cardID <- o .: "id"
  properties <- o .: "properties"
  question <- getTitleContent properties "問題"
  answer <- getRichTextContent properties "答え"
  ipa <- getRichTextContent properties "発音"
  sense <- getRichTextContent properties "意味"
  otherIPA <- getRichTextContent properties "その他発音"
  mnemonic <- getRichTextContent properties "語呂"

  return $ FlashCard cardID question answer ipa sense otherIPA mnemonic
  where
    getTitleContent :: Object -> Key -> Parser TXT.Text
    getTitleContent properties key = do
      property <- properties .: key
      richTexts <- property .: "title"
      withArray "title" getTextContents richTexts

    getRichTextContent :: Object -> Key -> Parser TXT.Text
    getRichTextContent properties key = do
      property <- properties .: key
      richTexts <- property .: "rich_text"
      withArray "rich_text" getTextContents richTexts

    getTextContent :: Value -> Parser TXT.Text
    getTextContent = withObject "title" $ \o -> do
      txt <- o .: "text"
      txt .: "content"

    getTextContents :: Array -> Parser TXT.Text
    getTextContents txts = do
      cs <- mapM getTextContent txts :: Parser (Vector TXT.Text)
      return $ foldl (<>) "" cs

getBearerToken :: ExceptT TXT.Text IO B.ByteString
getBearerToken = do
  token <- liftIO $ lookupEnv "PRIVATE_NOTION_TOKEN"
  case token of
    Just t -> return . BC.pack $ "Bearer " ++ t
    Nothing -> throwE "PRIVATE_NOTION_TOKEN is not set"