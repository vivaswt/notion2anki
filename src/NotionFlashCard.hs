{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module NotionFlashCard
  ( getFlashCardsFromNotion,
    FlashCard (..),
    BlockTypeObject (..),
    testShowFlashCardSimple,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Exception.Safe (SomeException (SomeException), catch)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT, throwE)
import Data.Aeson
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types
import Data.Bifunctor (Bifunctor (first))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
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
    flashCardMnemonic :: TXT.Text,
    flashCardBlockChildren :: [BlockTypeObject]
  }
  deriving (Show)

instance FromJSON FlashCard where
  parseJSON = parseFlashCard

type URL = TXT.Text

data BlockTypeObject = Image URL | Audio URL | Others
  deriving (Eq)

instance FromJSON BlockTypeObject where
  parseJSON = parseContentBlock

instance Show BlockTypeObject where
  show (Image url) = "Image: " ++ TXT.unpack url
  show (Audio url) = "Audio: " ++ TXT.unpack url
  show Others = "Others"

getFlashCardsFromNotion :: ExceptT TXT.Text IO [FlashCard]
getFlashCardsFromNotion = do
  token <- getBearerToken
  response <- catch (httpLBS . request $ token) $
    \(SomeException e) -> throwE $ "Failed to get flash cards from Notion " <> TXT.pack (show e)

  case getResponseStatusCode response of
    200 -> do
      let body = getResponseBody response
          cards = parseEither parseFlashCards =<< eitherDecode body
      cards' <- ExceptT . return . first TXT.pack $ cards
      mapM fillFlashCardBlockChildren cards'
    code ->
      throwE $
        "Failed to get flash cards from Notion : " <> (TXT.pack . show $ code)
  where
    request t =
      addRequestHeader "Authorization" t
        . addRequestHeader "Notion-Version" "2022-06-28"
        . setRequestHeader "Content-Type" ["application/json"]
        . setRequestBodyJSON bodyCondition
        . setRequestMethod "POST"
        $ parseRequest_ "https://api.notion.com/v1/databases/3f62546fdc9e47d1920756a6dbc55cfa/query"

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

  return $ FlashCard cardID question answer ipa sense otherIPA mnemonic []
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

getPageContent :: TXT.Text -> ExceptT TXT.Text IO BL.ByteString
getPageContent pageID = do
  token <- getBearerToken
  let request =
        addRequestHeader "Authorization" token
          . addRequestHeader "Notion-Version" "2022-06-28"
          . setRequestHeader "Content-Type" ["application/json"]
          . setRequestMethod "GET"
          . parseRequest_
          $ "https://api.notion.com/v1/blocks/" ++ TXT.unpack pageID ++ "/children"
  response <- catch (httpLBS request) $
    \(SomeException e) ->
      throwE $ "Failed to get flash card content from Notion : " <> TXT.pack (show e)

  case getResponseStatusCode response of
    200 -> do
      return . getResponseBody $ response
    code ->
      throwE $
        "Failed to get flash card content from Notion : "
          <> (TXT.pack . show $ code)
          <> " "
          <> (TXT.pack . show $ getResponseBody response)

parseContentBlock :: Value -> Parser BlockTypeObject
parseContentBlock = withObject "Block" $ \o -> do
  blockType <- o .: "type" :: Parser TXT.Text
  case blockType of
    "image" -> do
      im <- o .: "image"
      fl <- im .: "file" <|> im .: "external"
      url <- fl .: "url"
      return $ Image url
    "audio" -> do
      au <- o .: "audio"
      fl <- au .: "file" <|> au .: "external"
      url <- fl .: "url"
      return $ Audio url
    _ -> return Others

parseContentBlocks :: Value -> Parser [BlockTypeObject]
parseContentBlocks = withObject "Blocks" $ \o -> do
  results <- o .: "results" :: Parser [Value]
  mapM parseContentBlock results

fillFlashCardBlockChildren :: FlashCard -> ExceptT TXT.Text IO FlashCard
fillFlashCardBlockChildren card = do
  children <- getFlashCardBlockChildren $ flashCardID card
  return $ card {flashCardBlockChildren = children}

getFlashCardBlockChildren :: TXT.Text -> ExceptT TXT.Text IO [BlockTypeObject]
getFlashCardBlockChildren pageID = do
  responseBody <- getPageContent pageID
  let r = do
        contentJSON <- eitherDecode responseBody
        parseEither parseContentBlocks contentJSON
  eitherString2EitherTText r

eitherString2EitherTText :: Either String a -> ExceptT TXT.Text IO a
eitherString2EitherTText = ExceptT . return . first TXT.pack

showFlashCardSimple :: FlashCard -> IO ()
showFlashCardSimple card = do
  putStrLn $ "ID: " ++ TXT.unpack (flashCardID card)
  putStrLn $ "Question: " ++ TXT.unpack (flashCardQuestion card)
  putStrLn $ "Answer: " ++ TXT.unpack (flashCardAnswer card)
  putStrLn $ "IPA: " ++ TXT.unpack (flashCardIPA card)
  putStrLn $ "Sense: " ++ TXT.unpack (flashCardSense card)
  putStrLn $ "Other IPA: " ++ TXT.unpack (flashCardOtherIPA card)
  putStrLn $ "Mnemonic: " ++ TXT.unpack (flashCardMnemonic card)
  putStrLn "Block Children:"
  mapM_ showFlashCardBlockChildrenSimple (flashCardBlockChildren card)
  putStrLn ""

showFlashCardBlockChildrenSimple :: BlockTypeObject -> IO ()
showFlashCardBlockChildrenSimple (Image url) = putStrLn $ "Image: " ++ TXT.unpack url
showFlashCardBlockChildrenSimple (Audio url) = putStrLn $ "Audio: " ++ TXT.unpack url
showFlashCardBlockChildrenSimple Others = putStrLn "Others"

testShowFlashCardSimple :: IO ()
testShowFlashCardSimple = do
  cards <- runExceptT getFlashCardsFromNotion
  case cards of
    Left err -> putStrLn . TXT.unpack $ err
    Right cs -> mapM_ showFlashCardSimple cs