{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Anki as ANK
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified NotionFlashCard as NFC
import System.Environment (getArgs)
import System.IO (hSetEncoding, stdout, utf8)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  result <- runExceptT addCards
  case result of
    Left err -> TIO.putStrLn err
    Right rps -> showAddNoteReuslt rps

addCards :: ExceptT T.Text IO [(NFC.FlashCard, ANK.AnkiAddNoteResult)]
addCards = do
  env <- parseArgs
  flashCards <- NFC.getFlashCardsFromNotion
  when (null flashCards) $
    throwE "No flash cards to regist from Notion found"
  confirmToContinue flashCards
  zip flashCards <$> ANK.addNoteOfFlashCards env flashCards

showAddNoteReuslt :: [(NFC.FlashCard, ANK.AnkiAddNoteResult)] -> IO ()
showAddNoteReuslt resultPairs = do
  mapM_ showResult resultPairs
  where
    showResult (card, result) = do
      let ans = NFC.flashCardAnswer card
      case result of
        ANK.AnkiAddNoteResultSuccess _ -> TIO.putStrLn $ "[Success] " <> ans
        ANK.AnkiAddNoteResultFailure err -> TIO.putStrLn $ "[Error] " <> ans <> ":" <> err

confirmToContinue :: [NFC.FlashCard] -> ExceptT T.Text IO ()
confirmToContinue flashCards = do
  liftIO $ TIO.putStrLn "Do you want to continue to register these flash cards? [y/n]"
  liftIO $ TIO.putStrLn cardsText
  answer <- liftIO TIO.getLine
  when (answer /= "y") $
    throwE "Cancelled to regist flash cards"
  where
    cardsText = T.intercalate ", " $ map NFC.flashCardAnswer flashCards

parseArgs :: ExceptT T.Text IO ANK.AnkiRunningEnvironment
parseArgs = do
  args <- liftIO getArgs
  case args of
    [] -> do
      liftIO . TIO.putStrLn $ "runnning as local..."
      return ANK.Local
    ["d"] -> return ANK.Docker
    ["l"] -> return ANK.Local
    _ -> throwE "Invalid arguments"