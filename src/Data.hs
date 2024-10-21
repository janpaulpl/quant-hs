{-# LANGUAGE OverloadedStrings #-}
module Data
    ( loadWatchlist
    , saveWatchlist
    , fetchStockData
    ) where

import Types
import qualified Data.ByteString.Lazy as B
import Data.Aeson (encode, decode)
import System.Process (readCreateProcess, shell)
import System.Directory (getCurrentDirectory, doesFileExist)
import System.FilePath ((</>))
import qualified Data.ByteString.Char8 as BC
import Control.Exception (catch, SomeException)

loadWatchlist :: FilePath -> IO [WatchlistItem]
loadWatchlist path = do
  exists <- doesFileExist path
  if exists
    then do
      content <- B.readFile path `catch` handleIOException
      case decode content of
        Just watchlist -> return watchlist
        Nothing -> error $ "Failed to parse watchlist at " ++ path ++ ". Please check the file format."
    else error $ "Watchlist file not found at " ++ path ++ ". Please create the file and try again."

handleIOException :: SomeException -> IO B.ByteString
handleIOException e = error $ "Error reading watchlist file: " ++ show e

saveWatchlist :: FilePath -> [WatchlistItem] -> IO ()
saveWatchlist path watchlist = B.writeFile path (encode watchlist)

fetchStockData :: String -> IO [Double]
fetchStockData symbolName = do
  currentDir <- getCurrentDirectory
  let ryeProjectDir = currentDir </> "stock_data_fetcher"
      command = "cd " ++ ryeProjectDir ++ " && rye run python main.py " ++ symbolName
  output <- readCreateProcess (shell command) ""
  case decode (B.fromStrict $ BC.pack output) of
    Just prices -> return prices
    Nothing -> error $ "Failed to parse stock data for " ++ symbolName