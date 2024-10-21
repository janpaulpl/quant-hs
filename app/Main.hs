{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import qualified Types
import qualified Analysis
import qualified Recommendation
import qualified Data
import Control.Exception (catch, SomeException)
import System.IO (hFlush, stdout)
import Control.Monad (forM_, when)
import System.Console.ANSI

main :: IO ()
main = do
  setTitle "Quant Finance Analysis Tool"
  printColored Blue Vivid "Quant Finance Analysis Tool"
  printSeparator
  watchlist <- Data.loadWatchlist "watchlist.json"
  let totalStocks = length watchlist
  forM_ (zip [1..] watchlist) $ \(index, item) -> do
    printColored Cyan Vivid $ "Analyzing stock " ++ show index ++ " of " ++ show totalStocks ++ ": " ++ Types.wiSymbol item
    result <- catch (analyzeStock item) handleException
    case result of
      Left err -> printColored Red Vivid $ "Error analyzing " ++ Types.wiSymbol item ++ ": " ++ err
      Right _ -> printColored Green Vivid $ "Completed analysis for " ++ Types.wiSymbol item
    showProgressBar index totalStocks
    printSeparator
  printColored Blue Vivid "Analysis complete for all stocks."

handleException :: SomeException -> IO (Either String ())
handleException e = return $ Left $ "An unexpected error occurred: " ++ show e

analyzeStock :: Types.WatchlistItem -> IO (Either String ())
analyzeStock item = do
  printColored Cyan Vivid $ "Analyzing " ++ Types.wiSymbol item
  hFlush stdout

  prices <- safely "Fetching stock data" $ do
    prices <- Data.fetchStockData (Types.wiSymbol item)
    printColored Green Dull $ "Fetched " ++ show (length prices) ++ " price points"
    return prices

  volatility <- safely "Calculating volatility" $
    return $ Analysis.calculateVolatility prices

  prediction <- safely "Predicting price" $
    return $ Analysis.predictPrice prices

  stock <- safely "Creating stock object" $
    return $ Types.Stock (Types.wiSymbol item) (last prices) 0

  techs <- safely "Calculating technical indicators" $
    return $ calculateTechnicalIndicators prices

  funds <- safely "Fetching fundamental data" $
    return $ fetchFundamentalData (Types.wiSymbol item)

  -- Moved the recommendation generation block here inside analyzeStock
  safely "Generating recommendation" $ do
    printColored Magenta Vivid "Generating recommendation..."
    recommendationResult <- Recommendation.recommendAction stock item techs funds
    case recommendationResult of
      Just (action, reason) -> do
        printColored Yellow Vivid "Analysis Results:"
        printKeyValue "Current Price" (show $ last prices)
        printKeyValue "Volatility" (show volatility)
        printKeyValue "Predicted Price" (show prediction)
        printKeyValue "Recommendation" (show action)
        printColored Cyan Dull $ "Reason: " ++ reason
      Nothing -> printColored Red Vivid "Recommendation generation timed out or encountered an error."
    putStrLn ""
    hFlush stdout

  return $ Right ()

-- safely helper function
safely :: String -> IO a -> IO a
safely description action = do
  printColored Yellow Dull $ "Starting: " ++ description
  result <- action
  printColored Green Dull $ "Completed: " ++ description
  return result

-- Helper functions for technical indicators and printing
calculateTechnicalIndicators :: [Double] -> Types.TechnicalIndicators
calculateTechnicalIndicators prices = 
  Types.TechnicalIndicators
    { Types.sma50 = Analysis.calculateSMA 50 prices
    , Types.sma200 = Analysis.calculateSMA 200 prices
    , Types.rsi = Analysis.calculateRSI prices
    , Types.macd = Analysis.calculateMACD prices
    , Types.bbands = Analysis.calculateBollingerBands prices
    }

fetchFundamentalData :: String -> Types.FundamentalData
fetchFundamentalData _ = 
  Types.FundamentalData
    { Types.peRatio = Nothing
    , Types.pbRatio = Nothing
    , Types.debtToEquity = Nothing
    , Types.freeCashFlow = Nothing
    }

printColored :: Color -> ColorIntensity -> String -> IO ()
printColored color intensity text = do
  setSGR [SetColor Foreground intensity color]
  putStrLn text
  setSGR [Reset]

printKeyValue :: String -> String -> IO ()
printKeyValue key value = do
  setSGR [SetColor Foreground Vivid Yellow]
  putStr $ key ++ ": "
  setSGR [SetColor Foreground Vivid White]
  putStrLn value
  setSGR [Reset]

printSeparator :: IO ()
printSeparator = putStrLn $ replicate 50 '─'

showProgressBar :: Int -> Int -> IO ()
showProgressBar current total = do
  let width = 40
      progress = round ((fromIntegral current / fromIntegral total :: Double) * fromIntegral width)
      bar = replicate progress '█' ++ replicate (width - progress) '░'
      percentage :: Int
      percentage = round ((fromIntegral current / fromIntegral total :: Double) * 100)
  putStr $ "\r[" ++ bar ++ "] " ++ show percentage ++ "%"
  hFlush stdout
  when (current == total) $ putStrLn ""