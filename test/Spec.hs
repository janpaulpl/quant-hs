{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Control.Monad (forM_)
import Data.Time (Day, fromGregorian)
import qualified Data.Vector as V

import Types
import Analysis
import Recommendation

main :: IO ()
main = do
  putStrLn "Running QuickCheck tests..."
  quickCheck prop_volatilityNonNegative
  quickCheck prop_smaInRange
  quickCheck prop_rsiInRange
  quickCheck prop_macdConsistency
  quickCheck prop_bollingerBandsOrder
  quickCheck prop_recommendationConsistency
  quickCheck prop_fundamentalScoreInRange
  quickCheck prop_technicalScoreInRange
  putStrLn "All tests completed."

-- Generators

genPositiveDouble :: Gen Double
genPositiveDouble = arbitrary `suchThat` (> 0)

genNonNegativeDouble :: Gen Double
genNonNegativeDouble = arbitrary `suchThat` (>= 0)

genNonEmptyString :: Gen String
genNonEmptyString = listOf1 arbitraryASCIIChar

genStock :: Gen Stock
genStock = Stock <$> genNonEmptyString <*> genPositiveDouble <*> arbitrary

genHistoricalData :: Gen HistoricalData
genHistoricalData = do
  y <- choose (2000, 2025)
  m <- choose (1,12)
  d <- choose (1,28)  -- Simplify date generation to avoid invalid dates
  let date = fromGregorian y m d
  o <- genPositiveDouble
  h <- genPositiveDouble `suchThat` (>= o)
  l <- genPositiveDouble `suchThat` (\x -> x <= o && x <= h)
  c <- genPositiveDouble `suchThat` (\x -> x >= l && x <= h)
  v <- abs <$> arbitrary
  return $ HistoricalData date o h l c v

genTechnicalIndicators :: Gen TechnicalIndicators
genTechnicalIndicators = do
  sma50 <- genPositiveDouble
  sma200 <- genPositiveDouble
  rsi <- choose (0, 100)  -- Ensure RSI is between 0 and 100
  macd <- (,,) <$> arbitrary <*> arbitrary <*> arbitrary
  lower <- genPositiveDouble
  mid <- genPositiveDouble `suchThat` (>= lower)
  upper <- genPositiveDouble `suchThat` (>= mid)
  return $ TechnicalIndicators sma50 sma200 rsi macd (lower, mid, upper)

genFundamentalData :: Gen FundamentalData
genFundamentalData = FundamentalData
  <$> genMaybePositiveDouble  -- peRatio
  <*> genMaybePositiveDouble  -- pbRatio
  <*> genMaybePositiveDouble  -- debtToEquity
  <*> genMaybePositiveDouble  -- freeCashFlow

genMaybePositiveDouble :: Gen (Maybe Double)
genMaybePositiveDouble = frequency
  [ (1, return Nothing)
  , (9, Just <$> genPositiveDouble)
  ]

genRiskTolerance :: Gen RiskTolerance
genRiskTolerance = elements [Low, Medium, High]

genWatchlistItem :: Gen WatchlistItem
genWatchlistItem = WatchlistItem
  <$> genNonEmptyString
  <*> genMaybePositiveDouble  -- purchasePrice
  <*> genMaybePositiveDouble  -- targetSellPrice
  <*> genMaybePositiveDouble  -- potentialBuyPrice
  <*> genRiskTolerance

-- Properties

prop_volatilityNonNegative :: Property
prop_volatilityNonNegative = forAll (listOf1 genPositiveDouble) $ \prices ->
  length prices >= 2 ==> calculateVolatility prices >= 0

prop_smaInRange :: Property
prop_smaInRange = forAll (listOf1 genPositiveDouble) $ \prices ->
  length prices >= 50 ==>
    let sma = calculateSMA 50 prices
        smaPrices = take 50 prices
    in sma >= minimum smaPrices && sma <= maximum smaPrices

prop_rsiInRange :: Property
prop_rsiInRange = forAll (listOf1 genPositiveDouble) $ \prices ->
  length prices >= 15 ==>
    let rsiValue = calculateRSI prices
    in rsiValue >= 0 && rsiValue <= 100

prop_macdConsistency :: Property
prop_macdConsistency = forAll (listOf1 genPositiveDouble) $ \prices ->
  length prices >= 35 ==>
    let (macdValue, signal, histogram) = calculateMACD prices
    in histogram == macdValue - signal

prop_bollingerBandsOrder :: Property
prop_bollingerBandsOrder = forAll (listOf1 genPositiveDouble) $ \prices ->
  length prices >= 20 ==>
    let (lower, mid, upper) = calculateBollingerBands prices
    in lower <= mid && mid <= upper

prop_recommendationConsistency :: Property
prop_recommendationConsistency = forAll ((,,,) <$> genStock <*> genWatchlistItem <*> genTechnicalIndicators <*> genFundamentalData) $
  \(stock, item, techs, funds) -> monadicIO $ do
    result <- run $ recommendAction stock item techs funds
    case result of
      Just (action, _) -> assert $ action `elem` [StrongBuy, Buy, Hold, Sell, StrongSell]
      Nothing -> assert False  -- Handle the case where recommendation fails

prop_fundamentalScoreInRange :: Property
prop_fundamentalScoreInRange = forAll genFundamentalData $ \funds ->
  let score = analyzeFundamentals funds
  in score >= 0 && score <= 1

prop_technicalScoreInRange :: Property
prop_technicalScoreInRange = forAll genTechnicalIndicators $ \techs ->
  let score = analyzeTechnicals techs
  in score >= 0 && score <= 1