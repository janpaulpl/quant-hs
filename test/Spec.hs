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
genPositiveDouble = abs <$> (arbitrary :: Gen Double) `suchThat` (> 0)

genNonNegativeDouble :: Gen Double
genNonNegativeDouble = abs <$> (arbitrary :: Gen Double)

genStock :: Gen Stock
genStock = Stock <$> arbitrary <*> genPositiveDouble <*> arbitrary

genHistoricalData :: Gen HistoricalData
genHistoricalData = do
  d <- arbitrary
  o <- genPositiveDouble
  h <- genPositiveDouble `suchThat` (>= o)
  l <- genPositiveDouble `suchThat` (\x -> x <= o && x <= h)
  c <- genPositiveDouble `suchThat` (\x -> x >= l && x <= h)
  v <- abs <$> (arbitrary :: Gen Int)
  return $ HistoricalData d o h l c v

genTechnicalIndicators :: Gen TechnicalIndicators
genTechnicalIndicators = do
  sma50 <- genPositiveDouble
  sma200 <- genPositiveDouble
  rsi <- choose (0, 100)
  macd <- (,,) <$> arbitrary <*> arbitrary <*> arbitrary
  lower <- genPositiveDouble
  mid <- genPositiveDouble `suchThat` (>= lower)
  upper <- genPositiveDouble `suchThat` (>= mid)
  return $ TechnicalIndicators sma50 sma200 rsi macd (lower, mid, upper)

genFundamentalData :: Gen FundamentalData
genFundamentalData = FundamentalData
  <$> arbitrary
  <*> arbitrary
  <*> arbitrary
  <*> arbitrary

genWatchlistItem :: Gen WatchlistItem
genWatchlistItem = WatchlistItem
  <$> arbitrary
  <*> arbitrary
  <*> arbitrary
  <*> arbitrary
  <*> arbitrary

-- Properties

prop_volatilityNonNegative :: Property
prop_volatilityNonNegative = forAll (vectorOf 100 genPositiveDouble) $ \prices ->
  calculateVolatility prices >= 0

prop_smaInRange :: Property
prop_smaInRange = forAll (vectorOf 100 genPositiveDouble) $ \prices ->
  let sma = calculateSMA 50 prices
  in sma >= minimum prices && sma <= maximum prices

prop_rsiInRange :: Property
prop_rsiInRange = forAll (vectorOf 100 genPositiveDouble) $ \prices ->
  let rsi = calculateRSI prices
  in rsi >= 0 && rsi <= 100

prop_macdConsistency :: Property
prop_macdConsistency = forAll (vectorOf 100 genPositiveDouble) $ \prices ->
  let (macd, signal, histogram) = calculateMACD prices
  in histogram == macd - signal

prop_bollingerBandsOrder :: Property
prop_bollingerBandsOrder = forAll (vectorOf 100 genPositiveDouble) $ \prices ->
  let (lower, mid, upper) = calculateBollingerBands prices
  in lower <= mid && mid <= upper

prop_recommendationConsistency :: Property
prop_recommendationConsistency = forAll ((,,,) <$> genStock <*> genWatchlistItem <*> genTechnicalIndicators <*> genFundamentalData) $
  \(stock, item, techs, funds) ->
    let (action, _) = recommendAction stock item techs funds
    in action `elem` [StrongBuy, Buy, Hold, Sell, StrongSell]

prop_fundamentalScoreInRange :: Property
prop_fundamentalScoreInRange = forAll genFundamentalData $ \funds ->
  let score = analyzeFundamentals funds
  in score >= 0 && score <= 1

prop_technicalScoreInRange :: Property
prop_technicalScoreInRange = forAll genTechnicalIndicators $ \techs ->
  let score = analyzeTechnicals techs
  in score >= 0 && score <= 1