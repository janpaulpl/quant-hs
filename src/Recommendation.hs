{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Recommendation
    ( Action(..)
    , recommendAction
    , analyzeTechnicals
    , analyzeFundamentals
    , analyzePriceTargets
    , weightedAverage
    , scoreToAction
    , generateReason
    ) where

import Types
import System.Timeout (timeout)
import Debug.Trace (trace)
import Control.Exception (catch, SomeException, evaluate)

data Action = StrongBuy | Buy | Hold | Sell | StrongSell
  deriving (Show, Eq, Ord)

recommendAction :: Stock -> WatchlistItem -> TechnicalIndicators -> FundamentalData -> IO (Maybe (Action, String))
recommendAction stock item techs funds = timeout (5 * 1000000) $ do  -- 5 second timeout
  putStrLn "Starting recommendation process..."
  
  putStrLn "Calculating Technical Score"
  technicalScore <- evaluate $ trace "Technical Score calculation" $ analyzeTechnicals techs
  putStrLn $ "Technical Score: " ++ show technicalScore
  
  putStrLn "Calculating Fundamental Score"
  fundamentalScore <- evaluate $ trace "Fundamental Score calculated" $ analyzeFundamentals funds
  putStrLn $ "Fundamental Score: " ++ show fundamentalScore
  
  putStrLn "Calculating Price Score"
  priceScore <- evaluate $ trace "Price Score calculated" $ analyzePriceTargets stock item
  putStrLn $ "Price Score: " ++ show priceScore
  
  putStrLn "Calculating Overall Score"
  overallScore <- evaluate $ trace "Overall Score calculated" $ 
        weightedAverage [(technicalScore, 0.4), (fundamentalScore, 0.3), (priceScore, 0.3)]
  putStrLn $ "Overall Score: " ++ show overallScore
  
  putStrLn "Determining Action"
  action <- evaluate $ trace "Action determined" $ scoreToAction overallScore
  putStrLn $ "Action: " ++ show action
  
  putStrLn "Generating Reason"
  reason <- evaluate $ trace "Reason generated" $ generateReason techs funds stock item overallScore
  putStrLn "Reason generated"
  
  putStrLn "Recommendation process completed."
  return (action, reason)
  `catch` \(e :: SomeException) -> do
    putStrLn $ "Error in recommendation process: " ++ show e
    return (Hold, "Error occurred during recommendation generation: " ++ show e)

analyzeTechnicals :: TechnicalIndicators -> Double
analyzeTechnicals TechnicalIndicators{..} = trace "Entering analyzeTechnicals" $ do
  let smaScore = trace ("SMA Score: " ++ show (if sma50 > sma200 then (1 :: Double) else (0 :: Double))) $ if sma50 > sma200 then 1 else 0
  let rsiScoreRaw = (rsi - 30) / 40  -- Normalized RSI score
  let rsiScore = max 0 $ min 1 $ trace ("RSI Score (raw): " ++ show rsiScoreRaw) rsiScoreRaw
  let (macdLine, signalLine, _) = macd
  let macdScore = trace ("MACD Score: " ++ show (if macdLine > signalLine then (1 :: Double) else (0 :: Double))) $ if macdLine > signalLine then 1 else 0
  let (lower, mid, upper) = bbands
  let bbandsDenominator = upper - lower
  let bbandsScoreRaw = if bbandsDenominator /= 0
                        then (mid - lower) / bbandsDenominator
                        else 0.5  -- Neutral score if denominator is zero
  let bbandsScore = max 0 $ min 1 $ trace ("Bollinger Bands Score (raw): " ++ show bbandsScoreRaw) bbandsScoreRaw
  trace "Calling weightedAverage in analyzeTechnicals" $
    weightedAverage [(smaScore, 0.3), (rsiScore, 0.3), (macdScore, 0.2), (bbandsScore, 0.2)]

analyzeFundamentals :: FundamentalData -> Double
analyzeFundamentals FundamentalData{..} =
  trace "Analyzing fundamentals" $
  let peScore = maybe 0.5 (\x -> min 1 (15 / x)) peRatio
      pbScore = maybe 0.5 (\x -> min 1 (3 / x)) pbRatio
      deScore = maybe 0.5 (\x -> min 1 (1 / x)) debtToEquity
      fcfScore = maybe 0.5 (\x -> min 1 (x / 1e9)) freeCashFlow
  in weightedAverage [(peScore, 0.3), (pbScore, 0.3), (deScore, 0.2), (fcfScore, 0.2)]

analyzePriceTargets :: Stock -> WatchlistItem -> Double
analyzePriceTargets Stock{..} WatchlistItem{..} =
  trace "Analyzing price targets" $
  case (purchasePrice, targetSellPrice, potentialBuyPrice) of
    (Just pp, Just sp, _) ->
      let currentReturn = (currentPrice - pp) / pp
          targetReturn = (sp - pp) / pp
      in currentReturn / targetReturn
    (Nothing, Nothing, Just bp) ->
      if currentPrice <= bp then 1 else bp / currentPrice
    _ -> 0.5  -- Neutral score if not enough information

weightedAverage :: [(Double, Double)] -> Double
weightedAverage scores = trace ("Entering weightedAverage with scores: " ++ show scores) $ do
  let weightedSum = sum [trace ("Score * Weight: " ++ show (score * weight)) $ score * weight | (score, weight) <- scores]
  let totalWeight = sum [trace ("Weight: " ++ show weight) $ weight | (_, weight) <- scores]
  trace ("Weighted Sum: " ++ show weightedSum ++ ", Total Weight: " ++ show totalWeight) $
    weightedSum / totalWeight

scoreToAction :: Double -> Action
scoreToAction score =
  trace "Determining action based on score" $
  if score >= 0.8 then StrongBuy
  else if score >= 0.6 then Buy
  else if score >= 0.4 then Hold
  else if score >= 0.2 then Sell
  else StrongSell

generateReason :: TechnicalIndicators -> FundamentalData -> Stock -> WatchlistItem -> Double -> String
generateReason techs funds Stock{..} WatchlistItem{..} score =
  trace "Generating recommendation reason" $
  let technicalReason = "Technical indicators " ++ (if analyzeTechnicals techs > 0.5 then "support" else "suggest caution for") ++ " this stock. "
      fundamentalReason = "Fundamental analysis " ++ (if analyzeFundamentals funds > 0.5 then "shows strength" else "raises concerns") ++ ". "
      priceReason = case (purchasePrice, targetSellPrice, potentialBuyPrice) of
        (Just pp, Just sp, _) -> "The stock is " ++ (if currentPrice > pp then "above" else "below") ++ " your purchase price and " ++
                                 (if currentPrice > sp then "has reached" else "is approaching") ++ " your target sell price. "
        (Nothing, Nothing, Just bp) -> "The stock is " ++ (if currentPrice <= bp then "at or below" else "above") ++ " your potential buy price. "
        _ -> "Consider setting specific price targets for this stock. "
      overallReason = "Overall, the analysis " ++ (if score > 0.5 then "favors" else "does not strongly support") ++ " a positive outlook. "
  in technicalReason ++ fundamentalReason ++ priceReason ++ overallReason