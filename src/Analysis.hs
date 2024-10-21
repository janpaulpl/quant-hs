module Analysis
    ( calculateVolatility
    , calculateSMA
    , calculateRSI
    , calculateMACD
    , calculateBollingerBands
    , calculateEMA
    , predictPrice
    ) where

import qualified Data.Vector as V
import Statistics.Sample (variance)
import Debug.Trace (trace)

calculateVolatility :: [Double] -> Double
calculateVolatility prices = sqrt (variance (V.fromList prices) * 252)  -- Annualized volatility

calculateSMA :: Int -> [Double] -> Double
calculateSMA n prices = sum (take n prices) / fromIntegral n

calculateRSI :: [Double] -> Double
calculateRSI prices = 
  let gains = [max 0 (y - x) | (x, y) <- zip prices (tail prices)]
      losses = [max 0 (x - y) | (x, y) <- zip prices (tail prices)]
      avgGain = sum (take 14 gains) / 14
      avgLoss = sum (take 14 losses) / 14
      rs = if avgLoss == 0 then 0 else avgGain / avgLoss
  in 100 - (100 / (1 + rs))

calculateEMA :: Int -> [Double] -> [Double]
calculateEMA n prices =
  if length prices < n
  then []
  else
    let k = 2 / (fromIntegral n + 1)
        initialEMA = sum (take n prices) / fromIntegral n
        restPrices = drop n prices
        emaList = scanl (\ema price -> price * k + ema * (1 - k)) initialEMA restPrices
    in emaList

calculateMACD :: [Double] -> (Double, Double, Double)
calculateMACD prices =
  if length prices < 26
  then (0, 0, 0)  -- Not enough data
  else
    let ema12List = calculateEMA 12 prices
        ema26List = calculateEMA 26 prices
        -- Align the lists
        diffLength = length ema12List - length ema26List
        ema12ListAligned = drop diffLength ema12List
        macdLineList = zipWith (-) ema12ListAligned ema26List
        -- Ensure there is enough data for signal line calculation
        signalLineList = if length macdLineList >= 9
                         then calculateEMA 9 macdLineList
                         else []
        -- Align the lists for histogram calculation
        macdLineListAligned = drop (length macdLineList - length signalLineList) macdLineList
        histogramList = zipWith (-) macdLineListAligned signalLineList
        -- Get the last values
        macdLine = if null macdLineList then 0 else last macdLineList
        signalLine = if null signalLineList then 0 else last signalLineList
        histogram = if null histogramList then 0 else last histogramList
    in (macdLine, signalLine, histogram)

calculateBollingerBands :: [Double] -> (Double, Double, Double)
calculateBollingerBands prices =
  let sma = calculateSMA 20 prices
      stdDev = sqrt $ sum [(x - sma)^(2 :: Int) | x <- take 20 prices] / 20
      upper = sma + 2 * stdDev
      lower = sma - 2 * stdDev
  in (lower, sma, upper)

predictPrice :: [Double] -> Double
predictPrice prices = 
    let shortTermMA = calculateSMA 5 prices  -- 5-day moving average
        longTermMA = calculateSMA 20 prices  -- 20-day moving average
        lastPrice = last prices
        prediction = lastPrice + (shortTermMA - longTermMA)  -- Simple trend-based prediction
    in prediction