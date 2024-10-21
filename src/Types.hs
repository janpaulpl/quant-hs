{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Types 
  ( Stock(..)
  , HistoricalData(..)
  , TechnicalIndicators(..)
  , FundamentalData(..)
  , RiskTolerance(..)
  , WatchlistItem(..)
  ) where

import Data.Aeson
import Data.Time (Day)
import GHC.Generics (Generic)

data Stock = Stock
  { symbol :: String
  , currentPrice :: Double
  , quantity :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON Stock
instance FromJSON Stock

data HistoricalData = HistoricalData
  { date :: Day
  , open :: Double
  , high :: Double
  , low :: Double
  , close :: Double
  , volume :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON HistoricalData
instance FromJSON HistoricalData

data TechnicalIndicators = TechnicalIndicators
  { sma50 :: Double
  , sma200 :: Double
  , rsi :: Double
  , macd :: (Double, Double, Double)  -- (MACD, Signal, Histogram)
  , bbands :: (Double, Double, Double)  -- (Lower, Middle, Upper)
  } deriving (Show, Eq, Generic)

instance ToJSON TechnicalIndicators
instance FromJSON TechnicalIndicators

data FundamentalData = FundamentalData
  { peRatio :: Maybe Double
  , pbRatio :: Maybe Double
  , debtToEquity :: Maybe Double
  , freeCashFlow :: Maybe Double
  } deriving (Show, Eq, Generic)

instance ToJSON FundamentalData
instance FromJSON FundamentalData

data RiskTolerance = Low | Medium | High
  deriving (Show, Eq, Generic)

instance ToJSON RiskTolerance
instance FromJSON RiskTolerance

data WatchlistItem = WatchlistItem
  { wiSymbol :: String
  , purchasePrice :: Maybe Double
  , targetSellPrice :: Maybe Double
  , potentialBuyPrice :: Maybe Double
  , riskTolerance :: RiskTolerance
  } deriving (Show, Eq, Generic)

instance ToJSON WatchlistItem
instance FromJSON WatchlistItem