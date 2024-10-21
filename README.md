# Quantitative Finance Tool in Haskell

This project provides advanced stock analysis and trading recommendations based on technical indicators, fundamental data, and user-defined parameters.

## Features

- Fetch and analyze historical stock data
- Calculate key technical indicators (SMA, RSI, MACD, Bollinger Bands)
- Incorporate fundamental analysis (P/E ratio, P/B ratio, Debt/Equity, Free Cash Flow)
- Custom watchlist with personalized buy/sell targets
- Risk-adjusted recommendations based on user's risk tolerance
- Comprehensive analysis report for each stock
- QuickCheck property-based testing suite

## Setup

1. Install Haskell and Stack
2. Clone this repository
3. Run `stack build` to compile the project
4. Set up your Python environment for data fetching:
   ```
   cd /path/to/your/project
   rye init stock_data_fetcher
   cd stock_data_fetcher
   rye add yfinance pandas
   rye sync
   ```

## Usage

1. Edit `watchlist.json` to add your stocks:
   ```json
   [
     {
       "symbol": "AAPL",
       "purchasePrice": 150.0,
       "targetSellPrice": 180.0,
       "riskTolerance": "Medium"
     },
     {
       "symbol": "GOOGL",
       "potentialBuyPrice": 2500.0,
       "riskTolerance": "High"
     }
   ]
   ```

2. Run the analysis:
   ```
   stack run
   ```

3. Review the output for each stock, which includes:
   - Current price and volatility
   - [Technical indicators](https://medium.com/@redsword_23261/rsi-macd-bollinger-bands-and-volume-based-hybrid-trading-strategy-fb1ecfd58e1b) (SMA, RSI, MACD, Bollinger Bands)
   - Fundamental metrics (if available)
   - Personalized recommendation (Strong Buy, Buy, Hold, Sell, Strong Sell)
   - Detailed reasoning for the recommendation

## Understanding the output
When you run the analysis, the tool provides real-time feedback in the terminal and saves detailed reports in the `results/` directory.

- **Technical Indicators**
   - SMA (Simple Moving Average): Indicates the trend direction. A higher SMA50 compared to SMA200 suggests an upward trend.
   - RSI (Relative Strength Index): Measures the speed and change of price movements. Values between 30 and 70 are considered normal.
   - MACD (Moving Average Convergence Divergence): Shows the relationship between two moving averages of a stock’s price. A positive MACD suggests upward momentum.
   - Bollinger Bands: Provide a relative definition of high and low prices. Prices touching the upper band may indicate overbought conditions.
- **Fundamental Metrics**
   - P/E Ratio (Price-to-Earnings): Valuation metric indicating the company's current share price relative to its per-share earnings.
   - P/B Ratio (Price-to-Book): Compares a company's market value to its book value.
   - Debt/Equity Ratio: Measures a company's financial leverage.
  - Free Cash Flow: Indicates the cash a company generates after accounting for cash outflows to  support operations and maintain its capital assets.
- **Recommendation Reasoning**
   - Technical Indicators: Whether they support a buy/sell decision based on trends and momentum.
   - Fundamental Analysis: The financial health and valuation of the company.
- Price Targets: How the current price relates to your purchase and sell targets.

## QuickCheck Testing

This project uses QuickCheck, a library for property-based testing in Haskell. Property-based testing allows us to define properties that our functions should satisfy, and then automatically test these properties against a large number of randomly generated inputs.

### What QuickCheck Tests in Our Project

Our QuickCheck test suite (`test/Spec.hs`) verifies several key properties of our quantitative finance tool:

1. **Volatility Calculation**
   - Property: `prop_volatilityNonNegative`
   - Ensures that calculated volatility is always non-negative, regardless of input price series.

2. **Simple Moving Average (SMA)**
   - Property: `prop_smaInRange`
   - Verifies that the SMA is always between the minimum and maximum of the input prices.

3. **Relative Strength Index (RSI)**
   - Property: `prop_rsiInRange`
   - Checks that the RSI is always between 0 and 100, as per its definition.

4. **Moving Average Convergence Divergence (MACD)**
   - Property: `prop_macdConsistency`
   - Ensures that the MACD histogram is correctly calculated as the difference between the MACD line and the signal line.

5. **Bollinger Bands**
   - Property: `prop_bollingerBandsOrder`
   - Verifies that the lower band ≤ middle band ≤ upper band for all input price series.

6. **Recommendation System**
   - Property: `prop_recommendationConsistency`
   - Checks that our recommendation function always returns a valid action (StrongBuy, Buy, Hold, Sell, or StrongSell) for any combination of inputs.

7. **Fundamental Analysis Score**
   - Property: `prop_fundamentalScoreInRange`
   - Ensures that the fundamental analysis always produces a score between 0 and 1.

8. **Technical Analysis Score**
   - Property: `prop_technicalScoreInRange`
   - Verifies that the technical analysis always produces a score between 0 and 1.

### Running QuickCheck Tests

To run the QuickCheck tests:

```
stack test
```

This command will run all the property-based tests and report any failures or successes.

## Customization

- Adjust the weighting of different factors in `src/Recommendation.hs`
- Modify technical indicator calculations in `src/Analysis.hs`
- Add new fundamental metrics in `src/Types.hs` and `src/Recommendation.hs`
- Extend the QuickCheck test suite in `test/Spec.hs` to cover new functionality

## Why Haskell for Quantitative Finance?

1. **Type Safety**: Haskell's strong type system helps prevent many common errors in financial calculations.

2. **Purity and Immutability**: Pure functions and immutable data structures make it easier to reason about complex financial models.

3. **Concurrency**: Haskell's lightweight threads and software transactional memory are excellent for handling large datasets and real-time data streams.

4. **Performance**: Haskell can be compiled to very efficient machine code, crucial for time-sensitive financial calculations.

5. **Expressiveness**: Haskell's high-level abstractions allow for clear and concise expression of complex financial algorithms.

6. **Testing**: Property-based testing with QuickCheck is powerful for ensuring correctness of financial models.

7. **Maintainability**: Haskell's strong typing and functional paradigm lead to more maintainable code, crucial for long-term projects.

8. **Mathematical Orientation**: Haskell's roots in mathematical theory make it well-suited for implementing quantitative finance models.

By leveraging Haskell's strengths, this tool provides a robust, efficient, and extensible platform for quantitative finance analysis.