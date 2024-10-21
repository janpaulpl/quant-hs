import yfinance as yf
import sys
import json

def fetch_stock_data(symbol, period="1mo"):
    stock = yf.Ticker(symbol)
    history = stock.history(period=period)
    prices = history['Close'].tolist()
    return json.dumps(prices)

if __name__ == "__main__":
    if len(sys.argv) > 1:
        symbol = sys.argv[1]
        print(fetch_stock_data(symbol))
    else:
        print(json.dumps({"error": "No symbol provided"}))