#!.venv/bin/python3
from argparse import ArgumentParser
from datetime import datetime

import yfinance


if __name__ == "__main__":
    parser = ArgumentParser(description="Fetch historical stock data from Yahoo Finance")

    parser.add_argument("ticker", type=str, help="Stock ticker symbol")
    parser.add_argument("start_date", type=str, help="Start date in YYYY-MM-DD format")
    parser.add_argument("end_date", type=str, help="End date in YYYY-MM-DD format")
    parser.add_argument("output", type=str, help="Output CSV filename")

    args = parser.parse_args()
    ticker = args.ticker.upper()

    try:
        start_date = datetime.strptime(args.start_date, "%Y-%m-%d")
        end_date = datetime.strptime(args.end_date, "%Y-%m-%d")
    except ValueError:
        raise ValueError("Incorrect date format, should be YYYY-MM-DD")

    filename = args.output
    ohlc = yfinance.download(ticker, start=start_date, end=end_date, auto_adjust=True)

    ohlc["Close"].to_csv(filename, header=False, index=False)
