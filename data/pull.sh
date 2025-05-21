START_DATE="2024-08-01"
END_DATE="2024-12-31"
DATA_DIR="data"
TICKERS="$DATA_DIR/index/US30-tickers.txt"


for TICKER in $(cat $TICKERS); do
    OUTPUT="$DATA_DIR/prices/$TICKER.txt"
    echo "Fetching data for $TICKER into $OUTPUT"

    ./data/download.py $TICKER $START_DATE $END_DATE $OUTPUT
done
