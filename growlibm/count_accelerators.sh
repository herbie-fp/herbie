#!/bin/bash

if [ "$#" -ne 2 ]; then
    echo "Error: Incorrect number of arguments."
    echo "Usage: $0 <accelerators_file> <search_file>"
    exit 1
fi

ACCELERATORS_FILE=$1
SEARCH_FILE=$2
TEMP_FILE=$(mktemp)

if [ ! -f "$ACCELERATORS_FILE" ]; then
    echo "Error: Accelerator file not found at '$ACCELERATORS_FILE'"
    exit 1
fi

if [ ! -f "$SEARCH_FILE" ]; then
    echo "Error: Search file not found at '$SEARCH_FILE'"
    exit 1
fi

while IFS= read -r line || [[ -n "$line" ]]; do
    if [[ "$line" =~ ^[0-9]+- ]]; then
        accelerator_id=$(echo "$line" | cut -d',' -f1)

        count=$(grep -o -w "$accelerator_id" "$SEARCH_FILE" | wc -l)

        echo "$line, $count" >> "$TEMP_FILE"
    else
        echo "$line" >> "$TEMP_FILE"
    fi
done < "$ACCELERATORS_FILE"

mv "$TEMP_FILE" "$ACCELERATORS_FILE"