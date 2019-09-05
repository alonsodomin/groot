#!/bin/bash

set -e

echo "Generating source distribution package and uploading it to Hackage..."
mkdir -p "$HOME/.stack/upload"
echo "{ \"username\": \"$HACKAGE_USERNAME\", \"password\": \"$HACKAGE_PASSWORD\" }" > "$HOME/.stack/upload/credentials.json"