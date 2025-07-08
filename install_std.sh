#!/bin/bash
LANGUAGE_NAME="duck"
DEST_DIR="/usr/local/lib/$LANGUAGE_NAME"
SRC_DIR="./std"
if [ ! -d "$SRC_DIR" ]; then
  echo "Error: Source directory '$SRC_DIR' not found in the current path."
  echo "Please run this script from the directory containing the 'std' folder."
  exit 1
fi

if [ -d "$DEST_DIR" ]; then
  echo "Warning: Destination directory '$DEST_DIR' already exists."
else
  echo "Creating destination directory: $DEST_DIR"
  sudo mkdir -p "$DEST_DIR"
  if [ $? -ne 0 ]; then
    echo "Error: Failed to create destination directory. Do you have sudo permissions?"
    exit 1
  fi
fi

echo "Copying '$SRC_DIR' to '$DEST_DIR/std'..."
sudo cp -r "$SRC_DIR" "$DEST_DIR/"

if [ $? -eq 0 ]; then
  echo "Successfully moved the standard library."
  echo "New location: $DEST_DIR/std"

  echo "Setting permissions to be world-readable..."
  sudo chmod -R 755 "$DEST_DIR"
  echo "Done."
else
  echo "Error: Failed to move the directory. Please check permissions and paths."
  exit 1
fi

exit 0
