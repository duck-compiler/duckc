#!/bin/bash

LANGUAGE_NAME="duck"
DEST_PARENT_DIR="$HOME/.$LANGUAGE_NAME"
DEST_DIR="$DEST_PARENT_DIR/std"
SRC_DIR="./std"

if [ ! -d "$SRC_DIR" ]; then
  echo "Error: Source directory '$SRC_DIR' not found in the current path."
  echo "Please run this script from the root directory of the '$LANGUAGE_NAME' repository."
  exit 1
fi

if [ -d "$DEST_DIR" ]; then
  echo "An existing installation was found. Removing it before installing the new version..."
  rm -rf "$DEST_DIR"
  if [ $? -ne 0 ]; then
    echo "Error: Failed to remove the old standard library from '$DEST_DIR'."
    exit 1
  fi
  echo "Old version removed."
fi

echo "Creating destination directory: $DEST_PARENT_DIR"
mkdir -p "$DEST_PARENT_DIR"
if [ $? -ne 0 ]; then
  echo "Error: Failed to create destination directory '$DEST_PARENT_DIR'."
  exit 1
fi

echo "Copying '$SRC_DIR' to '$DEST_PARENT_DIR/'..."
cp -r "$SRC_DIR" "$DEST_PARENT_DIR/"

if [ $? -eq 0 ]; then
  echo "Successfully installed the standard library."
  echo "New location: $DEST_DIR"

  chmod -R 755 "$DEST_PARENT_DIR"
  echo "Permissions set. Installation complete."
else
  echo "Error: Failed to copy the standard library. Please check for issues."
  exit 1
fi

exit 0
