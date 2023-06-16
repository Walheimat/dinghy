#!/bin/bash

# Get the latest two revisions
REV_LIST=$(git rev-list --tags --max-count=2)

if [[ -n $REV_LIST ]]; then
  # shellcheck disable=SC2086
  IFS=$'\n' read -r -d '' -a TAGS < <(git describe --abbrev=0 --tags $REV_LIST)

  # Remove prefix
  PREV_VERSION=${TAGS[1]#v}
  CUR_VERSION=${TAGS[0]#v}
fi

FILE=$1

echo "$PREV_VERSION"

function dinghy::update_and_replace {
  local base
  base=$(basename "$FILE")

  echo "Updating ${base@Q}"

  sed -e "s/${PREV_VERSION}/${CUR_VERSION}/" "$FILE" > /tmp/"$base"-updated
  mv -f /tmp/"$base"-updated "$FILE"
}

function dinghy::can_update {
  if [[ -n $PREV_VERSION && -n $CUR_VERSION ]] && grep "$PREV_VERSION" "$FILE" >/dev/null; then
    return 0
  fi

  echo "File ${FILE@Q} cannot be updated"
  return 1
}

# If file contains the old version string, replace it
if dinghy::can_update; then
  dinghy::update_and_replace
fi
