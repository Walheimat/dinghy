#!/bin/bash

DESIRED_VERSION=$1; shift

# Get the latest revision
REV_LIST=$(git rev-list --tags --max-count=1)

if [[ -n $REV_LIST ]]; then
  # shellcheck disable=SC2086
  IFS=$'\n' read -r -d '' -a TAGS < <(git describe --abbrev=0 --tags $REV_LIST)

  LATEST_TAG_VERSION=${TAGS[0]#v}
else
  exit 1
fi

if [[ $LATEST_TAG_VERSION == "$DESIRED_VERSION" ]]; then
  echo "Latest tag version and desired version match ($LATEST_TAG_VERSION)"
  exit 0
fi

function dinghy::update_and_replace {
  local file=$1
  local latest=${2:-$LATEST_TAG_VERSION}
  local base
  base=$(basename "$file")

  echo "Updating ${base@Q} from $latest to $DESIRED_VERSION"

  sed -e "s/${latest}/${DESIRED_VERSION}/" "$file" > /tmp/"$base"-updated
  mv -f /tmp/"$base"-updated "$file"
}

function dinghy::can_update {
  local file=$1;
  if [[ -n $LATEST_TAG_VERSION && -n $DESIRED_VERSION ]] && grep "$LATEST_TAG_VERSION" "$file" >/dev/null; then
    return 0
  fi

  if grep "$DESIRED_VERSION" "$file" >/dev/null; then
    echo "File ${file@Q} has already been updated to $DESIRED_VERSION"
  else
    echo "File ${file@Q} cannot be updated from $LATEST_TAG_VERSION to $DESIRED_VERSION"
  fi
  return 1
}

FILES=("$@")

for i in "${FILES[@]}"; do
  # If file contains the old version string, replace it
  if dinghy::can_update "$i"; then
    dinghy::update_and_replace "$i"
  fi
done

if [[ -f $CHANGELOG_FILE ]]; then
  dinghy::update_and_replace "$CHANGELOG_FILE" "$CHANGELOG_HEADING"
else
  echo "${CHANGELOG_FILE@Q} is not a file"
fi
