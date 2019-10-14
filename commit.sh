#!/bin/bash
git checkout HEAD~ -- lazpaint/release/bin/i18n/*.po
git add .
git status
echo "Type commit description (or press Enter to cancel):"
read commitdesc
if test -z "$commitdesc"
then
  git reset --
else
  git commit -m "$commitdesc"
fi
cd ..
