#!/usr/bin/sh
git checkout main
git --no-pager log --pretty='format:%as' --reverse \
    | uniq -c \
    | sed 's/^ *//' \
    | awk '{X += $1; print $2, X}' > time-commits.txt
./time-commits.gnu
echo "time-commits.svg written"
git checkout main
