#!/usr/bin/sh
git checkout main
git --no-pager log --pretty='format:%at %H' --reverse \
    | awk '{printf "%s ", $1; system("git checkout -q " $2);
            system("cloc --vcs=git --csv | grep SUM | cut -d, -f5");}' \
                | sed 1d \
                       > graphs/time-lines.txt
graphs/time-lines.gnu
git checkout main
