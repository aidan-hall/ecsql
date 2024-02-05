#!/usr/bin/gnuplot
set term svg
set grid
set xlabel "Date"
set ylabel "Commits"
set output "time-commits.svg"
set timefmt "%Y-%m-%d"
set format x "%d/%m"
set xdata time
set xrange ["2024-1-1" : "2024-3-15"]
plot "time-commits.txt" using (timecolumn(1, "%Y-%m-%d")):2 with lines notitle

