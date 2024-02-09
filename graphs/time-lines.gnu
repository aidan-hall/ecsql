#!/usr/bin/gnuplot
set term svg
set grid
set xlabel "Date"
set ylabel "SLoC"
set output "graphs/time-lines.svg"
set timefmt "%Y-%m-%d"
set xrange [1704067200 : 1710547200] # 01/01 - 16/03
set timefmt "%s"
set format x "%d/%m"
set xdata time
plot "graphs/time-lines.txt" using (timecolumn(1, "%s")):2 with lines notitle

