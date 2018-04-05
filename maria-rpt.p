output to c:\maria\report.txt.
for each pro-desp where sub# = 0 and route# > 0.
if freq = "ON_CALL" or freq = "WEEKLY" or freq = "ONE_TIME_CLEANING" or freq = "INITIAL_CLEANING"
    then next.
if wks[1] = yes or wks[2] = yes or wks[3] = yes or wks[4] = yes or wks[5] = yes then next.
display cust# propsl# item# freq route# sub# format 9.