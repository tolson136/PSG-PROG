def var xdate as date format "99/99/9999".
def var xweek as integer.
repeat:
set xdate.
xweek = weekday(xdate).
if xweek <> 7 then do:
    display "Not a Saturday".
    next.
end.    
if day(xdate) < 8 then display "week 1" at 30.
if day(xdate) < 15 and day(xdate) > 7 then display "week 2" at 30.
if day(xdate) < 22 and day(xdate) > 14 then display "week 3" at 30.
if day(xdate) < 29 and day(xdate) > 21 then display "week 4" at 30.
if day(xdate) > 28 and
    (month(xdate) = month(xdate - 28)) then display "week 5" at 30.
end.