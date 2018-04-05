def var gweekly as log.
def var gday as log extent 7.
def var gweek as int.
def var gmonth as log extent 12.
display gweekly label "Print weekly tickets only?" skip(3).
display gweek label "Which week do you want to print?" skip(3)
.
display "Which day(s) of the week do you want to print?" skip(2)
        gday[1] label "Sat" skip gday[2] label "Sun" skip gday[3] label "Mon" skip 
        gday[4] label "Tue" skip gday[5] label "Wed" skip gday[6] label "Thr" skip 
        gday[7] label "Fri" skip(3).
display "What month do you want to print?" skip(2)
        gmonth[1] label "Jan" skip gmonth[2] label "Feb" skip gmonth[3] label "Mar" skip
        gmonth[4] label "Apr" skip gmonth[5] label "May" skip gmonth[6] label "Jun" skip
        gmonth[7] label "Jul" skip gmonth[8] label "Aug" skip gmonth[9] label "Sep" skip
        gmonth[10] label "Oct" skip gmonth[11] label "Nov" skip gmonth[12] label "Dec".
update gweekly gweek gday[1] gday[2] gday[3] gday[4] gday[5] gday[6] gday[7]
       gmonth[1] gmonth[2] gmonth[3] gmonth[4] gmonth[5] gmonth[6] gmonth[7] gmonth[8] gmonth[9]
       gmonth[10] gmonth[11] gmonth[12] with side-labels.
       