output to c:\maria\maria.rpt.
for each pro-desp where wkday[1] = no and wkday[2] = no and wkday[3] = no and wkday[4] = no and
                        wkday[5] = no and wkday[6] = no and wkday[7] = no and sub# = 0 and
                        route > 0
                        and wks[1] = no and wks[2] = no and wks[3] = no and wks[4] = no
                        and wks[5] = no.
if freq = "on_call"  or freq = "one_time_cleaning" or freq = "initial_cleaning" or 
   freq = "special_cleaning" then next.                        
display cust# propsl# item# sub# route# freq.
end.
output close.