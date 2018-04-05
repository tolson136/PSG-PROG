output to c:\psg-work\missingweeks.out.

def var nowk as log init no no-undo.
def var daycount as int no-undo.


for each pro-desp where freq NE "ON_CALL" AND
                        freq NE "ONE_TIME_CLEANING"
                        NO-LOCK 
                        by comp# 
                        by div# 
                        by sub#
                        by route#
                        by propsl#
                        by item#
                        :
   nowk = no.
   repeat daycount = 1 to 7:
      if wkday[daycount] = yes then nowk = yes.
   end.
   if nowk then display
      comp#
      div#
      sub#
      route#
      cust#
      propsl#
      item#.
END.                        
