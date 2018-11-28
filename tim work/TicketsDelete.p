DEF VAR PropslList AS CHAR.
DEF VAR ItemList AS CHAR.
DEF VAR X AS INT.
DEF VAR tc AS CHAR.

PropslList = "214697,216909,216909,218109,219683,219718,220523,222474,223107,223196,223384,223551,223589,223997,224595,224623,227546,227721".
ItemList = "38,25,37,1,2,2,1,2,2,119,1,2,2,1,1,1,1,3".

DISPLAY NUM-ENTRIES(PropslList).

DISPLAY NUM-ENTRIES(ItemList).

OUTPUT TO c:\psg-wrk\tickets.out.
REPEAT X = 1 TO NUM-ENTRIES(PropslList):
   FOR EACH ticket WHERE
     ticket.propsl# = integer(ENTRY(x,propsllist)) AND
     ticket.item# = INTEGER(ENTRY(X,ItemList)) AND
     ticket.t-stat NE "X":
        DISPLAY
         TICKET.PROPSL#
         TICKET.ITEM#
         TICKET.T-INDX
         TICKET.ROUTE#
         TICKET.DATE-PRT 
         TICKET.DATE-RET   
         TICKET.TicketDate
         TICKET.T-STAT
         WITH WIDTH 120.
   END.
END.
  


