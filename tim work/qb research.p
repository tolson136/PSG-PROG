
FOR EACH ticket WHERE
  ticket.propsl# = 227546 /*AND
  ticket.ITEM# = 1*/ AND
  ticket.MONTH# = 11 /*AND
  ticket.ITEM# = 3*/:
  
  DISPLAY Ticket WITH SIDE-LABELS 1 COL.
END.

/* 
FOR EACH invoice WHERE
   Invoice.PROPSL# = 224472 /*AND
   INVOICE.MONTH# = 10        */
   :
   
   DISPLAY Invoice WITH SIDE-LABELS 1 COL WITH WIDTH 130.
END.
*/

/*
FOR EACH acct-rcv WHERE
   acct-rcv.cust# = 5303:
   
   DISPLAY acct-rcv WITH SIDE-LABELS 1 COL.
   
END.
*/
