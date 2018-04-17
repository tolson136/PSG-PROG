/*for each propsl where cust# = 5590:

/*display propsl with side-labels 1 col.*/

display propsl.comp# propsl.div# propsl.cust# propsl.propsl#.
*/


for each ticket where 
 ticket.propsl# = 214483 /*propsl.propsl#*/:
 display ticket.comp# ticket.div# ticket.cust# ticket.propsl# .
end.
/*
for each ticketdetail where 
 ticketdetail.propsl# = propsl.propsl#:
 display ticketdetail.div# /*= propsl.div#*/.
end.
*/

/***************
   for each ACCT-RCV WHERE ACCT-RCV.CUST# = propsl.cust#:
      assign ACCT-RCV.COMP# = propsl.comp#
      ACCT-RCV.DIV# = propsl.div#.
   end.

   for each ar-desp where                 
    ar-desp.propsl# = propsl.propsl#:
    ar-desp.div# = propsl.div#.
   end. 
for each CallNotes where 
 CallNotes.propsl# = propsl.propsl#:
 CallNotes.div# = propsl.div#. 
end. 
for each cl-date where 
 cl-date.propsl# = propsl.propsl#:
 cl-date.div# = propsl.div#.
end. 
for each employee where 
 employee.propsl# = propsl.propsl#:
 employee.div# = propsl.div#.
end. 
for each invoice where 
 invoice.propsl# = propsl.propsl#:
 invoice.div# = propsl.div#.
end.
for each met-arch where 
 met-arch.propsl# = propsl.propsl#:
 met-arch.div# = propsl.div#.
end.
for each metdesc where 
 metdesc.propsl# = propsl.propsl#:
 metdesc.div# = propsl.div#.
end.
for each metpro where 
 metpro.propsl# = propsl.propsl#:
 metpro.div# = propsl.div#.
end.
for each notes where 
 notes.propsl# = propsl.propsl#:
 notes.div# = propsl.div#.
end.
for each pro-desp where 
 pro-desp.propsl# = propsl.propsl#:
 pro-desp.div# = propsl.div#.
end.
for each propslprice where 
 propslprice.propsl# = propsl.propsl#:
 propslprice.div# = propsl.div#.
end.
for each schedule where 
 schedule.propsl# = propsl.propsl#:
 schedule.div# = propsl.div#.
end.
************/

/*end.*/
