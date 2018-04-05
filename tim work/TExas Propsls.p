def var x as int.


/* Delete all proposals in Div 2 */
for each propsl where div = 2:


for each ar-desp where 
 ar-desp.propsl# = propsl.propsl# and
 ar-desp.div# = propsl.div#:
 delete ar-desp.
end. 
for each CallNotes where 
 CallNotes.propsl# = propsl.propsl# and
 CallNotes.div# = propsl.div#:
 delete callnotes.
end. 
for each cl-date where 
 cl-date.propsl# = propsl.propsl# and
 cl-date.div# = propsl.div#:
  delete cl-date.
end. 
for each employee where 
 employee.propsl# = propsl.propsl# and
 employee.div# = propsl.div#:
  delete employee.
end. 
for each invoice where 
 invoice.propsl# = propsl.propsl# and
 invoice.div# = propsl.div#:
 delete invoice.
end.
for each met-arch where 
 met-arch.propsl# = propsl.propsl# and
 met-arch.div# = propsl.div#:
 delete met-arch.
end.
for each metdesc where 
 metdesc.propsl# = propsl.propsl# and
 metdesc.div# = propsl.div#:
 delete metdesc.
end.
for each metpro where 
 metpro.propsl# = propsl.propsl# and
 metpro.div# = propsl.div#:
 delete metpro.
end.
for each notes where 
 notes.propsl# = propsl.propsl# and
 notes.div# = propsl.div#:
 delete notes.
end.
for each pro-desp where 
 pro-desp.propsl# = propsl.propsl# and
 pro-desp.div# = propsl.div#:
 delete pro-desp.
end.
for each propslprice where 
 propslprice.propsl# = propsl.propsl# and
 propslprice.div# = propsl.div#:
 delete propslprice.
end.
for each schedule where 
 schedule.propsl# = propsl.propsl# and
 schedule.div# = propsl.div#:
 delete schedule.
end.
for each ticket where 
 ticket.propsl# = propsl.propsl# and
 ticket.div# = propsl.div#:
 delete ticket.
end.
for each ticketdetail where 
 ticketdetail.propsl# = propsl.propsl# and
 ticketdetail.div# = propsl.div#:
 delete ticketdetail.
end.

delete propsl.

end.

/* move all Texas proposals to div 2*/
for each propsl where l-state = "TX":


for each ar-desp where 
 ar-desp.propsl# = propsl.propsl# and
 ar-desp.div# = propsl.div#:
 ar-desp.div# = 2.
end. 
for each CallNotes where 
 CallNotes.propsl# = propsl.propsl# and
 CallNotes.div# = propsl.div#:
 CallNotes.div# = 2. 
end. 
for each cl-date where 
 cl-date.propsl# = propsl.propsl# and
 cl-date.div# = propsl.div#:
 cl-date.div# = 2.
end. 
for each employee where 
 employee.propsl# = propsl.propsl# and
 employee.div# = propsl.div#:
 employee.div# = 2.
end. 
for each invoice where 
 invoice.propsl# = propsl.propsl# and
 invoice.div# = propsl.div#:
 invoice.div# = 2.
end.
for each met-arch where 
 met-arch.propsl# = propsl.propsl# and
 met-arch.div# = propsl.div#:
 met-arch.div# = 2.
end.
for each metdesc where 
 metdesc.propsl# = propsl.propsl# and
 metdesc.div# = propsl.div#:
 metdesc.div# = 2.
end.
for each metpro where 
 metpro.propsl# = propsl.propsl# and
 metpro.div# = propsl.div#:
 metpro.div# = 2.
end.
for each notes where 
 notes.propsl# = propsl.propsl# and
 notes.div# = propsl.div#:
 notes.div# = 2.
end.
for each pro-desp where 
 pro-desp.propsl# = propsl.propsl# and
 pro-desp.div# = propsl.div#:
 pro-desp.div# = 2.
end.
for each propslprice where 
 propslprice.propsl# = propsl.propsl# and
 propslprice.div# = propsl.div#:
 propslprice.div# = 2.
end.
for each schedule where 
 schedule.propsl# = propsl.propsl# and
 schedule.div# = propsl.div#:
 schedule.div# = 2.
end.
for each ticket where 
 ticket.propsl# = propsl.propsl# and
 ticket.div# = propsl.div#:
 ticket.div# = 2.
end.
for each ticketdetail where 
 ticketdetail.propsl# = propsl.propsl# and
 ticketdetail.div# = propsl.div#:
 ticketdetail.div# = 2.
end.
x = x + 1.

assign propsl.div# = 2.

end.
display x.
