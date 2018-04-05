def var x as int.

def var prop as char init "227827,227828,227829,227830,227831,227832,227981,227983,227984,227985,227986,227827,227828,227829,227830,227831,227832,227981,227983,227984,227985,227986".
def var propcount as int.

repeat propcount = 1 to num-entries(prop):
/* move selected Texas proposals to div 2*/
   for each propsl where propsl.propsl# = INTEGER(ENTRY(propcount,prop)):


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

end.
display x.
