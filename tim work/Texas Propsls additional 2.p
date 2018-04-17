def var x as int.
def var searchcust as int.

def var cust as char init "23,203,328,1829,3534,3875,4156,4891,5302,5303,5393,5522,5590,5792,5909,6174,6177,6326,6338,6346,6385,6446,6447,6451,6452,6557,6621,6629,6650,6673,6708,6734,6840,7144,7177,7180,7201,7345,7415,7415,7466,7525,7578".
   cust = cust + ",7619,7653,7958,7963,7966,8038,8147,8179,8198,8216,8236,8315,8316,8347,8363,8425,8452,8621,8690,8782,8845,8882,8941,9031,9258,9299,9327,9369,9416,9454,9476,9477,9497,9596,9625,9640,9650,9687".
   cust = cust + ",6403,6536,9592,5590,9581".
def var custcount as int.

repeat custcount = 1 to num-entries(cust):

   searchcust = INTEGER(ENTRY(custcount,cust)).
   
   for each  Propsl WHERE PROPSL.CUST# = searchcust:     
/*display propsl.cust# propsl.cust# propsl.propsl# propsl.div# L-STATE                  .*/



   for each ACCT-RCV WHERE ACCT-RCV.CUST# = searchcust:
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
for each ticket where 
 ticket.propsl# = propsl.propsl#:
 ticket.div# = propsl.div#.
end.
for each ticketdetail where 
 ticketdetail.propsl# = propsl.propsl#:
 ticketdetail.div# = propsl.div#.
end.
end.		

end.
