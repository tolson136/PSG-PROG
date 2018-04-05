if lastkey = keycode("ALT-C") then do:
def var datechoice as date format "99/99/9999".
clear frame cl-frame all no-pause.
ctr = 0.
form cl-date.notz cl-date.cl-date space(3) cl-date.month# space(3)
     cl-date.propsl# space(3) cl-date.item#
    with frame cl-frame scroll 1 15 down.
find first cl-date use-index indx06 where
	   cl-date.comp# = xcom and
	   cl-date.div# = xdiv and
	   cl-date.cust# = ticket.cust# and
	   cl-date.propsl# = ticket.propsl# and
	   cl-date.item# = ticket.item# and
	   cl-date.t-indx = ticket.t-indx no-error.
if not available cl-date then leave.
ctr = ctr + 1.
repeat counter = 1 to 15:
    pause 0.
    if cl-date.note-line1 > "" or
       cl-date.note-line2 > "" or
       cl-date.note-line3 > "" or
       cl-date.note-line4 > "" then
       do:
	update cl-date.notz = "*".
       end.
       else do:
	update cl-date.notz = " ".
       end.
    if cl-date.cl-date = 01/01/9999 then cl-date.notz = "?".
    display notz cl-date.cl-date cl-date.month# cl-date.propsl# cl-date.item#
		 cl-date.t-indx with frame cl-frame.
    down with frame cl-frame.
    find next cl-date use-index indx06 where
	      cl-date.comp# = xcom and
	      cl-date.div# = xdiv and
	      cl-date.cust# = ticket.cust# and
	      cl-date.propsl# = ticket.propsl# and
	      cl-date.item# = ticket.item# no-error.
    if not available cl-date then leave.
    ctr = ctr + 1.
end.
pause 0.
if ctr < 15
    then do:
	up ctr with frame cl-frame.
    end.
    else do:
	up ctr - 1 with frame cl-frame.
    end.
    oddchoice = frame-value.
    cl-d = integer(substring(oddchoice,4,2)).
    cl-m = integer(substring(oddchoice,1,2)).
    cl-y = integer(substring(oddchoice,7,4)).
    datechoice = date(cl-m,cl-d,cl-y).
    find first cl-date use-index indx06 where
	       cl-date.comp# = xcom and
	       cl-date.div# = xdiv and
	       cl-date.cust# = ticket.cust# and
	       cl-date.propsl# = ticket.propsl# and
	       cl-date.item# = ticket.item# and
	       cl-date.cl-date = datechoice and
	       cl-date.t-indx = ticket.t-indx no-error.
repeat:
status default "use up and down arrows".
choose row cl-date.cl-date no-error go-on(cursor-right)
	with frame cl-frame.
color display normal cl-date.cl-date with frame cl-frame.
if lastkey = keycode("cursor-down")
then do:
    oddchoice = frame-value.
    cl-d = integer(substring(oddchoice,4,2)).
    cl-m = integer(substring(oddchoice,1,2)).
    cl-y = integer(substring(oddchoice,7,4)).
    datechoice = date(cl-m,cl-d,cl-y).
    find last cl-date use-index indx06 where
	       cl-date.comp# = xcom and
	       cl-date.div# = xdiv and
	       cl-date.cust# = ticket.cust# and
	       cl-date.propsl# = ticket.propsl# and
	       cl-date.item# = ticket.item# and
	       cl-date.cl-date = datechoice no-error.
    find next cl-date use-index indx06 where
	      cl-date.comp# = xcom and
	      cl-date.div# = xdiv and
	      cl-date.cust# = ticket.cust# and
	      cl-date.propsl# = ticket.propsl# and
	      cl-date.item# = ticket.item# no-error.
    if not available cl-date then
    find first cl-date use-index indx06 where
	       cl-date.comp# = xcom and
	       cl-date.div# = xdiv and
	       cl-date.cust# = ticket.cust# and
	       cl-date.propsl# = ticket.propsl# and
	       cl-date.item# = ticket.item# no-error.
    if not available cl-date then leave.
    down with frame cl-frame.
    pause 0.
    if cl-date.note-line1 > "" or
       cl-date.note-line2 > "" or
       cl-date.note-line3 > "" or
       cl-date.note-line4 > "" then
       do:
	update cl-date.notz = "*".
       end.
       else do:
	update cl-date.notz = " ".
       end.
    if cl-date.cl-date = 01/01/9999 then cl-date.notz = "?".
    display cl-date.notz    column-label "Notes?"
	    cl-date.cl-date column-label "Cleaning!Date    "
	    cl-date.month#  column-label "Month of!Cleaning"
	    cl-date.propsl# column-label "Proposal!Number  "
	    cl-date.item#   column-label "Item   !Number "
	    cl-date.t-indx  column-label "Ticket!Index "
	    with frame cl-frame.
    next.
end.
if lastkey = keycode("cursor-up")
then do:
    oddchoice = frame-value.
    cl-d = integer(substring(oddchoice,4,2)).
    cl-m = integer(substring(oddchoice,1,2)).
    cl-y = integer(substring(oddchoice,7,4)).
    datechoice = date(cl-m,cl-d,cl-y).
    find first cl-date use-index indx06 where
	       cl-date.comp# = xcom and
	       cl-date.div# = xdiv and
	       cl-date.cust# = ticket.cust# and
	       cl-date.propsl# = ticket.propsl# and
	       cl-date.item# = ticket.item# and
	       cl-date.cl-date = datechoice no-error.
    find prev cl-date use-index indx06 where
	      cl-date.comp# = xcom and
	      cl-date.div# = xdiv and
	      cl-date.cust# = ticket.cust# and
	      cl-date.propsl# = ticket.propsl# and
	      cl-date.item# = ticket.item# no-error.
    if not available cl-date then
    find last cl-date use-index indx06 where
	      cl-date.comp# = xcom and
	      cl-date.div# = xdiv and
	      cl-date.cust# = ticket.cust# and
	      cl-date.propsl# = ticket.propsl# and
	      cl-date.item# = ticket.item# no-error.
    if not available cl-date then leave.
    up with frame cl-frame.
    pause 0.
    if cl-date.note-line1 > "" or
       cl-date.note-line2 > "" or
       cl-date.note-line3 > "" or
       cl-date.note-line4 > "" then
       do:
	update cl-date.notz = "*".
       end.
       else do:
	update cl-date.notz = " ".
       end.
    if cl-date.cl-date = 01/01/9999 then cl-date.notz = "?".
    display cl-date.notz cl-date.cl-date cl-date.month#
	    cl-date.propsl# cl-date.item# cl-date.t-indx
	with frame cl-frame.
    next.
end.
if lastkey = keycode("ESC") then leave.
if lastkey = keycode("ENTER") then do:
    pause 0.
    oddchoice = frame-value.
    cl-d = integer(substring(oddchoice,4,2)).
    cl-m = integer(substring(oddchoice,1,2)).
    cl-y = integer(substring(oddchoice,7,4)).
    datechoice = date(cl-m,cl-d,cl-y).
    find first cl-date use-index indx06 where
	       cl-date.comp# = xcom and
	       cl-date.div# = xdiv and
	       cl-date.cust# = ticket.cust# and
	       cl-date.propsl# = ticket.propsl# and
	       cl-date.item# = ticket.item# and
	       cl-date.t-indx = ticket.t-indx and
	       cl-date.cl-date = datechoice no-error.
    display cl-date.comp# label "Company"
	    cl-date.div# label "Division"
	    cl-date.cust# label "Customer"
	    cl-date.cl-date label "Clean Date"
	    cl-date.date-ret label "Date Ret'd"
	    cl-date.freq label "Frequency"
	    cl-date.month# label "Month"
	    cl-date.m-num label "Manager"
	    cl-date.route# label "Route"
	    cl-date.sub# label "Sub Route"
	    cl-date.t-indx label "Ticket Index" skip(2)
	    cl-date.note-line1 label "Cleaning   "
	    cl-date.note-line2 label "  Date     "
	    cl-date.note-line3 label "    Notes  "
	    cl-date.note-line4 label "           "
		with frame cl-b side-labels.
    update cl-date.note-line1
	   cl-date.note-line2
	   cl-date.note-line3
	   cl-date.note-line4
	   with frame cl-b side-labels
	   editing:
	    readkey.
	    if lastkey = keycode("ESC") then leave.
	    if frame-field <> "m-num" then do:
		apply lastkey.
		if go-pending then leave.
		else next.
	    end.
	   end.
    assign cl-date.
    leave.
    clear frame cl-frame all no-pause.
end.
end.
end.
