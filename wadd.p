/************************************************/
/* wadd.p                                       */
/*                                              */
/* Print 1 weekly ticket                        */
/*                                              */
/*  1/26/2018   TO    Added laser print option  */
/*                                              */
/************************************************/

DEFINE SHARED VARIABLE XCOM AS INTEGER FORMAT "ZZ".
DEFINE SHARED VARIABLE XDIV AS INTEGER FORMAT "ZZ".
DEFINE SHARED VARIABLE XCOM-N AS CHAR FORMAT "X(30)".
DEFINE SHARED VARIABLE XDIV-N AS CHAR FORMAT "X(30)".
DEFINE SHARED VARIABLE XOPR AS CHAR FORMAT "XXX".
DEF SHARED VAR XPROG AS LOGICAL.

DEFINE NEW SHARED VARIABLE BEG# AS date FORMAT "99/99/9999"
  LABEL "ENTER NEW SATURDAY DATE FOR TICKET".
DEF VAR JAN AS LOGICAL LABEL "PROCESS JANITORIAL TICKETS ?".
DEFINE NEW SHARED VARIABLE D2 AS DECIMAL FORMAT "9999.99".
DEFINE NEW SHARED VARIABLE D3 AS DECIMAL FORMAT "9999".
DEFINE NEW SHARED VARIABLE D4 AS DECIMAL FORMAT "9999.99".
DEFINE NEW SHARED VARIABLE D5 AS INTEGER FORMAT "9999" LABEL "YEAR (4 DIGITS)".
DEFINE NEW SHARED VARIABLE L-YEAR AS LOGICAL.
DEFINE NEW SHARED VARIABLE DY AS INTEGER FORMAT "99".
DEFINE NEW SHARED VARIABLE UP-LIM AS INTEGER FORMAT "99".
DEFINE NEW SHARED VARIABLE F-CUST AS DECIMAL FORMAT "ZZZZZZZZZZ"
    LABEL "CUSTOMER NUMBER".
DEFINE NEW SHARED VARIABLE F-PROP AS DECIMAL FORMAT "ZZZZZZZZZZ"
    LABEL "PROPOSAL NUMBER".
DEFINE NEW SHARED VARIABLE F-ITEM AS INTEGER FORMAT "ZZZZ"
    LABEL "ITEM NUMBER".
DEFINE NEW SHARED VARIABLE F-INDEX AS INTEGER FORMAT "ZZ"
    LABEL "TICK INDEX".
DEF NEW SHARED VAR LaserPrinter AS LOG INIT No.
DEF NEW SHARED VAR NumTickets AS INT INIT 2.    
    
    
def var xday as int.
def new shared var xweek as int.
def new shared var xroute as int format "ZZ"
    label "Enter Route Number".
def new shared var xsub as int format "ZZ"
    label "Enter Sub Number".
IF (USERID = "OPERATIONS") OR (USERID = "LANDMARK") OR (USERID = "GARCIA")
THEN DO:
    MESSAGE "YOU ARE NOT AUTHORIZED TO RUN THIS PROCEDURE".
    RETURN.
END.
REPEAT:
OUTPUT TO TERMINAL.
DISPLAY SPACE(5)
"W E E K L Y   S I N G L E   T I C K E T   R E Q U E S T   S C R E E N"
    SKIP(1) SPACE(20)
XCOM-N LABEL "COMPANY" SKIP(0) SPACE(20)
XDIV-N LABEL "DIVISION" SKIP(0) SPACE(20)
XOPR LABEL "OPERATOR"
SKIP(5) WITH FRAME A NO-BOX SIDE-LABELS.
IF LASTKEY = KEYCODE("F4") THEN LEAVE.
IF LASTKEY = KEYCODE("ESC") THEN LEAVE.
/* REPEAT:  */
IF LASTKEY = KEYCODE("F4") THEN LEAVE.
IF LASTKEY = KEYCODE("ESC") THEN LEAVE.
UPDATE BEG# WITH FRAME A NO-BOX SIDE-LABELS.
xday = weekday(beg#).
if xday <> 7 then do:
    message "Please enter a valid Saturday date".
    undo, retry.
end.

UPDATE SKIP(1) F-CUST F-PROP SKIP(0)
               F-ITEM F-INDEX skip(0)
               xsub space(5) xroute SKIP(1)
               LaserPrinter LABEL "Send to Laser?"
               WITH FRAME A NO-BOX SIDE-LABELS.
if day(beg#) < 8 then xweek = 1.
if day(beg#) < 15 and day(beg#) > 7 then xweek = 2.
if day(beg#) < 22 and day(beg#) > 14 then xweek = 3.
if day(beg#) < 29 and day(beg#) > 21 then xweek = 4.
if day(beg#) > 28 and (month(beg#) = month(beg# - 28)) then xweek = 5.
/* END.  */
IF LASTKEY = KEYCODE("F4") THEN LEAVE.
IF LASTKEY = KEYCODE("ESC") THEN LEAVE.
RUN WADDR.P.
END.
