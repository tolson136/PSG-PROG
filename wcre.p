
/**********************************************/
/* wcre.p                                     */
/*                                            */
/* Print weekly tickets                       */
/*                                            */
/*  1/15   TO    Added laser print option     */
/*                                            */
/**********************************************/

DEFINE SHARED VARIABLE XCOM AS INTEGER FORMAT "ZZ".
DEFINE SHARED VARIABLE XDIV AS INTEGER FORMAT "ZZ".
DEFINE SHARED VARIABLE XCOM-N AS CHAR FORMAT "X(30)".
DEFINE SHARED VARIABLE XDIV-N AS CHAR FORMAT "X(30)".
DEFINE SHARED VARIABLE XPROG AS LOGICAL.

DEFINE NEW SHARED VARIABLE BEG# AS date FORMAT "99/99/9999"
  LABEL "ENTER NEW SATURDAY DATE FOR TICKETS RUN".
DEFINE NEW SHARED VARIABLE XROUTE AS INTEGER FORMAT "ZZ"
  LABEL "ENTER ROUTE NUMBER FOR THIS COMPANY AND DIVISION".
DEF NEW SHARED VAR XSUB AS INT FORMAT "ZZ"
  LABEL "ENTER SUB NUMBER FOR THIS COMPANY AND DIVISION".
DEFINE NEW SHARED VARIABLE D2 AS DECIMAL FORMAT "9999.99".
DEFINE NEW SHARED VARIABLE D3 AS DECIMAL FORMAT "9999".
DEFINE NEW SHARED VARIABLE D4 AS DECIMAL FORMAT "9999.99".
DEFINE NEW SHARED VARIABLE D5 AS INTEGER FORMAT "9999" LABEL "YEAR (4 DIGITS)".
DEFINE NEW SHARED VARIABLE DY AS INTEGER FORMAT "99".
DEFINE NEW SHARED VARIABLE UP-LIM AS INTEGER FORMAT "99".
define new shared variable xweek as integer.
DEF NEW SHARED VAR LaserPrinter AS LOG INIT No.
DEF NEW SHARED VAR NumTickets AS INT INIT 2.

define variable xday as integer.
DEF VAR JAN AS LOGICAL LABEL "PROCESS JANITORIAL TICKETS ?".
DEFINE VARIABLE H-FREQ AS CHAR FORMAT "X(10)".
DEFINE VARIABLE L-YEAR AS LOGICAL.
DEFINE VARIABLE TICKS AS INTEGER FORMAT "9999".
DEFINE VARIABLE F-TICK AS LOGICAL.
DEFINE VARIABLE H-WEEK AS INTEGER FORMAT "9".
DEFINE VARIABLE C-DATE AS DATE FORMAT "99/99/9999".

IF (USERID = "OPERATIONS") OR (USERID = "LANDMARK") OR (USERID = "GARCIA")
THEN DO:
    MESSAGE "YOU ARE NOT AUTHORIZED TO RUN THIS PROCEDURE".
    RETURN.
END.

OUTPUT TO TERMINAL.

DISPLAY SPACE(13)
   "W E E K L Y   T I C K E T   R E Q U E S T   S C R E E N   (wcre.p)"
    SKIP(1) SPACE(20)
   XCOM-N LABEL "COMPANY" SKIP(0) SPACE(20) XDIV-N LABEL "DIVISION"
   SKIP(5) WITH FRAME A NO-BOX SIDE-LABELS.
   clear.
UPDATE BEG# with no-box side-labels.
xday = weekday(beg#).
if xday <> 7 then do:
    message "Please enter a valid Saturday date".
    undo, retry.
end.

update SKIP(1) XROUTE SKIP(1) XSUB SKIP(1) JAN SKIP(1)
LaserPrinter LABEL "Send to Laser?" SKIP(1)
WITH NO-BOX SIDE-LABELS.
if day(beg#) < 8 then xweek = 1.
if day(beg#) < 15 and day(beg#) > 7 then xweek = 2.
if day(beg#) < 22 and day(beg#) > 14 then xweek = 3.
if day(beg#) < 29 and day(beg#) > 21 then xweek = 4.
if day(beg#) > 28 and (month(beg#) = month(beg# - 28)) then xweek = 5.
HIDE.
IF LASTKEY = KEYCODE("F4") THEN LEAVE.
IF LASTKEY = KEYCODE("ESC") THEN LEAVE.
    IF xsub = 0 and xroute > 0 THEN RUN wcrer.p.
