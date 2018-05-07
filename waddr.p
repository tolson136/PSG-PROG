/***********************************************************/
/* waddr.p                                                 */
/*                                                         */
/* Print 1 weekly ticket                                   */
/*                                                         */
/*  1/26/2018   TO    Added laser print option             */
/*  4/18/2018   TO    Removed Start/Stop and Equip         */
/*                    Added TestMode                       */
/*                                                         */
/***********************************************************/

DEFINE SHARED VARIABLE TestMode AS LOGICAL.
DEFINE SHARED VARIABLE XCOM AS INTEGER FORMAT "ZZ".
DEFINE SHARED VARIABLE XDIV AS INTEGER FORMAT "ZZ".
DEFINE SHARED VARIABLE XCOM-N AS CHAR FORMAT "X(30)".
DEFINE SHARED VARIABLE XDIV-N AS CHAR FORMAT "X(30)".
DEFINE SHARED VARIABLE XOPR AS CHAR FORMAT "XXX".
DEFINE SHARED VARIABLE BEG# AS date FORMAT "99/99/9999"
  LABEL "ENTER MONTH & YEAR FOR WHICH YOU WANT TO RUN TICKETS".
DEFINE VARIABLE H-FREQ AS CHAR FORMAT "X(5)".
DEFINE SHARED VARIABLE D2 AS DECIMAL FORMAT "9999.99".
DEFINE SHARED VARIABLE D3 AS DECIMAL FORMAT "9999".
DEFINE SHARED VARIABLE D4 AS DECIMAL FORMAT "9999.99".
DEFINE SHARED VARIABLE D5 AS INTEGER FORMAT "9999" LABEL "YEAR (4 DIGITS)".
DEFINE SHARED VARIABLE L-YEAR AS LOGICAL.
DEFINE VARIABLE TICKS AS INTEGER FORMAT "9999".
DEFINE VARIABLE F-TICK AS LOGICAL.
DEFINE VARIABLE H-WEEK AS INTEGER FORMAT "9".
DEFINE SHARED VARIABLE DY AS INTEGER FORMAT "99".
DEFINE SHARED VARIABLE UP-LIM AS INTEGER FORMAT "99".
DEFINE VARIABLE C-DATE AS DATE FORMAT "99/99/9999".
DEFINE SHARED VARIABLE F-CUST AS DECIMAL FORMAT "ZZZZZZZZZZ"
    LABEL "CUSTOMER NUMBER".
DEFINE SHARED VARIABLE F-PROP AS DECIMAL FORMAT "ZZZZZZZZZZ"
    LABEL "PROPOSAL NUMBER".
DEFINE SHARED VARIABLE F-ITEM AS INTEGER FORMAT "ZZZZ"
    LABEL "ITEM NUMBER".
DEFINE SHARED VARIABLE F-INDEX AS INTEGER FORMAT "ZZ"
    LABEL "TICK INDEX".
def shared var xweek as int.
DEFINE SHARED VARIABLE LaserPrinter AS LOG.
DEFINE SHARED VARIABLE NumTickets AS INT.


def var gxmonth as int.
def shared var xroute as int format "ZZ"
    label "Enter Route Number".
def shared var xsub as int format "ZZ"
    label "Enter Sub Number".
DEF VAR FirstMonthDate AS DATE NO-UNDO.
DEF VAR LastMonthDate AS DATE NO-UNDO.    
DEFINE VARIABLE TicketsPerPage AS INT INIT 3.
DEFINE VARIABLE CurrentTicket  AS INT INIT 1. /* count within expect tickets per*/
DEFINE VARIABLE TicketCount    AS INT INIT 1. /* Count on page */
DEF VAR CoProposal AS CHAR FORMAT "X(30)".
DEF VAR ttDocXSequence AS INT.
DEF VAR FileName AS CHAR.
DEF VAR Cmd AS CHAR.

DEF TEMP-TABLE ttDocXPrint
  FIELD Idx AS INT 
  FIELD Week AS CHAR
  FIELD CoProposal AS CHAR
  FIELD Location AS CHAR
  FIELD Note1 AS CHAR
  FIELD Note2 AS CHAR
  FIELD Note3 AS CHAR
  FIELD Note4 AS CHAR
  FIELD Note5 AS CHAR
  FIELD Note6 AS CHAR
  FIELD Note7 AS CHAR
  FIELD Note8 AS CHAR
  FIELD Note9 AS CHAR
  FIELD Note10 AS CHAR
  FIELD StartEndCodEquip AS CHAR
  FIELD SpcIntr AS CHAR
  .
  
{slibooxml/slibdocx.i}
{include/stdutils.i}
{slib/slibos.i}
    
IF (USERID = "OPERATIONS") OR (USERID = "LANDMARK") OR (USERID = "GARCIA")
THEN DO:
    MESSAGE "YOU ARE NOT AUTHORIZED TO RUN THIS PROCEDURE".
    RETURN.
END.

ASSIGN 
  FirstMonthDate  = DATE(MONTH(BEG#),1,YEAR(BEG#))
  LastMonthDate   = DATE(MONTH(BEG#),DaysInMonth(MONTH(BEG#),YEAR(BEG#)),YEAR(BEG#))
  .

OUTPUT TO TERMINAL.
def var gweekly as log.
def var gday as log extent 7.
def var gweek as int.
def var gmonth as log extent 12.
display gweekly label "Print weekly tickets only?" skip(3).
display gweek label "Which week do you want to print?" skip(3)
.
display "Which day(s) of the week do you want to print?" skip(2)
        gday[1] label "Sat" skip gday[2] label "Sun" skip gday[3] label "Mon" skip 
        gday[4] label "Tue" skip gday[5] label "Wed" skip gday[6] label "Thr" skip 
        gday[7] label "Fri" skip(3).
display "What month do you want to print?" skip(2)
        gmonth[1] label "Jan" skip gmonth[2] label "Feb" skip gmonth[3] label "Mar" skip
        gmonth[4] label "Apr" skip gmonth[5] label "May" skip gmonth[6] label "Jun" skip
        gmonth[7] label "Jul" skip gmonth[8] label "Aug" skip gmonth[9] label "Sep" skip
        gmonth[10] label "Oct" skip gmonth[11] label "Nov" skip gmonth[12] label "Dec".
update gweekly gweek gday[1] gday[2] gday[3] gday[4] gday[5] gday[6] gday[7]
 gmonth[1] gmonth[2] gmonth[3] gmonth[4] gmonth[5] gmonth[6] gmonth[7] gmonth[8] gmonth[9]
       gmonth[10] gmonth[11] gmonth[12] with side-labels.
       if gmonth[1] then gxmonth = 1.
       if gmonth[2] then gxmonth = 2.
       if gmonth[3] then gxmonth = 3.
       if gmonth[4] then gxmonth = 4.
       if gmonth[5] then gxmonth = 5.
       if gmonth[6] then gxmonth = 6.
       if gmonth[7] then gxmonth = 7.
       if gmonth[8] then gxmonth = 8.
       if gmonth[9] then gxmonth = 9.
       if gmonth[10] then gxmonth = 10.
       if gmonth[11] then gxmonth = 11.
       if gmonth[12] then gxmonth = 12.
 
 IF LaserPrinter THEN DO:
    IF NOT TestMode THEN RUN docx_load("p:\template\Tickets2up.dfw").
    IF     TestMode THEN RUN docx_load("c:psg-prog\template\Tickets2up.dfw").
END. 
  FIND FIRST ACCT-RCV WHERE ACCT-RCV.COMP# = XCOM AND
                            ACCT-RCV.DIV# = XDIV  AND
                            ACCT-RCV.CUST# = F-CUST AND
                            ACCT-RCV.C-STATUS <> "I" 
                            NO-LOCK NO-ERROR.
  IF NOT AVAILABLE ACCT-RCV THEN DO:
      MESSAGE "CUSTOMER DOES NOT EXIST OR IS INACTIVE".
      NEXT.
  END.
  IF ACCT-RCV.C-STATUS = "I" THEN NEXT.
  FIND FIRST PROPSL WHERE 
     PROPSL.COMP# = XCOM AND
     PROPSL.DIV# = XDIV AND
     PROPSL.CUST# = F-CUST AND
     PROPSL.PROPSL# = F-PROP /*AND
     (Propsl.StartDate LE LastMonthDate OR Propsl.StartDate = ?) AND
     (Propsl.EndDate GE FirstMonthDate OR Propsl.EndDate = ?)*/
         NO-LOCK NO-ERROR.
  IF NOT AVAILABLE PROPSL THEN DO:
      MESSAGE "THAT PROPOSAL DOES NOT EXIST FOR THIS CUSTOMER".
      NEXT.
  END.
    IF PROPSL.JANITOR = yes THEN DO:
           MESSAGE "THIS IS A JANITORIAL TICKET".
           NEXT.
    END.
  IF NOT 
     ( Propsl.StartDate LE LastMonthDate OR Propsl.StartDate = ?) AND
     (Propsl.EndDate GE FirstMonthDate OR Propsl.EndDate = ?)  
     THEN DO:
       MESSAGE "This proposal it outside of its active dates" view-as alert-box.
       NEXT.
  END.   
  FIND FIRST PRO-DESP WHERE PRO-DESP.COMP# = XCOM AND
                            PRO-DESP.DIV# = XDIV AND
                            PRO-DESP.CUST# = F-CUST AND
                            PRO-DESP.PROPSL# = F-PROP AND
                           /*  pro-desp.wks[xweek] and  */
                            pro-desp.sub# = xsub and
                            pro-desp.route# = xroute and
                            PRO-DESP.ITEM# = F-ITEM 
                            NO-LOCK NO-ERROR.
  IF NOT AVAILABLE PRO-DESP THEN DO:
      MESSAGE "THAT ITEM DOES NOT EXIST FOR THIS CUSTOMER AND PROPOSAL".
      NEXT.
  END.
  
IF LASTKEY = KEYCODE("F4") THEN LEAVE.
IF LASTKEY = KEYCODE("ESC") THEN LEAVE.
      F-TICK = no.
      H-FREQ = "".
      IF PRO-DESP.FREQ = "DAILY" THEN DO:
               H-FREQ = "D".
               TICKS = 7.
               F-TICK = yes.
      END.
      IF PRO-DESP.FREQ = "TWO_TIMES_PER_WEEK" THEN DO:
          H-FREQ = "2XW".
          TICKS = 0.
          F-TICK = yes.
/*          REPEAT DY = 0 TO 6:            */
              C-DATE = BEG# + DY.
              H-WEEK = WEEKDAY(C-DATE).
              IF gday[2] AND PRO-DESP.WKDAY[2] THEN TICKS = TICKS + 1.
              IF gday[3] AND PRO-DESP.WKDAY[3] THEN TICKS = TICKS + 1.
              IF gday[4] AND PRO-DESP.WKDAY[4] THEN TICKS = TICKS + 1.
              IF gday[5] AND PRO-DESP.WKDAY[5] THEN TICKS = TICKS + 1.
              IF gday[6] AND PRO-DESP.WKDAY[6] THEN TICKS = TICKS + 1.
              IF gday[7] AND PRO-DESP.WKDAY[7] THEN TICKS = TICKS + 1.
              IF gday[1] AND PRO-DESP.WKDAY[1] THEN TICKS = TICKS + 1.
/*              
              IF H-WEEK = 1 AND PRO-DESP.WKDAY[2] THEN TICKS = TICKS + 1.
              IF H-WEEK = 2 AND PRO-DESP.WKDAY[3] THEN TICKS = TICKS + 1.
              IF H-WEEK = 3 AND PRO-DESP.WKDAY[4] THEN TICKS = TICKS + 1.
              IF H-WEEK = 4 AND PRO-DESP.WKDAY[5] THEN TICKS = TICKS + 1.
              IF H-WEEK = 5 AND PRO-DESP.WKDAY[6] THEN TICKS = TICKS + 1.
              IF H-WEEK = 6 AND PRO-DESP.WKDAY[7] THEN TICKS = TICKS + 1.
              IF H-WEEK = 7 AND PRO-DESP.WKDAY[1] THEN TICKS = TICKS + 1.
            
          END.  */
      END.
      IF PRO-DESP.FREQ = "THREE_TIMES_PER_WEEK" THEN DO:
          H-FREQ = "3XW".
          TICKS = 0.
          F-TICK = yes.
/*          REPEAT DY = 0 to 6:    */
              C-DATE = BEG# + DY.
              H-WEEK = WEEKDAY(C-DATE).
              IF gday[2] AND PRO-DESP.WKDAY[2] THEN TICKS = TICKS + 1.
              IF gday[3] AND PRO-DESP.WKDAY[3] THEN TICKS = TICKS + 1.
              IF gday[4] AND PRO-DESP.WKDAY[4] THEN TICKS = TICKS + 1.
              IF gday[5] AND PRO-DESP.WKDAY[5] THEN TICKS = TICKS + 1.
              IF gday[6] AND PRO-DESP.WKDAY[6] THEN TICKS = TICKS + 1.
              IF gday[7] AND PRO-DESP.WKDAY[7] THEN TICKS = TICKS + 1.
              IF gday[1] AND PRO-DESP.WKDAY[1] THEN TICKS = TICKS + 1.
/*              
              IF H-WEEK = 1 AND PRO-DESP.WKDAY[2] THEN TICKS = TICKS + 1.
              IF H-WEEK = 2 AND PRO-DESP.WKDAY[3] THEN TICKS = TICKS + 1.
              IF H-WEEK = 3 AND PRO-DESP.WKDAY[4] THEN TICKS = TICKS + 1.
              IF H-WEEK = 4 AND PRO-DESP.WKDAY[5] THEN TICKS = TICKS + 1.
              IF H-WEEK = 5 AND PRO-DESP.WKDAY[6] THEN TICKS = TICKS + 1.
              IF H-WEEK = 6 AND PRO-DESP.WKDAY[7] THEN TICKS = TICKS + 1.
              IF H-WEEK = 7 AND PRO-DESP.WKDAY[1] THEN TICKS = TICKS + 1.
            
          END.  */
      END.
      IF PRO-DESP.FREQ = "FOUR_TIMES_PER_WEEK" THEN DO:
          H-FREQ = "4XW".
          TICKS = 0.
          F-TICK = yes.
/*          REPEAT DY = 0 to 6:   */
              C-DATE = BEG# + DY.
              H-WEEK = WEEKDAY(C-DATE).
              IF gday[2] AND PRO-DESP.WKDAY[2] THEN TICKS = TICKS + 1.
              IF gday[3] AND PRO-DESP.WKDAY[3] THEN TICKS = TICKS + 1.
              IF gday[4] AND PRO-DESP.WKDAY[4] THEN TICKS = TICKS + 1.
              IF gday[5] AND PRO-DESP.WKDAY[5] THEN TICKS = TICKS + 1.
              IF gday[6] AND PRO-DESP.WKDAY[6] THEN TICKS = TICKS + 1.
              IF gday[7] AND PRO-DESP.WKDAY[7] THEN TICKS = TICKS + 1.
              IF gday[1] AND PRO-DESP.WKDAY[1] THEN TICKS = TICKS + 1.
/*              
              IF H-WEEK = 1 AND PRO-DESP.WKDAY[2] THEN TICKS = TICKS + 1.
              IF H-WEEK = 2 AND PRO-DESP.WKDAY[3] THEN TICKS = TICKS + 1.
              IF H-WEEK = 3 AND PRO-DESP.WKDAY[4] THEN TICKS = TICKS + 1.
              IF H-WEEK = 4 AND PRO-DESP.WKDAY[5] THEN TICKS = TICKS + 1.
              IF H-WEEK = 5 AND PRO-DESP.WKDAY[6] THEN TICKS = TICKS + 1.
              IF H-WEEK = 6 AND PRO-DESP.WKDAY[7] THEN TICKS = TICKS + 1.
              IF H-WEEK = 7 AND PRO-DESP.WKDAY[1] THEN TICKS = TICKS + 1.

          END.     */
      END.
      IF PRO-DESP.FREQ = "FIVE_TIMES_PER_WEEK" THEN DO:
          H-FREQ = "5XW".
          TICKS = 0.
          F-TICK = yes.
/*          REPEAT DY = 0 TO 6:    */
              C-DATE = BEG# + DY.
              H-WEEK = WEEKDAY(C-DATE).
              IF gday[2] AND PRO-DESP.WKDAY[2] THEN TICKS = TICKS + 1.
              IF gday[3] AND PRO-DESP.WKDAY[3] THEN TICKS = TICKS + 1.
              IF gday[4] AND PRO-DESP.WKDAY[4] THEN TICKS = TICKS + 1.
              IF gday[5] AND PRO-DESP.WKDAY[5] THEN TICKS = TICKS + 1.
              IF gday[6] AND PRO-DESP.WKDAY[6] THEN TICKS = TICKS + 1.
              IF gday[7] AND PRO-DESP.WKDAY[7] THEN TICKS = TICKS + 1.
              IF gday[1] AND PRO-DESP.WKDAY[1] THEN TICKS = TICKS + 1.
/*              
              IF H-WEEK = 1 AND PRO-DESP.WKDAY[2] THEN TICKS = TICKS + 1.
              IF H-WEEK = 2 AND PRO-DESP.WKDAY[3] THEN TICKS = TICKS + 1.
              IF H-WEEK = 3 AND PRO-DESP.WKDAY[4] THEN TICKS = TICKS + 1.
              IF H-WEEK = 4 AND PRO-DESP.WKDAY[5] THEN TICKS = TICKS + 1.
              IF H-WEEK = 5 AND PRO-DESP.WKDAY[6] THEN TICKS = TICKS + 1.
              IF H-WEEK = 6 AND PRO-DESP.WKDAY[7] THEN TICKS = TICKS + 1.
              IF H-WEEK = 7 AND PRO-DESP.WKDAY[1] THEN TICKS = TICKS + 1.

          END. */
      END.
      IF PRO-DESP.FREQ = "SIX_TIMES_PER_WEEK" THEN DO:
          H-FREQ = "6XW".
          TICKS = 0.
          F-TICK = yes.
/*          REPEAT DY = 0 TO 6:   */
              C-DATE = BEG# + DY.
              H-WEEK = WEEKDAY(C-DATE).
              IF gday[2] AND PRO-DESP.WKDAY[2] THEN TICKS = TICKS + 1.
              IF gday[3] AND PRO-DESP.WKDAY[3] THEN TICKS = TICKS + 1.
              IF gday[4] AND PRO-DESP.WKDAY[4] THEN TICKS = TICKS + 1.
              IF gday[5] AND PRO-DESP.WKDAY[5] THEN TICKS = TICKS + 1.
              IF gday[6] AND PRO-DESP.WKDAY[6] THEN TICKS = TICKS + 1.
              IF gday[7] AND PRO-DESP.WKDAY[7] THEN TICKS = TICKS + 1.
              IF gday[1] AND PRO-DESP.WKDAY[1] THEN TICKS = TICKS + 1.
/*              
              IF H-WEEK = 1 AND PRO-DESP.WKDAY[2] THEN TICKS = TICKS + 1.
              IF H-WEEK = 2 AND PRO-DESP.WKDAY[3] THEN TICKS = TICKS + 1.
              IF H-WEEK = 3 AND PRO-DESP.WKDAY[4] THEN TICKS = TICKS + 1.
              IF H-WEEK = 4 AND PRO-DESP.WKDAY[5] THEN TICKS = TICKS + 1.
              IF H-WEEK = 5 AND PRO-DESP.WKDAY[6] THEN TICKS = TICKS + 1.
              IF H-WEEK = 6 AND PRO-DESP.WKDAY[7] THEN TICKS = TICKS + 1.
              IF H-WEEK = 7 AND PRO-DESP.WKDAY[1] THEN TICKS = TICKS + 1.

          END.    */
      END.
      IF PRO-DESP.FREQ = "WEEKLY" THEN DO:
          H-FREQ = "W".
          TICKS = 0.
          F-TICK = yes.
/*          REPEAT DY = 0 TO 6:   */
              C-DATE = BEG# + DY.
              H-WEEK = WEEKDAY(C-DATE).
              IF gday[2] AND PRO-DESP.WKDAY[2] THEN TICKS = TICKS + 1.
              IF gday[3] AND PRO-DESP.WKDAY[3] THEN TICKS = TICKS + 1.
              IF gday[4] AND PRO-DESP.WKDAY[4] THEN TICKS = TICKS + 1.
              IF gday[5] AND PRO-DESP.WKDAY[5] THEN TICKS = TICKS + 1.
              IF gday[6] AND PRO-DESP.WKDAY[6] THEN TICKS = TICKS + 1.
              IF gday[7] AND PRO-DESP.WKDAY[7] THEN TICKS = TICKS + 1.
              IF gday[1] AND PRO-DESP.WKDAY[1] THEN TICKS = TICKS + 1.
/*              
              IF H-WEEK = 1 AND PRO-DESP.WKDAY[2] THEN TICKS = TICKS + 1.
              IF H-WEEK = 2 AND PRO-DESP.WKDAY[3] THEN TICKS = TICKS + 1.
              IF H-WEEK = 3 AND PRO-DESP.WKDAY[4] THEN TICKS = TICKS + 1.
              IF H-WEEK = 4 AND PRO-DESP.WKDAY[5] THEN TICKS = TICKS + 1.
              IF H-WEEK = 5 AND PRO-DESP.WKDAY[6] THEN TICKS = TICKS + 1.
              IF H-WEEK = 6 AND PRO-DESP.WKDAY[7] THEN TICKS = TICKS + 1.
              IF H-WEEK = 7 AND PRO-DESP.WKDAY[1] THEN TICKS = TICKS + 1.

          END.   */
      END.
      IF PRO-DESP.FREQ = "EVERY_TWO_WEEKS" THEN DO:
          H-FREQ = "2W".
          TICKS = 0.
          F-TICK = yes.
          IF PRO-DESP.CL-DATE + 14 >= BEG# AND
             PRO-DESP.CL-DATE + 14 <= BEG# + 6
             THEN TICKS = TICKS + 1.
          IF PRO-DESP.CL-DATE + 28 >= BEG# AND
             PRO-DESP.CL-DATE + 28 <= BEG# + 6
             THEN TICKS = TICKS + 1.
          IF PRO-DESP.CL-DATE + 42 >= BEG# AND
             PRO-DESP.CL-DATE + 42 <= BEG# + 6
             THEN TICKS = TICKS + 1.
          IF PRO-DESP.CL-DATE + 56 >= BEG# AND
             PRO-DESP.CL-DATE + 56 <= BEG# + 6
             THEN TICKS = TICKS + 1.
          IF PRO-DESP.CL-DATE + 70 >= BEG# AND
             PRO-DESP.CL-DATE + 70 <= BEG# + 6
             THEN TICKS = TICKS + 1.
          IF PRO-DESP.CL-DATE = 01/01/1901 THEN TICKS = 2.
      END.
      IF PRO-DESP.FREQ = "EVERY_THREE_WEEKS" THEN DO:
          H-FREQ = "3W".
          TICKS = 0.
          F-TICK = yes.
          IF PRO-DESP.CL-DATE + 21 >= BEG# AND
             PRO-DESP.CL-DATE + 21 <= BEG# + 6
             THEN TICKS = TICKS + 1.
          IF PRO-DESP.CL-DATE + 42 >= BEG# AND
             PRO-DESP.CL-DATE + 42 <= BEG# + 6
             THEN TICKS = TICKS + 1.
          IF PRO-DESP.CL-DATE + 63 >= BEG# AND
             PRO-DESP.CL-DATE + 63 <= BEG# + 6
             THEN TICKS = TICKS + 1.
          IF PRO-DESP.CL-DATE = 01/01/1901 THEN TICKS = 1.
      END.
      IF PRO-DESP.FREQ = "EVERY_FOUR_WEEKS" THEN DO:
          H-FREQ = "4W".
          TICKS = 0.
          F-TICK = yes.
          IF PRO-DESP.CL-DATE + 28 >= BEG# AND
             PRO-DESP.CL-DATE + 28 <= BEG# + 6
             THEN TICKS = TICKS + 1.
          IF PRO-DESP.CL-DATE + 56 >= BEG# AND
             PRO-DESP.CL-DATE + 56 <= BEG# + 6
             THEN TICKS = TICKS + 1.
          IF PRO-DESP.CL-DATE + 84 >= BEG# AND
             PRO-DESP.CL-DATE + 84 <= BEG# + 6
             THEN TICKS = TICKS + 1.
          IF PRO-DESP.CL-DATE = 01/01/1901 THEN TICKS = 1.
      END.

      IF PRO-DESP.FREQ = "EVERY_FIVE_WEEKS" THEN DO:
          H-FREQ = "5W".
          TICKS = 0.
          F-TICK = yes.
          IF PRO-DESP.CL-DATE + 35 >= BEG# AND
             PRO-DESP.CL-DATE + 35 <= BEG# + 6
             THEN TICKS = TICKS + 1.
          IF PRO-DESP.CL-DATE + 70 >= BEG# AND
             PRO-DESP.CL-DATE + 70 <= BEG# + 6
             THEN TICKS = TICKS + 1.
          IF PRO-DESP.CL-DATE = 01/01/1901 THEN TICKS = 1.
      END.
      IF PRO-DESP.FREQ = "EVERY_SIX_WEEKS" THEN DO:
          H-FREQ = "6W".
          TICKS = 0.
          F-TICK = yes.
          IF PRO-DESP.CL-DATE + 42 >= BEG# AND
             PRO-DESP.CL-DATE + 42 <= BEG# + 6
             THEN TICKS = TICKS + 1.
          IF PRO-DESP.CL-DATE + 84 >= BEG# AND
             PRO-DESP.CL-DATE + 84 <= BEG# + 6
             THEN TICKS = TICKS + 1.
          IF PRO-DESP.CL-DATE = 01/01/1901 THEN TICKS = 1.
      END.
      IF PRO-DESP.FREQ = "EVERY_EIGHT_WEEKS" THEN DO:
          H-FREQ = "8W".
          TICKS = 0.
          F-TICK = yes.
          IF PRO-DESP.CL-DATE + 56 >= BEG# AND
             PRO-DESP.CL-DATE + 56 <= BEG# + 6
             THEN TICKS = TICKS + 1.
          IF PRO-DESP.CL-DATE + 112 >= BEG# AND
             PRO-DESP.CL-DATE + 112 <= BEG# + 6
             THEN TICKS = TICKS + 1.
          IF PRO-DESP.CL-DATE = 01/01/1901 THEN TICKS = 1.
      END.
      IF PRO-DESP.FREQ = "MONTHLY" and pro-desp.wks[gweek] THEN DO:
          H-FREQ = "M".
          TICKS = 1.
          F-TICK = yes.
      END.
      IF PRO-DESP.FREQ = "QUARTERLY" AND PRO-DESP.MTH[gxmonth] and pro-desp.wks[gweek]
        THEN DO:
          H-FREQ = "Q".
          TICKS = 1.
          F-TICK = yes.
      END.
      IF PRO-DESP.FREQ = "TWO_TIMES_PER_MONTH" and pro-desp.wks[gweek] THEN DO:
          H-FREQ = "2XM".
          TICKS = 1.
          F-TICK = yes.
      END.
      IF PRO-DESP.FREQ = "ANNUAL" AND PRO-DESP.MTH[gxmonth] and pro-desp.wks[gweek]
        THEN DO:
          H-FREQ = "A".
          TICKS = 1.
          F-TICK = yes.
      END.
      IF PRO-DESP.FREQ = "TWO_TIMES_PER_YEAR" AND PRO-DESP.MTH[gxmonth] and
       pro-desp.wks[gweek]
        THEN DO:
          H-FREQ = "2XYR".
          TICKS = 1.
          F-TICK = yes.
      END.
      IF PRO-DESP.FREQ = "THREE_TIMES_PER_YEAR" AND PRO-DESP.MTH[gxmonth] and
       pro-desp.wks[gweek]
        THEN DO:
          H-FREQ = "3XYR".
          TICKS = 1.
          F-TICK = yes.
      END.
      IF PRO-DESP.FREQ = "FOUR_TIMES_PER_YEAR" AND PRO-DESP.MTH[gxmonth] and
       pro-desp.wks[gweek]
        THEN DO:
          H-FREQ = "4XYR".
          TICKS = 1.
          F-TICK = yes.
      END.
      IF PRO-DESP.FREQ = "FIVE_TIMES_PER_YEAR" AND PRO-DESP.mth[gxmonth] and
       pro-desp.wks[gweek]
        THEN DO:
          H-FREQ = "5XYR".
          TICKS = 1.
          F-TICK = yes.
      END.
      IF PRO-DESP.FREQ = "SIX_TIMES_PER_YEAR" AND PRO-DESP.mth[gxmonth] and
       pro-desp.wks[gweek]
        THEN DO:
          H-FREQ = "6XYR".
          TICKS = 1.
          F-TICK = yes.
      END.                                          
      IF PRO-DESP.FREQ = "SEVEN_TIMES_PER_YEAR" AND PRO-DESP.mth[gxmonth] and
       pro-desp.wks[gweek]
        THEN DO:
          H-FREQ = "7XYR".
          TICKS = 1.
          F-TICK = yes.
      END.
      IF PRO-DESP.FREQ = "EIGHT_TIMES_PER_YEAR" AND PRO-DESP.mth[gxmonth] and
       pro-desp.wks[gweek]
        THEN DO:
          H-FREQ = "8XYR".
          TICKS = 1.
          F-TICK = yes.
      END.
      IF PRO-DESP.FREQ = "NINE_TIMES_PER_YEAR" AND PRO-DESP.mth[gxmonth] and
       pro-desp.wks[gweek]
        THEN DO:
          H-FREQ = "9XYR".
          TICKS = 1.
          F-TICK = yes.
      END.
      IF PRO-DESP.FREQ = "TEN_TIMES_PER_YEAR" AND PRO-DESP.mth[gxmonth] and
       pro-desp.wks[gweek]
        THEN DO:
          H-FREQ = "10XYR".
          TICKS = 1.
          F-TICK = yes.
      END.
      IF PRO-DESP.FREQ = "ELEVEN_TIMES_PER_YEAR" AND PRO-DESP.mth[gxmonth] and
       pro-desp.wks[gweek]
        THEN DO:
          H-FREQ = "11XYR".
          TICKS = 1.
          F-TICK = yes.
      END.
      IF PRO-DESP.FREQ = "EVERY_TWO_YEARS" THEN
          IF (year(beg#) - YEAR(PRO-DESP.CL-DATE) = 2) AND PRO-DESP.mth[gxmonth] and
           pro-desp.wks[gweek]
            THEN DO:
              H-FREQ = "1X2YR".
              TICKS = 1.
              F-TICK = yes.
          END.
      IF PRO-DESP.FREQ = "EVERY_THREE_YEARS" THEN
          IF (year(beg#) - YEAR(PRO-DESP.CL-DATE) = 3) AND PRO-DESP.mth[gxmonth] and
           pro-desp.wks[gweek]
            THEN DO:
              H-FREQ = "1X3YR".
              TICKS = 1.
              F-TICK = yes.
          END.
      IF PRO-DESP.FREQ = "EVERY_FOUR_YEARS" THEN
          IF (year(beg#) - YEAR(PRO-DESP.CL-DATE) = 4) AND PRO-DESP.mth[gxmonth] and
           pro-desp.wks[gweek]
            THEN DO:
              H-FREQ = "1X4YR".
              TICKS = 1.
              F-TICK = yes.
          END.
      IF PRO-DESP.FREQ = "EVERY_FIVE_YEARS" THEN
          IF (year(beg#) - YEAR(PRO-DESP.CL-DATE) = 5) AND PRO-DESP.mth[gxmonth] and
           pro-desp.wks[gweek]
            THEN DO:
              H-FREQ = "1X5YR".
              TICKS = 1.
              F-TICK = yes.
          END.
      IF PRO-DESP.FREQ = "EVERY_EIGHTEEN_MONTHS" THEN
          IF PRO-DESP.CL-DATE + 549 >= BEG# AND
             PRO-DESP.CL-DATE + 549 <= BEG# + 6 and pro-desp.mth[gxmonth] and 
              pro-desp.wks[gweek]
               THEN DO:
                 TICKS = 1.
                 H-FREQ = "1X18M".
                 F-TICK = yes.
          END.
      IF PRO-DESP.FREQ = "SPECIAL_CLEANING" AND PRO-DESP.mth[gxmonth] and
       pro-desp.wks[gweek]
        THEN DO:
          H-FREQ = "SPCL".
          TICKS = 1.
          F-TICK = yes.
      END.
      IF PRO-DESP.FREQ = "ON_CALL" AND PRO-DESP.mth[gxmonth] and
       pro-desp.wks[gweek]
        THEN DO:
          H-FREQ = "OC".
          TICKS = 1.
          F-TICK = yes.
      END.
      IF PRO-DESP.FREQ = "INITIAL_CLEANING" AND PRO-DESP.mth[gxmonth] and
       pro-desp.wks[gweek]
        THEN DO:
          H-FREQ = "IC".
          TICKS = 1.
          F-TICK = yes.
      END.
      IF PRO-DESP.FREQ = "ONE_TIME_CLEANING" AND PRO-DESP.mth[gxmonth] and
       pro-desp.wks[gweek]
        THEN DO:
          H-FREQ = "1X".
          TICKS = 1.
          F-TICK = yes.
      END.                                          
      IF NOT TestMode THEN OUTPUT TO PRINTER PAGE-SIZE 0. 
      ELSE OUTPUT TO "c:\psg-work\addone.txt".
        
    IF F-TICK THEN DO:
      REPEAT DY = 1 TO TICKS:
      FIND FIRST TICKET WHERE TICKET.COMP# = ACCT-RCV.COMP# AND
                              TICKET.DIV# = ACCT-RCV.DIV# AND
                              TICKET.CUST# = ACCT-RCV.CUST# AND
                              TICKET.PROPSL# = PRO-DESP.PROPSL# AND
                              TICKET.ITEM# = PRO-DESP.ITEM# AND
                              TICKET.MONTH# = month(BEG#) AND
                              ticket.sub# = xsub and
                              ticket.route# = xroute and
                              ticket.wk# = xweek and
                              TICKET.T-INDX = DY NO-LOCK NO-ERROR.
     IF AVAILABLE TICKET THEN DO:
          NEXT.
      END.
      IF DY = F-INDEX THEN DO:
      
      IF NOT LaserPrinter THEN
        DISPLAY SPACE(74) H-FREQ SKIP(1) SPACE(44)
              STRING(PRO-DESP.COMP#, ">>") + "-" +
              TRIM(STRING(PRO-DESP.DIV#, ">>")) + "-" +
              TRIM(STRING(PRO-DESP.CUST#, ">>>>>>>>>>")) + "-" +
              TRIM(STRING(PRO-DESP.PROPSL#, ">>>>>>>>>>")) + "-" +
              TRIM(STRING(PRO-DESP.ITEM#, ">>>>")) + "-" +
              TRIM(STRING(gxmonth, ">>")) + "-" +
              trim(string(gweek, ">>")) + "-" +
              TRIM(STRING(DY, ">>"))
                FORMAT "X(35)" SKIP(1) SPACE(20)
              PROPSL.L-NAME SPACE(1)
              PROPSL.LADDR01 SKIP(0) SPACE(20)
              PROPSL.LADDR02 SPACE(1)
              PROPSL.LADDR03 SKIP(0)
              PRO-DESP.DESC01 SKIP(0)
              PRO-DESP.DESC02 SKIP(0)
              PRO-DESP.DESC03 SKIP(0)
              PRO-DESP.DESC04 SKIP(0)
              PRO-DESP.DESC05 SKIP(0)
              PRO-DESP.DESC06 SKIP(0)
              PRO-DESP.DESC07 SKIP(0)
              PRO-DESP.DESC08 SKIP(0)
              PRO-DESP.DESC09 SKIP(0)
              PRO-DESP.DESC10 SKIP(1)
              PRO-DESP.SPC-INTR SKIP(4) WITH FRAME P NO-BOX NO-LABELS.
 
       IF LaserPrinter THEN DO:

           CoProposal = 
                 STRING(PRO-DESP.COMP#, ">>") + "-" +
 	          TRIM(STRING(PRO-DESP.DIV#, ">>")) + "-" + 
	          TRIM(STRING(PRO-DESP.CUST#, ">>>>>>>>>>")) + "-" +
	          TRIM(STRING(PRO-DESP.PROPSL#, ">>>>>>>>>>")) + "-" +
	          TRIM(STRING(PRO-DESP.ITEM#, ">>>>")) + "-" +
	          TRIM(STRING(gxmonth, ">>")) + "-" +
	          trim(string(gweek, ">>")) + "-" +
	          TRIM(STRING(DY, ">>")).
	    REPEAT CurrentTicket = 1 TO NumTickets: /* how many of each to print */  
	       CREATE ttDocXPrint.
              ASSIGN 
                  ttDocXPrint.Idx              = ttDocXSequence
                  ttDocXPrint.Week             = h-Freq
                  ttDocXPrint.CoProposal       = CoProposal
                  ttDocXPrint.Location         = Propsl.L-Name + " " + 
                                                 Propsl.Laddr01 + " " +
                                                 Propsl.Laddr02 + " " +
                                                 propsl.laddr03
                  ttDocXPrint.Note1            = PRO-DESP.DESC01
                  ttDocXPrint.Note2            = PRO-DESP.DESC02
                  ttDocXPrint.Note3            = PRO-DESP.DESC03
                  ttDocXPrint.Note4            = PRO-DESP.DESC04
                  ttDocXPrint.Note5            = PRO-DESP.DESC05
                  ttDocXPrint.Note6            = PRO-DESP.DESC06
                  ttDocXPrint.Note7            = PRO-DESP.DESC07
                  ttDocXPrint.Note8            = PRO-DESP.DESC08
                  ttDocXPrint.Note9            = PRO-DESP.DESC09
                  ttDocXPrint.Note10           = PRO-DESP.DESC10
                  ttDocXPrint.SpcIntr          =  PRO-DESP.SPC-INTR
                  .
              ttDocXSequence = ttDocXSequence + 1.    
                          
          END. /* REPEAT CurrentTicket */
      END. /* IF Laser */ 
              
              CREATE TICKET.
              TICKET.COMP# = ACCT-RCV.COMP#.
              TICKET.DIV# = ACCT-RCV.DIV#.
              TICKET.CUST# = ACCT-RCV.CUST#.
              TICKET.PROPSL# = PRO-DESP.PROPSL#.
              TICKET.ITEM# = PRO-DESP.ITEM#.
              ticket.route# = pro-desp.route#.
              ticket.sub# = pro-desp.sub#.
              TICKET.MONTH# = gxmonth.
              ticket.wk# = gweek.
              ticket.ticketdate = beg#.
              TICKET.T-INDX = DY.
              TICKET.DATE-PRT = TODAY.
              TICKET.DL-BUD = PRO-DESP.DL.
              TICKET.DL-BAL = PRO-DESP.DL.
              TICKET.WK-DL-BAL = PRO-DESP.DL.
              TICKET.WK-DL-BUD = PRO-DESP.DL.
              TICKET.PRT = "Y".
              TICKET.FREQ = PRO-DESP.FREQ.
              TICKET.T-STAT = "P".
              IF PRO-DESP.WHCH-AMT = 1 THEN DO:
                  TICKET.TOT-AMT = PRO-DESP.AMT.
                  TICKET.WK-START = PRO-DESP.AMT.
                  TICKET.TOT-AMT-REM = PRO-DESP.AMT.
              END.
              IF PRO-DESP.WHCH-AMT = 2 THEN DO:
                  TICKET.TOT-AMT = AMT2 * AMT2-MUL.
                  TICKET.WK-START = AMT2 * AMT2-MUL.
                  TICKET.TOT-AMT-REM = AMT2 * AMT2-MUL.
              END.
              RELEASE TICKET.
              create cl-date.
                cl-date.cl-date = 01/01/9999.
                cl-date.comp# = acct-rcv.comp#.
                cl-date.cust# = acct-rcv.cust#.
                cl-date.date-ret = 01/01/9999.
                cl-date.div# = acct-rcv.div#.
                cl-date.freq = pro-desp.freq.
                cl-date.item# = pro-desp.item#.
                cl-date.month# = gxmonth.
                cl-date.propsl# = pro-desp.propsl#.
                cl-date.route# = pro-desp.route#.
                cl-date.sub# = pro-desp.sub#.
                cl-date.wk# = gweek.
                cl-date.t-indx = dy.
              release cl-date.
            END.
          END.
      END.
      
      IF LaserPrinter THEN DO: /* Print ticket data to Word if Laser */
     
    	  FOR EACH ttDocXPrint BY Idx:
    	       display idx ttDocXPrint.CoProposal ttDocXPrint.Location. pause.
    	        run docx_setClipboardValue("Ticket",string(TicketCount) + "Week", ttDocXPrint.Week).
               run docx_setClipboardValue("Ticket",string(TicketCount) + "ProposalNumber", ttDocXPrint.CoProposal).  
               run docx_setClipboardValue("Ticket",string(TicketCount) + "Location", ttDocXPrint.Location).
               run docx_setClipboardValue("Ticket",string(TicketCount) + "Note1", ttDocXPrint.Note1).
               run docx_setClipboardValue("Ticket",string(TicketCount) + "Note2", ttDocXPrint.Note2).
               run docx_setClipboardValue("Ticket",string(TicketCount) + "Note3", ttDocXPrint.Note3).
               run docx_setClipboardValue("Ticket",string(TicketCount) + "Note4", ttDocXPrint.Note4).
               run docx_setClipboardValue("Ticket",string(TicketCount) + "Note5", ttDocXPrint.Note5).
               run docx_setClipboardValue("Ticket",string(TicketCount) + "Note6", ttDocXPrint.Note6).
               run docx_setClipboardValue("Ticket",string(TicketCount) + "Note7", ttDocXPrint.Note7).
               run docx_setClipboardValue("Ticket",string(TicketCount) + "Note8", ttDocXPrint.Note8).
               run docx_setClipboardValue("Ticket",string(TicketCount) + "Note9", ttDocXPrint.Note9).
               run docx_setClipboardValue("Ticket",string(TicketCount) + "Note10", ttDocXPrint.Note10).
               run docx_setClipboardValue("Ticket",string(TicketCount) + "SpcIntr", ttDocXPrint.SpcIntr).
               TicketCount = TicketCount + 1.
               IF TicketCount GE TicketsPerPage THEN DO: /* print page and set for next page */
                  run docx_paste("Ticket"). /* Output page to Word.*/
                  TicketCount = 1.
               END.  
    	   END. /* FOR EACH ttDocXPrint */
    	  
          /*run docx_paste("Ticket"). /* Output final page to Word.*/ */
          FileName = os_getNextFile ( "c:\LaserTickets\SingleTicket" + 
                                      STRING(YEAR(TODAY))  + 
                                      STRING(MONTH(TODAY)) +
                                      STRING(DAY(TODAY)) +
                                      ".docx" ). 
          run docx_save(FileName).
          Cmd = "start winword.exe /t " + FileName.
          OS-command silent VALUE(Cmd).
  

      END.


