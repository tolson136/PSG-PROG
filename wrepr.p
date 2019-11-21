/**************************************************/
/* wrepr.p                                        */
/*                                                */
/* Weekly Ticket Reprint                          */
/*                                                */
/*  4/10/2017    TO   Added StartDate and EndDate */
/*  1/30/2018    TO   Added Laser Print           */
/*  4/17/2018    TO   Add test mode               */
/*                    Remove Start/End Date       */
/*                    Clean up                    */
/*  10/22/2019   TO   Node print for Word/Laser   */
/*                                                */
/**************************************************/

DEFINE SHARED VARIABLE TestMode AS LOGICAL.
DEFINE SHARED VARIABLE XCOM AS INTEGER FORMAT "ZZ".
DEFINE SHARED VARIABLE XDIV AS INTEGER FORMAT "ZZ".
DEFINE SHARED VARIABLE XCOM-N AS CHAR FORMAT "X(30)".
DEFINE SHARED VARIABLE XDIV-N AS CHAR FORMAT "X(30)".
DEFINE SHARED VARIABLE XOPR AS CHAR FORMAT "XXX".
DEFINE VARIABLE H-FREQ AS CHAR FORMAT "X(5)".
DEFINE SHARED VARIABLE BEG# AS INTEGER FORMAT "ZZ"
  LABEL "MONTH".
DEFINE SHARED VARIABLE F-CUST AS DECIMAL FORMAT "ZZZZZZZZZZ"
    LABEL "CUST #".
DEFINE SHARED VARIABLE F-PROP AS DECIMAL FORMAT "ZZZZZZZZZZ"
    LABEL "PROP #".
DEFINE SHARED VARIABLE F-ITEM AS INTEGER FORMAT "ZZZZ"
    LABEL "ITEM #".
DEFINE SHARED VARIABLE F-INDX AS INTEGER FORMAT "ZZ"
    LABEL "TICK INDEX".
DEFINE SHARED VARIABLE F-NUM AS CHAR FORMAT "X(4)".
DEFINE SHARED VARIABLE F-MAN AS INTEGER FORMAT "ZZZ".
DEFINE SHARED VARIABLE f-wk  AS INTEGER FORMAT "Z".
DEFINE SHARED VARIABLE F-DATE AS DATE FORMAT "99/99/9999".
DEFINE SHARED VARIABLE LaserPrinter AS LOG.
DEFINE SHARED VARIABLE NumTickets AS INT.

DEFINE VARIABLE JobWeek as CHAR FORMAT "X(8)".
DEFINE VARIABLE JobDay AS CHAR FORMAT "X(18)".
DEFINE VARIABLE TicketsPerPage AS INT INIT 3.
DEFINE VARIABLE CurrentTicket  AS INT INIT 1. /* count within expect tickets per*/
DEFINE VARIABLE TicketCount    AS INT INIT 1. /* Count on page */
DEFINE VARIABLE CoProposal AS CHAR FORMAT "X(30)".
DEFINE VARIABLE ttDocSequence AS INT.
DEFINE VARIABLE FileName AS CHAR.
DEFINE VARIABLE Cmd AS CHAR.

DEF TEMP-TABLE ttDocPrint
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
  
/* Various OS uilities */  
{slib/slibos.i}

OUTPUT TO TERMINAL.
FIND FIRST TICKET WHERE TICKET.COMP# = XCOM AND
                        TICKET.DIV# = XDIV AND
                        TICKET.CUST# = F-CUST AND
                        TICKET.PROPSL# = F-PROP AND
                        TICKET.ITEM# = F-ITEM AND
                        TICKET.MONTH# = BEG# AND
                        ticket.wk# = f-wk and
                        TICKET.T-INDX = F-INDX NO-ERROR.
IF NOT AVAILABLE TICKET THEN DO:
    BELL.
    MESSAGE COLOR BLINK
        "THIS TICKET DOES NOT EXIST".
    BELL.
    UNDO, RETRY.
END.

FIND FIRST PRO-DESP WHERE PRO-DESP.COMP# = XCOM AND
                          PRO-DESP.DIV# = XDIV AND
                          PRO-DESP.CUST# = F-CUST AND
                          PRO-DESP.PROPSL# = F-PROP AND
                          PRO-DESP.ITEM# = F-ITEM NO-ERROR.
IF NOT AVAILABLE PRO-DESP THEN DO:
    BELL.
    HIDE MESSAGE.
    MESSAGE COLOR BLINK "THIS PROPOSAL DESCRIPTION NO LONGER EXISTS".
    BELL.
    UNDO, RETRY.
END.
FIND FIRST PROPSL WHERE PROPSL.COMP# = XCOM AND
                        PROPSL.DIV# = XDIV AND
                        PROPSL.CUST# = F-CUST AND
                        PROPSL.PROPSL# = F-PROP NO-ERROR.
IF NOT AVAILABLE PROPSL THEN DO:
    BELL.
    HIDE MESSAGE.
    MESSAGE COLOR BLINK "THIS PROPOSAL NO LONGER EXISTS".
    BELL.
    UNDO, RETRY.
END.
IF PROPSL.JANITOR = yes THEN DO:
    BELL.
    HIDE MESSAGE.
    MESSAGE COLOR BLINK "THIS PROPOSAL IS A JANITORIAL TICKET".
    BELL.
    UNDO, RETRY.
END.
         
      IF PRO-DESP.FREQ = "DAILY" THEN H-FREQ = "D".
      IF PRO-DESP.FREQ = "TWO_TIMES_PER_WEEK" THEN H-FREQ = "2XW".
      IF PRO-DESP.FREQ = "THREE_TIMES_PER_WEEK" THEN H-FREQ = "3XW".
      IF PRO-DESP.FREQ = "FOUR_TIMES_PER_WEEK" THEN H-FREQ = "4XW".
      IF PRO-DESP.FREQ = "FIVE_TIMES_PER_WEEK" THEN H-FREQ = "5XW".
      IF PRO-DESP.FREQ = "SIX_TIMES_PER_WEEK" THEN H-FREQ = "6XW".
      IF PRO-DESP.FREQ = "WEEKLY" THEN H-FREQ = "W".
      IF PRO-DESP.FREQ = "EVERY_TWO_WEEKS" THEN H-FREQ = "2W".
      IF PRO-DESP.FREQ = "EVERY_THREE_WEEKS" THEN H-FREQ = "3W".
      IF PRO-DESP.FREQ = "EVERY_FOUR_WEEKS" THEN H-FREQ = "4W".
      IF PRO-DESP.FREQ = "EVERY_FIVE_WEEKS" THEN H-FREQ = "5W".
      IF PRO-DESP.FREQ = "EVERY_SIX_WEEKS" THEN H-FREQ = "6W".
      IF PRO-DESP.FREQ = "EVERY_EIGHT_WEEKS" THEN H-FREQ = "8W".
      IF PRO-DESP.FREQ = "MONTHLY" THEN H-FREQ = "M".
      IF PRO-DESP.FREQ = "QUARTERLY" THEN H-FREQ = "Q".
      IF PRO-DESP.FREQ = "TWO_TIMES_PER_MONTH" THEN H-FREQ = "2XM".
      IF PRO-DESP.FREQ = "ANNUAL" THEN H-FREQ = "A".
      IF PRO-DESP.FREQ = "TWO_TIMES_PER_YEAR" THEN H-FREQ = "2XYR".
      IF PRO-DESP.FREQ = "THREE_TIMES_PER_YEAR" THEN H-FREQ = "3XYR".
      IF PRO-DESP.FREQ = "FOUR_TIMES_PER_YEAR" THEN H-FREQ = "4XYR".
      IF PRO-DESP.FREQ = "FIVE_TIMES_PER_YEAR" THEN H-FREQ = "5XYR".
      IF PRO-DESP.FREQ = "SIX_TIMES_PER_YEAR" THEN H-FREQ = "6XYR".
      IF PRO-DESP.FREQ = "SEVEN_TIMES_PER_YEAR" THEN H-FREQ = "7XYR".
      IF PRO-DESP.FREQ = "EIGHT_TIMES_PER_YEAR" THEN H-FREQ = "8XYR".
      IF PRO-DESP.FREQ = "NINE_TIMES_PER_YEAR" THEN H-FREQ = "9XYR".
      IF PRO-DESP.FREQ = "TEN_TIMES_PER_YEAR" THEN H-FREQ = "10XYR".
      IF PRO-DESP.FREQ = "ELEVEN_TIMES_PER_YEAR" THEN H-FREQ = "11XYR".
      IF PRO-DESP.FREQ = "EVERY_TWO_YEARS" THEN H-FREQ = "1X2YR".
      IF PRO-DESP.FREQ = "EVERY_THREE_YEARS" THEN H-FREQ = "1X3YR".
      IF PRO-DESP.FREQ = "EVERY_FOUR_YEARS" THEN H-FREQ = "1X4YR".
      IF PRO-DESP.FREQ = "EVERY_FIVE_YEARS" THEN H-FREQ = "1X5YR".
      IF PRO-DESP.FREQ = "EVERY_EIGHTEEN_MONTHS" THEN H-FREQ = "1X18M".
      IF PRO-DESP.FREQ = "SPECIAL_CLEANING" THEN H-FREQ = "SPCL".
      IF PRO-DESP.FREQ = "ON_CALL" THEN H-FREQ = "OC".
      IF PRO-DESP.FREQ = "INITIAL_CLEANING" THEN H-FREQ = "IC".
      IF PRO-DESP.FREQ = "ONE_TIME_CLEANING" THEN H-FREQ = "1X".
      
      JobWeek = "".
      IF PRO-DESP.Wks[1] THEN JobWeek = JobWeek + "1 ". 
      IF PRO-DESP.Wks[2] THEN JobWeek = JobWeek + "2 ". 
      IF PRO-DESP.Wks[3] THEN JobWeek = JobWeek + "3 ". 
      IF PRO-DESP.Wks[4] THEN JobWeek = JobWeek + "4 ". 
      IF PRO-DESP.Wks[5] THEN JobWeek = JobWeek + "5 ".
      
      JobDay = "".
      IF WKDAY[1] THEN JObDay = JobDay + "S".
      IF WKDAY[2] THEN JObDay = JobDay + "S".
      IF WKDAY[3] THEN JObDay = JobDay + "M".
      IF WKDAY[4] THEN JObDay = JobDay + "T".
      IF WKDAY[5] THEN JObDay = JobDay + "W".
      IF WKDAY[6] THEN JObDay = JobDay + "R".
      IF WKDAY[7] THEN JObDay = JobDay + "F".
                
      IF NOT LaserPrinter AND NOT TestMode THEN OUTPUT TO PRINTER PAGE-SIZE 0.
      IF NOT LaserPrinter AND TestMode THEN OUTPUT TO c:\psg-work\ticket.txt.
      IF NOT LaserPrinter THEN
       DISPLAY SPACE(74) H-FREQ + TRIM(JobDay) + STRING(PRO-DESP.ROUTE#) SKIP(1) SPACE(44)
              STRING(TICKET.COMP#, ">>") + "-" +
              TRIM(STRING(TICKET.DIV#, ">>")) + "-" +
              TRIM(STRING(TICKET.CUST#, ">>>>>>>>>>")) + "-" +
              TRIM(STRING(TICKET.PROPSL#, ">>>>>>>>>>")) + "-" +
              TRIM(STRING(TICKET.ITEM#, ">>>>")) + "-" +
              TRIM(STRING(TICKET.MONTH#, ">>")) + "-" +
              trim(string(ticket.wk#, ">")) + "-" +
              TRIM(STRING(TICKET.T-INDX, ">>"))
                FORMAT "X(35)" SKIP(1) 
              SPACE(5) 
              PROPSL.L-NAME + " " +
              PROPSL.LADDR01 + " " +
              PROPSL.LADDR02 + " " +
              PROPSL.LADDR03  + " " + STATE FORMAT "X(80)" SKIP(0)
              PRO-DESP.DESC01 SKIP(0)
              PRO-DESP.DESC02 SKIP(0)
              PRO-DESP.DESC03 SKIP(0)              
              PRO-DESP.DESC04 SKIP(0)                             
              PRO-DESP.DESC05 SKIP(0)
              PRO-DESP.DESC06 SKIP(0)
              PRO-DESP.DESC07 SKIP(0)
              PRO-DESP.DESC08 SKIP(0)
              PRO-DESP.DESC09 SKIP(0)
              PRO-DESP.DESC10 SKIP(0)             
              PRO-DESP.SPC-INTR SKIP(6) 
                WITH FRAME P width 100 NO-BOX NO-LABELS
              .
          
   IF LaserPrinter THEN DO:

           CoProposal = 
                   STRING(PRO-DESP.COMP#, ">>") + "-" +
              TRIM(STRING(PRO-DESP.DIV#, ">>")) + "-" + 
              TRIM(STRING(PRO-DESP.CUST#, ">>>>>>>>>>")) + "-" +
              TRIM(STRING(PRO-DESP.PROPSL#, ">>>>>>>>>>")) + "-" +
              TRIM(STRING(PRO-DESP.ITEM#, ">>>>")) + "-" +
              TRIM(STRING(ticket.month#, ">>")) + "-" +
              trim(string(ticket.wk#, ">>")) + "-" +
              TRIM(STRING(ticket.t-indx, ">>")).

                
           CREATE ttDocPrint.
              ASSIGN 
                  ttDocPrint.Idx              = ttDocSequence
                  ttDocPrint.Week             = h-Freq + string(pro-desp.route#)
                  ttDocPrint.CoProposal       = CoProposal
                  ttDocPrint.Location         = Propsl.L-Name + " " + 
                                                 Propsl.Laddr01 + " " +
                                                 Propsl.Laddr02 + " " +
                                                 propsl.laddr03
                  ttDocPrint.Note1            = PRO-DESP.DESC01
                  ttDocPrint.Note2            = PRO-DESP.DESC02
                  ttDocPrint.Note3            = PRO-DESP.DESC03
                  ttDocPrint.Note4            = PRO-DESP.DESC04
                  ttDocPrint.Note5            = PRO-DESP.DESC05
                  ttDocPrint.Note6            = PRO-DESP.DESC06
                  ttDocPrint.Note7            = PRO-DESP.DESC07
                  ttDocPrint.Note8            = PRO-DESP.DESC08
                  ttDocPrint.Note9            = PRO-DESP.DESC09
                  ttDocPrint.Note10           = PRO-DESP.DESC10
                  ttDocPrint.SpcIntr          =  PRO-DESP.SPC-INTR
                  .
              ttDocSequence = ttDocSequence + 1.                  
 
 END. /* IF Laser */   
      IF LaserPrinter THEN DO: /* Print ticket data to Word if Laser */
          /* Output ticket data to CSV file for processing by NODE */
          OUTPUT TO c:\LaserTickets\TicketData.csv.
        
          FOR EACH ttDocPrint BY Idx:
               PUT UNFORMATTED '"' + ttDocPrint.Week       + '"' + ",".
               PUT UNFORMATTED '"' + ttDocPrint.CoProposal + '"' + ",".
               PUT UNFORMATTED '"' + ttDocPrint.Location   + '"' + ",".
               PUT UNFORMATTED '"' + ttDocPrint.Note1      + '"' + ",".
               PUT UNFORMATTED '"' + ttDocPrint.Note2      + '"' + ",".
               PUT UNFORMATTED '"' + ttDocPrint.Note3      + '"' + ",".
               PUT UNFORMATTED '"' + ttDocPrint.Note4      + '"' + ",".
               PUT UNFORMATTED '"' + ttDocPrint.Note5      + '"' + ",".
               PUT UNFORMATTED '"' + ttDocPrint.Note6      + '"' + ",".
               PUT UNFORMATTED '"' + ttDocPrint.Note7      + '"' + ",".                               
               PUT UNFORMATTED '"' + ttDocPrint.Note8      + '"' + ",".
               PUT UNFORMATTED '"' + ttDocPrint.Note9      + '"' + ",".
               PUT UNFORMATTED '"' + ttDocPrint.Note10     + '"' + ",".
               PUT UNFORMATTED '"' + ttDocPrint.SpcIntr    + '"' + ",". /*SKIP*/.
               /*IF Idx LT ttDocSequence THEN PUT UNFORMATTED SKIP.*/
.          
           END. /* FOR EACH ttDocPrint */
           OUTPUT CLOSE.
          FileName = os_getNextFile ( "c:/LaserTickets/ReprintTicket" + 
                                      STRING(YEAR(TODAY))  + 
                                      STRING(MONTH(TODAY)) +
                                      STRING(DAY(TODAY)) +
                                      ".docx" ). 
           /* run node procdess process arg1=csv file arg2=template arg3=output doc*/
                     
           Cmd = "node c:/nodeproj/DocXTemplater\ticketsnode " +
                 "c:/lasertickets/TicketData.csv " +
                 "m:/template/tickets2upnode.docx " + 
                 /*"c:/lasertickets/test.docx"*/ 
                 FILENAME +
                 " 1> c:\lasertickets\ticketnodereprint.err 2>&1". 
           OS-COMMAND SILENT VALUE(Cmd).      
         
           /*OS-COMMAND /*SILENT*/ "node c:/nodeproj/docxtemplater/ticketsnode c:/lasertickets/TicketData.csv c:/psg-prog/template/tickets2upnode.docx " + "c:/lasertickets/test.docx" /*FILENAME*/.*/
           /*OS-COMMAND SILENT "node c:\nodeproj\docxtemplater\\ticketsnode c:/psg-node/psg-node/TicketData.csv c:/psg-prog/template/tickets2upnode.docx c:/lasertickets/test.docx > c:/lasertickets/ticketsnode.err".*/                           
                                      
          Cmd = "start winword.exe /t " + FileName.
          OS-command silent VALUE(Cmd).
          
      END.                 
             
