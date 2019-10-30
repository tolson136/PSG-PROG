/**************************************************/
/* wrep.p                                         */
/*                                                */
/* Weekly Ticket Reprint                          */
/*                                                */
/*  4/10/2017    TO   Added StartDate and EndDate */
/*  1/30/2018    TO   Added Laser Print           */
/*  4/17/2018    TO   Add test mode               */
/*                    Remove Start/End Date       */
/*                    Clean up                    */
/*                                                */
/**************************************************/ 

DEFINE SHARED VARIABLE XPROG    AS LOGICAL.
DEFINE SHARED VARIABLE XCOM     AS INTEGER FORMAT "ZZ".
DEFINE SHARED VARIABLE XDIV     AS INTEGER FORMAT "ZZ".
DEFINE SHARED VARIABLE XCOM-N   AS CHAR FORMAT "X(30)".
DEFINE SHARED VARIABLE XDIV-N   AS CHAR FORMAT "X(30)".
DEFINE SHARED VARIABLE XOPR     AS CHAR FORMAT "XXX".

DEFINE NEW SHARED VARIABLE    TestMode AS LOGICAL INIT Yes.
DEFINE NEW SHARED VARIABLE    BEG# AS INTEGER FORMAT "ZZ"
  LABEL "MONTH".
DEFINE NEW SHARED VARIABLE    F-CUST AS DECIMAL FORMAT "ZZZZZZZZZZ"
    LABEL "CUST #".
DEFINE NEW SHARED VARIABLE    F-PROP AS DECIMAL FORMAT "ZZZZZZZZZZ"
    LABEL "PROP #".
DEFINE NEW SHARED VARIABLE    F-ITEM AS INTEGER FORMAT "ZZZZ"
    LABEL "ITEM #".
DEFINE NEW SHARED VARIABLE    F-INDX AS INTEGER FORMAT "ZZ"
    LABEL "TICK INDEX".
DEFINE NEW SHARED VARIABLE    f-wk as int format "Z" label "WEEK #".
DEFINE NEW SHARED VARIABLE    F-NUM AS CHAR FORMAT "X(4)".
DEFINE NEW SHARED VARIABLE    F-MAN AS INTEGER FORMAT "ZZZ".
DEFINE NEW SHARED VARIABLE    F-DATE AS DATE FORMAT "99/99/9999".
DEFINE NEW SHARED VARIABLE    LaserPrinter AS LOG INIT No.
DEFINE NEW SHARED VARIABLE    NumTickets AS INT INIT 2. 

DEFINE VARIABLE dlog AS LOGICAL.

IF (USERID = "LANDMARK") OR (USERID = "GARCIA")
THEN DO:
    MESSAGE "YOU ARE NOT AUTHORIZED TO RUN THIS PROCEDURE".
    RETURN.
END.

REPEAT:
   DISPLAY SPACE(6)
   "W E E K L Y   R E P R I N T   T I C K E T   I N P U T   S C R E E N (wrep.p)"
    SKIP(0)
    XCOM-N LABEL "CO" XDIV-N LABEL "DV" XOPR LABEL "OPER"
     SKIP(1) WITH FRAME X NO-BOX SIDE-LABELS.
   PROMPT-FOR TICKET.CUST#
              TICKET.PROPSL#
              TICKET.ITEM#
              TICKET.MONTH#
              ticket.wk#
              TICKET.T-INDX SKIP (2)
              LaserPrinter LABEL "Send to laser?"
              WITH FRAME X NO-BOX SIDE-LABELS.
   ASSIGN F-CUST = INPUT TICKET.CUST#
          BEG# = INPUT TICKET.MONTH#
          F-PROP = INPUT TICKET.PROPSL#
          F-ITEM = INPUT TICKET.ITEM#
          F-INDX = INPUT TICKET.T-INDX
          f-wk   = input ticket.wk#
          LaserPrinter.
   IF LaserPrinter = ? THEN LaserPrinter = No.       
   IF LASTKEY = KEYCODE("F4") THEN LEAVE.
   IF LASTKEY = KEYCODE("ESC") THEN LEAVE.
   IF NOT XPROG THEN DO:
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
 
	 IF PROPSL.JANITOR THEN RUN TK-REPJ.P.
     IF NOT PROPSL.JANITOR THEN RUN WREPR.P.
   END. /* IF NOT xprog */
   ELSE DO:
      FIND FIRST METPRO WHERE METPRO.COMP# = XCOM AND
                          METPRO.DIV# = XDIV AND
                          METPRO.CUST# = F-CUST AND
                          METPRO.PROPSL# = F-PROP NO-ERROR.
      IF NOT AVAILABLE METPRO THEN DO:
         BELL.
          HIDE MESSAGE.
          MESSAGE COLOR BLINK "THIS CONTRACT NO LONGER EXISTS".
         BELL.
         UNDO, RETRY.
      END.
      IF METPRO.JANITOR THEN RUN TK-REPJP.P.
      IF NOT METPRO.JANITOR THEN RUN TK-REPRP.P.
   END. /* ELSE DO */
END. /* REPEAT */
