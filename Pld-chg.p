/***********************************************/
/*  pld-chg.p                                  */
/*                                             */
/*  Proposal Description Maintenance           */
/*                                             */
/*   12/22/2017    TO   Changed Search Method  */
/*   02/15/2018    TO   Changed Address labels */
/*   04/20/2018    TO   BigSearch added        */
/***********************************************/

DEFINE SHARED VARIABLE XCOM AS INTEGER FORMAT "ZZ".
DEFINE SHARED VARIABLE XDIV AS INTEGER FORMAT "ZZ".
DEFINE VARIABLE XPROP AS DECIMAL FORMAT "ZZZZZZZZZZ".
DEFINE VARIABLE XITEM AS INTEGER FORMAT "ZZZZ".
DEFINE VARIABLE XCUST LIKE PRO-DESP.CUST#.
DEFINE VARIABLE POSER AS LOG.
DEFINE VARIABLE NOTER AS log.
DEFINE VARIABLE ADDNOTE AS LOG
    LABEL "No Notes exist. Would you like to add them ?".
DEFINE VARIABLE SEARCHER AS LOG.
DEFINE VARIABLE SEARCHED AS CHAR FORMAT "X".
DEFINE SHARED VARIABLE XCOM-N AS CHAR FORMAT "X(30)".
DEFINE SHARED VARIABLE XDIV-N AS CHAR FORMAT "X(30)".
DEFINE SHARED VARIABLE XOPR AS CHAR FORMAT "XXX".
/*DEFINE VARIABLE NewAmt LIKE pro-desp.amt.*/
   def var ctr as int.
   ctr = 0.
   def var counter as int.
   def var oldchoice as char.
   def var oddchoice as char.  
   def var cl-d as int.
   def var cl-m as int.
   def var cl-y as int format "9999".
DEFINE VARIABLE SearchPropsl# LIKE propsl.propsl#.   

REPEAT:
DISPLAY SPACE(1)
"P R O P O S A L   D E S C R I P T I O N   M A I N T E N A N C E   S C R E E N"
SKIP(0) XCOM-N LABEL "CMP" SPACE(1)
        XDIV-N LABEL "DIV" SPACE(1)
        XOPR LABEL "OPR" SKIP(0) WITH FRAME X NO-HIDE.
UPDATE SEARCHED AUTO-RETURN WITH FRAME X NO-HIDE NO-LABEL.



IF SEARCHED <> "" THEN DO:
  IF Searched = "1" THEN DO:
      RUN util\custsearch.w 
        ( INPUT xcom,
          INPUT xdiv,
          OUTPUT SearchPropsl#).
      
      FIND FIRST Propsl WHERE propsl.propsl# = SearchPropsl# NO-ERROR.
      FIND FIRST pro-desp WHERE Pro-Desp.Propsl# = SearchPropsl# NO-ERROR.
  END.
 
   ELSE DO: /* Searched not 1 */

   def var xnull as character initial "".
   def var cust-in as char format 'x(25)'
      label "Enter the first few letters of the Customer name".


   
   form propsl.propsl# propsl.l-name propsl.laddr01 skip propsl.c-name
     with frame cust-frame scroll 1 10 down no-labels.
   update cust-in with frame sc overlay side-labels.
   find first propsl use-index indx6 where propsl.c-name begins cust-in and
                                        propsl.comp# = xcom and
                                        propsl.div# = xdiv
                                        no-error.
   if not available propsl then leave.
   ctr = ctr + 1.
   repeat counter = 1 to 10:
       pause 0.
       display propsl.propsl# propsl.c-name propsl.l-name propsl.laddr01
         with frame cust-frame with side-labels.
       down with frame cust-frame.
       find next propsl use-index indx6
        where propsl.c-name begins cust-in and
              propsl.comp# = xcom and
              propsl.div# = xdiv no-error.
       if not available propsl then leave.
       ctr = ctr + 1.
   end. /* REPEAT */
   pause 0.
   up 10 with frame cust-frame.
   oldchoice = "".
   repeat:
      status default "use up and down arrows".
      choose row propsl.propsl# no-error go-on(cursor-right) with frame cust-frame.
      color display normal propsl.propsl# with frame cust-frame.
      if frame-value = "" then next.
      if frame-value <> oldchoice then do:
         oldchoice = frame-value.
         find first propsl use-index indx2 where propsl.propsl# = integer(frame-value).
      end.
      if lastkey = keycode("cursor-down")
       then do:
          find next propsl use-index indx6
              where propsl.c-name begins cust-in and
                    propsl.comp# = xcom and
                    propsl.div# = xdiv no-error.
          if not available propsl then
          find first propsl use-index indx6 where propsl.c-name begins cust-in and
                                                  propsl.comp# = xcom and
                                                  propsl.div# = xdiv.
          down with frame cust-frame.
          pause 0.
          display propsl.propsl# propsl.c-name propsl.l-name propsl.laddr01
            with frame cust-frame.
          next.
      end. /* down */
      if lastkey = keycode("cursor-up")
       then do:
          find prev propsl use-index indx6
              where propsl.c-name begins cust-in and
                    propsl.comp# = xcom and
                    propsl.div# = xdiv no-error.
          if not available propsl then
          find last propsl use-index indx6 where propsl.c-name begins cust-in and
                                                 propsl.comp# = xcom and
                                                 propsl.div# = xdiv.
          up with frame cust-frame.
          pause 0.
          display propsl.propsl# propsl.c-name propsl.l-name propsl.laddr01
              with frame cust-frame.
          next.
      end. /* UP */
      if lastkey = keycode("enter") then do:
          poser = yes.
          find first pro-desp where pro-desp.propsl# = propsl.propsl# and
                                    pro-desp.comp# = propsl.comp# and
                                    pro-desp.div# = propsl.div# and
                                    pro-desp.cust# = propsl.cust# no-error.
          clear frame cust-frame all no-pause.
          leave.
      end. /* ENTER */
   end. /* REPEAT */
  
   END. /* NOT M */  
end. /* SEARCHED */
/*************************END*******************************************/

ELSE DO:

    PROMPT-FOR PRO-DESP.PROPSL# WITH FRAME X NO-HIDE.
    XPROP = INPUT PRO-DESP.PROPSL#.
    PROMPT-FOR PRO-DESP.ITEM# WITH FRAME X NO-HIDE.
    XITEM = INPUT PRO-DESP.ITEM#.
    FIND FIRST PRO-DESP WHERE PRO-DESP.PROPSL# = XPROP AND
                              PRO-DESP.COMP# = XCOM AND
                              PRO-DESP.DIV# = XDIV AND
                              PRO-DESP.ITEM# = XITEM.
    ASSIGN XCUST = PRO-DESP.CUST#.
    find propsl where propsl.propsl# = xprop and propsl.comp# = xcom and
         propsl.div# = xdiv and propsl.cust# = xcust no-error.
    IF AVAILABLE PROPSL THEN DO:
        POSER = yes.
    END.
    ELSE DO:
        POSER = no.
    END.
    IF LASTKEY = KEYCODE("F4") OR LASTKEY = KEYCODE("ESC") THEN LEAVE.

END.


    FIND NOTES WHERE NOTES.COMP# = XCOM AND
                     NOTES.DIV# = XDIV AND
                     NOTES.CUST# = XCUST AND
                     NOTES.PROPSL# = XPROP AND
                     NOTES.ITEM# = XITEM NO-ERROR.
    IF AVAILABLE NOTES THEN DO:
        NOTER = yes.
    END.
    ELSE DO:
        NOTER = no.
    END.
if noter then do:
   message "".
   message color BLINK-Y
"                           ___N O T E S   E X I S T___                          ".
end.

    FORM
               PRO-DESP.PROPSL# LABEL "PROP #"
               PRO-DESP.ITEM# LABEL "ITEM #"
               PRO-DESP.COMP# LABEL "COMPANY"
               PRO-DESP.DIV# LABEL "DIVISION"
               START-DT SKIP(0) SPACE(2)
               PRO-DESP.CUST# LABEL "CUST #"
               FREQ
               AMT FORMAT "$>>,>>>.99"
               cod-amt FORMAT   "$>>,>>>.99" LABEL "COD"
               DESC01
               DESC02
               DESC03
               DESC04
               DESC05
               DESC06
               DESC07
               DESC08
               DESC09
               DESC10
               SPC-INTR SKIP(0)
               /*
               DL LABEL "D/L" SPACE(5)
               */
               "Time-HH:MM AM" SPACE(13)
               WKS[1] LABEL "FIRST WEEK "
               WKDAY[1] LABEL "SAT"
               MTH[1] LABEL "JANUARY "
               MTH[7] LABEL "JULY     " SKIP(0)
               StartTime LABEL "Start Time" SPACE(6)
               /*WHCH-AMT LABEL "USE AMT 1/2?"
               AC# LABEL "AC#" SPACE(2)
               */
               WKS[2] LABEL "SECOND WEEK"
               WKDAY[2] LABEL "SUN"
               MTH[2] LABEL "FEBRUARY"
               MTH[8] LABEL "AUGUST   " SKIP(0)
               /* LABEL "ALT DESP"*/
               SPACE(2) EndTime LABEL "End Time" space(6)
               WKS[3] LABEL "THIRD WEEK "
               WKDAY[3] LABEL "MON"
               MTH[3] LABEL "MARCH   "
               MTH[9] LABEL "SEPTEMBER" SKIP(0)
               /*AMT2-MUL LABEL "ALT AMT MULTIPLIER"*/
               space(26)
               WKS[4] LABEL "FOURTH WEEK"
               WKDAY[4] LABEL "TUE"
               MTH[4] LABEL "APRIL   "
               MTH[10] LABEL "OCTOBER  " SKIP(0)
               /*AMT2 LABEL "ALT AMT"*/
               space(26)
               WKS[5] LABEL "FIFTH WEEK "
               WKDAY[5] LABEL "WED"
               MTH[5] LABEL "MAY     "
               MTH[11] LABEL "NOVEMBER " SKIP(0)
               CL-DATE LABEL "LST CLEAN DTE"
               PRO-DESP.SUB# LABEL "SUB#"
               PRO-DESP.ROUTE# LABEL "RT#"
               WKDAY[6] LABEL "THU"
               MTH[6] LABEL "JUNE    "
               MTH[12] LABEL "DECEMBER " SKIP(0)
               /*COMMIS LABEL "COMM$" SPACE(3)*/
               space(22)
               SUB-CON LABEL "SUB$" SPACE(3)
               WKDAY[7] LABEL "FRI"
               PRO-DESP.PO# LABEL "P" SKIP
               /*BudgetedHours LABEL "Budget Hrs"*/
               EquipmentRequired label "Equip Reqd"
               WITH FRAME X WIDTH 100 SIDE-LABELS NO-BOX no-hide.

    DISPLAY
               PRO-DESP.PROPSL# PRO-DESP.ITEM# PRO-DESP.COMP# PRO-DESP.DIV#
               START-DT LABEL "START DT" PRO-DESP.CUST# FREQ LABEL "FREQ" AMT cod-amt
               DESC01 DESC02 DESC03 DESC04 DESC05 DESC06 DESC07 DESC08
               DESC09 DESC10 SPC-INTR /*DL*/ wks[1] WKDAY[1] MTH[1] MTH[7] StartTime /*WHCH-AMT*/
               wks[2] WKDAY[2] MTH[2] MTH[8] /*A2-DESP*/ EndTime wks[3] WKDAY[3] MTH[3] MTH[9]
               /*AMT2-MUL*/ wks[4] WKDAY[4] MTH[4] MTH[10] /*AMT2*/ wks[5] PRO-DESP.SUB#
               WKDAY[5] MTH[5] MTH[11] CL-DATE PRO-DESP.ROUTE#
               WKDAY[6] MTH[6] MTH[12] /*COMMIS*/ SUB-CON WKDAY[7] PRO-DESP.PO#
               /*BudgetedHours*/ EquipmentRequired
               WITH FRAME X WIDTH 100 SIDE-LABELS.

IF (USERID <> "OPERATIONS") AND (USERID <> "LANDMARK") AND (USERID <> "GARCIA")
THEN DO:
if noter then do:
   message "".
   message color BLINK-Y
"                           ___N O T E S   E X I S T___                          ".
end.
  REPEAT:

       UPDATE pro-desp.FREQ WITH FRAME X WIDTH 100 COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
       IF FREQ BEGINS "D" OR FREQ = "DAILY"
       THEN DO:
         pro-desp.FREQ = "DAILY".
         DISPLAY pro-desp.FREQ WITH FRAME X WIDTH 100 COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF pro-desp.FREQ BEGINS "2XW" OR FREQ = "TWO_TIMES_PER_WEEK"
       THEN DO:
         pro-desp.FREQ = "TWO_TIMES_PER_WEEK".
         DISPLAY pro-desp.FREQ WITH FRAME X WIDTH 100 COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "3XW" OR FREQ = "THREE_TIMES_PER_WEEK"
       THEN DO:
         FREQ = "THREE_TIMES_PER_WEEK".
         DISPLAY pro-desp.FREQ WITH FRAME X WIDTH 100 COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "4XW" OR FREQ = "FOUR_TIMES_PER_WEEK"
       THEN DO:
         FREQ = "FOUR_TIMES_PER_WEEK".
         DISPLAY pro-desp.FREQ WITH FRAME X WIDTH 100 COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "5XW" OR FREQ = "FIVE_TIMES_PER_WEEK"
       THEN DO:
         FREQ = "FIVE_TIMES_PER_WEEK".
         DISPLAY pro-desp.FREQ WITH FRAME X WIDTH 100 COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "6XW" OR FREQ = "SIX_TIMES_PER_WEEK"
       THEN DO:
         FREQ = "SIX_TIMES_PER_WEEK".
         DISPLAY pro-desp.FREQ WITH FRAME X WIDTH 100 COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "W" OR FREQ = "WEEKLY"
       THEN DO:
         FREQ = "WEEKLY".
         DISPLAY pro-desp.FREQ WITH FRAME X WIDTH 100 COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "2W" OR FREQ = "EVERY_TWO_WEEKS"
       THEN DO:
         FREQ = "EVERY_TWO_WEEKS".
         DISPLAY pro-desp.FREQ WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "3W" OR FREQ = "EVERY_THREE_WEEKS"
       THEN DO:
         FREQ = "EVERY_THREE_WEEKS".
         DISPLAY pro-desp.FREQ WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "4W" OR FREQ = "EVERY_FOUR_WEEKS"
       THEN DO:
         FREQ = "EVERY_FOUR_WEEKS".
         DISPLAY pro-desp.FREQ WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "5W" OR FREQ = "EVERY_FIVE_WEEKS"
       THEN DO:
         FREQ = "EVERY_FIVE_WEEKS".
         DISPLAY pro-desp.FREQ WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "6W" OR FREQ = "EVERY_SIX_WEEKS"
       THEN DO:
         FREQ = "EVERY_SIX_WEEKS".
         DISPLAY pro-desp.FREQ WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "8W" OR FREQ = "EVERY_EIGHT_WEEKS"
       THEN DO:
         FREQ = "EVERY_EIGHT_WEEKS".
         DISPLAY pro-desp.FREQ WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "M" OR FREQ = "MONTHLY"
       THEN DO:
         FREQ = "MONTHLY".
         DISPLAY pro-desp.FREQ WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "Q" OR FREQ = "QUARTERLY"
       THEN DO:
         FREQ = "QUARTERLY".
         DISPLAY pro-desp.FREQ WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "2XM" OR FREQ = "TWO_TIMES_PER_MONTH"
       THEN DO:
         FREQ = "TWO_TIMES_PER_MONTH".
         DISPLAY pro-desp.FREQ WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "A" OR FREQ = "ANNUAL"
       THEN DO:
         FREQ = "ANNUAL".
         DISPLAY pro-desp.FREQ WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "2XYR" OR FREQ = "TWO_TIMES_PER_YEAR"
       THEN DO:
         FREQ = "TWO_TIMES_PER_YEAR".
         DISPLAY pro-desp.FREQ WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "3XYR" OR FREQ = "THREE_TIMES_PER_YEAR"
       THEN DO:
         FREQ = "THREE_TIMES_PER_YEAR".
         DISPLAY pro-desp.FREQ WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "4XYR" OR FREQ = "FOUR_TIMES_PER_YEAR"
       THEN DO:
         FREQ = "FOUR_TIMES_PER_YEAR".
         DISPLAY pro-desp.FREQ WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "5XYR" OR FREQ = "FIVE_TIMES_PER_YEAR"
       THEN DO:
         FREQ = "FIVE_TIMES_PER_YEAR".
         DISPLAY pro-desp.FREQ WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "6XYR" OR FREQ = "SIX_TIMES_PER_YEAR"
       THEN DO:
         FREQ = "SIX_TIMES_PER_YEAR".
         DISPLAY pro-desp.FREQ WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "7XYR" OR FREQ = "SEVEN_TIMES_PER_YEAR"
       THEN DO:
         FREQ = "SEVEN_TIMES_PER_YEAR".
         DISPLAY pro-desp.FREQ WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE

       IF FREQ BEGINS "8XYR" OR FREQ = "EIGHT_TIMES_PER_YEAR"
       THEN DO:
         FREQ = "EIGHT_TIMES_PER_YEAR".
         DISPLAY pro-desp.FREQ WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "9XYR" OR FREQ = "NINE_TIMES_PER_YEAR"
       THEN DO:
         FREQ = "NINE_TIMES_PER_YEAR".
         DISPLAY pro-desp.FREQ WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "10XYR" OR FREQ = "TEN_TIMES_PER_YEAR"
       THEN DO:
         FREQ = "TEN_TIMES_PER_YEAR".
         DISPLAY pro-desp.FREQ WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "11XYR" OR FREQ = "ELEVEN_TIMES_PER_YEAR"
       THEN DO:
         FREQ = "ELEVEN_TIMES_PER_YEAR".
         DISPLAY pro-desp.FREQ WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "1X2YR" OR FREQ = "EVERY_TWO_YEARS"
       THEN DO:
         FREQ = "EVERY_TWO_YEARS".
         DISPLAY pro-desp.FREQ WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "1X3YR" OR FREQ = "EVERY_THREE_YEARS"
       THEN DO:
         FREQ = "EVERY_THREE_YEARS".
         DISPLAY pro-desp.FREQ WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "1X4YR" OR FREQ = "EVERY_FOUR_YEARS"
       THEN DO:
         FREQ = "EVERY_FOUR_YEARS".
         DISPLAY pro-desp.FREQ WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "1X5YR" OR FREQ = "EVERY_FIVE_YEARS"
       THEN DO:
         FREQ = "EVERY_FIVE_YEARS".
         DISPLAY pro-desp.FREQ WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "1X18M" OR FREQ = "EVERY_EIGHTEEN_MONTHS"
       THEN DO:
         FREQ = "EVERY_EIGHTEEN_MONTHS".
         DISPLAY pro-desp.FREQ WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "SPCL" OR FREQ = "SPECIAL_CLEANING"
       THEN DO:
         FREQ = "SPECIAL_CLEANING".
         DISPLAY pro-desp.FREQ WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "OC" OR FREQ = "ON_CALL"
       THEN DO:
         FREQ = "ON_CALL".
         DISPLAY pro-desp.FREQ WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "IC" OR FREQ = "INITIAL_CLEANING"
       THEN DO:
         FREQ = "INITIAL_CLEANING".
         DISPLAY pro-desp.FREQ WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE
       IF FREQ BEGINS "1X" OR FREQ = "ONE_TIME_CLEANING"
       THEN DO:
         FREQ = "ONE_TIME_CLEANING".
         DISPLAY pro-desp.FREQ WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE.
         LEAVE.
       END.
       ELSE MESSAGE color blink-y "INCORRECT FREQUENCY".
   END.
if noter then do:
   message "".
   message color BLINK-Y
"                           ___N O T E S   E X I S T___                          ".
end.
    IF LASTKEY = KEYCODE("F4") OR LASTKEY = KEYCODE("ESC") THEN UNDO, RETRY.

       UPDATE  START-DT AMT cod-amt DESC01 DESC02 DESC03 DESC04 DESC05 DESC06
               DESC07 DESC08 DESC09 DESC10 SPC-INT StartTime EndTime /*DL WHCH-AMT AC# A2-DESP*/
               /*AMT2-MUL AMT2*/ CL-DATE /*COMMIS*/ SUB-CON /*pro-desp.BudgetedHours*/ EquipmentRequired
               wks[1] wks[2] wks[3] wks[4] wks[5] PRO-DESP.SUB# PRO-DESP.ROUTE#
               WKDAY[1] WKDAY[2] WKDAY[3] WKDAY[4] WKDAY[5] WKDAY[6] WKDAY[7]
               MTH[1] MTH[2] MTH[3] MTH[4] MTH[5] MTH[6] MTH[7] MTH[8] MTH[9]
               MTH[10] MTH[11] MTH[12] PRO-DESP.PO#
               WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA NO-HIDE
       editing:
        readkey.
        if lastkey <> keycode("CTRL-N") and lastkey <> keycode("CTRL-P") and
         lastkey <> keycode("PAGE-DOWN") and lastkey <> keycode("PAGE-UP") and
         lastkey <> keycode("ALT-C")
          then do:
            apply lastkey.
            if go-pending
            then leave.
            else next.
        end.
        if lastkey = keycode("CTRL-N") and not noter then do:
                UPDATE ADDNOTE
                    WITH FRAME A SIDE-LABELS OVERLAY ROW 10 COLUMN 10.
                    IF ADDNOTE THEN DO:
                        CREATE NOTES.
                     ASSIGN
                     NOTES.COMP# = XCOM
                     NOTES.DIV# = XDIV
                     NOTES.CUST# = XCUST
                     NOTES.PROPSL# = XPROP
                     NOTES.ITEM# = XITEM.
                     update note-line1 note-line2 note-line3
                            note-line4 note-line5 note-line6
                            with frame b overlay side-labels column 20 row 15
                            TITLE 'N  O  T  E  S'.
                     RELEASE NOTES.
                  END.
            end.
        if lastkey = keycode("CTRL-P") and poser then do:

        repeat:
          display
            PROPSL.PROPSL# LABEL "PROPOSAL NUMBER"
            PROPSL.COMP# LABEL "COMPANY NUMBER"
            PROPSL.DIV# LABEL "DIVISION NUMBER" SKIP(1)
            PROPSL.CUST# LABEL "CUSTOMER NUMBER "
            PROPSL.SUB# PROPSL.ROUTE# SKIP(1)
            PROPSL.C-NAME LABEL "Customer " SPACE(5)
            PROPSL.L-NAME LABEL "L-Name " SKIP(0)
            PROPSL.ADDR1 LABEL "   Street" SPACE(5)
            PROPSL.LADDR01 LABEL "L-Addr1" SKIP(0)
            PROPSL.ADDR2 LABEL "Address 2" SPACE(5)
            PROPSL.LADDR02 LABEL " L-City" SKIP(0)
            PROPSL.ADDR3 LABEL "     City" SPACE(5)
            PROPSL.LADDR03 LABEL "  L-Zip" SKIP(0)
            PROPSL.ADDR4 LABEL "Contact 1" SPACE(5)
            PROPSL.L-STATE
            PROPSL.L-TELE SKIP(0)
            PROPSL.ADDR5 LABEL "Contact 2" SPACE(5)
            PROPSL.L-TELE2 LABEL "2ND PHONE OR EXT#" SKIP(0)
            PROPSL.STATE LABEL "State    " SPACE(5)
            PROPSL.ZIP SPACE(8)
            PROPSL.L-COMMENTS LABEL "COMMENT" SKIP(0)
            PROPSL.TELE LABEL "TELEPHONE" SPACE(16)
            PROPSL.TOT-AMT SKIP(0)
            PROPSL.FAX-TELE LABEL "FAX PHONE" SPACE(16)
            PROPSL.DATE-P LABEL "PROPOSAL DATE" SKIP(0)
            PROPSL.EMAIL LABEL "Extension" SPACE(5)
            PROPSL.CUST-TRF LABEL "CUSTOMER TRANSFERED" SKIP(0)
            PROPSL.JANITOR LABEL "JANITORIAL ?" SPACE(24)
            PROPSL.ACTIVE LABEL "CUSTOMER STATUS" SKIP(0)
            PROPSL.MLABEL LABEL "MAILING LIST ?" skip(0)
            propsl.email1 label "1st email" skip(0)
            propsl.email2 label "2nd email"
            propsl.startdate
            propsl.enddate
                WITH SIDE-LABELS frame p overlay.
            update PROPSL.C-NAME PROPSL.ADDR1 PROPSL.ADDR2
                   PROPSL.ADDR3 PROPSL.ADDR4 PROPSL.ADDR5 PROPSL.STATE
                   PROPSL.ZIP PROPSL.TELE PROPSL.FAX-TELE PROPSL.EMAIL
                   PROPSL.JANITOR PROPSL.MLABEL PROPSL.L-NAME
                   PROPSL.LADDR01 PROPSL.LADDR02 PROPSL.LADDR03
                   PROPSL.L-STATE PROPSL.L-TELE PROPSL.L-TELE2
                   PROPSL.L-COMMENTS PROPSL.TOT-AMT PROPSL.DATE-P
                   propsl.email1 propsl.email2 
                   propsl.startdate propsl.enddate
                   WITH FRAME P COLOR DISPLAY NORMAL PROMPT W/MA.
            assign propsl.
            end.
            end.
/*******************************************************************/
{pld-chg.i}
/*******************************************************************/


        if lastkey = keycode("CTRL-N") and noter then do:
            FIND NOTES WHERE NOTES.COMP# = XCOM AND
                             NOTES.DIV# = XDIV AND
                             NOTES.CUST# = XCUST AND
                             NOTES.PROPSL# = XPROP AND
                             NOTES.ITEM# = XITEM NO-ERROR.
            update notes.note-line1 notes.note-line2 notes.note-line3
                   notes.note-line4 notes.note-line5 notes.note-line6
                   with frame b overlay side-labels column 20 row 15.
        END.
        if lastkey = keycode("PAGE-UP") then do:
        find prev pro-desp where pro-desp.propsl# = propsl.propsl# and
                                 pro-desp.comp# = propsl.comp# and
                                 pro-desp.div# = propsl.div# and
                                 pro-desp.cust# = propsl.cust# no-error.
        if not available pro-desp then do:
        find last pro-desp where pro-desp.propsl# = propsl.propsl# and
                                 pro-desp.comp# = propsl.comp# and
                                 pro-desp.div# = propsl.div# and
                                 pro-desp.cust# = propsl.cust# no-error.
        end.

    DISPLAY
               PRO-DESP.PROPSL# PRO-DESP.ITEM# PRO-DESP.COMP# PRO-DESP.DIV#
               START-DT PRO-DESP.CUST# PRO-DESP.FREQ Amt cod-amt
               DESC01 DESC02 DESC03 DESC04 DESC05 DESC06 DESC07 DESC08
               DESC09 DESC10 SPC-INTR /*DL*/ wks[1] WKDAY[1] MTH[1] MTH[7]
               /*WHCH-AMT AC#*/ StartTime /*pro-desp.BudgetedHours*/ EquipmentRequired
               wks[2] WKDAY[2] MTH[2] MTH[8] EndTime /*A2-DESP*/ wks[3] WKDAY[3] MTH[3] MTH[9]
               /*(AMT2-MUL*/ wks[4] WKDAY[4] MTH[4] MTH[10] wks[5] /*AMT2*/ PRO-DESP.SUB#
               WKDAY[5] MTH[5] MTH[11] PRO-DESP.CL-DATE PRO-DESP.ROUTE#
               WKDAY[6] MTH[6] MTH[12] /*COMMIS*/ SUB-CON WKDAY[7] PRO-DESP.PO#
               WITH FRAME X.
        end.
        if lastkey = keycode("PAGE-DOWN") then do:
        find next pro-desp where pro-desp.propsl# = propsl.propsl# and
                                 pro-desp.comp# = propsl.comp# and
                                 pro-desp.div# = propsl.div# and
                                 pro-desp.cust# = propsl.cust# no-error.
        if not available pro-desp then do:
        find first pro-desp where pro-desp.propsl# = propsl.propsl# and
                                  pro-desp.comp# = propsl.comp# and
                                  pro-desp.div# = propsl.div# and
                                  pro-desp .cust# = propsl.cust# no-error.
        end.

    DISPLAY
               PRO-DESP.PROPSL# PRO-DESP.ITEM# PRO-DESP.COMP# PRO-DESP.DIV#
               START-DT PRO-DESP.CUST# PRO-DESP.FREQ AMT cod-amt
               DESC01 DESC02 DESC03 DESC04 DESC05 DESC06 DESC07 DESC08
               DESC09 DESC10 SPC-INTR /*DL*/ wks[1] WKDAY[1] MTH[1] MTH[7]
               /*WHCH-AMT AC#*/ StartTime /*pro-desp.BudgetedHours*/ EquipmentRequired
               wks[2] WKDAY[2] MTH[2] MTH[8] EndTime /*A2-DESP*/ wks[3] WKDAY[3] MTH[3] MTH[9]
               /*AMT2-MUL*/ wks[4] WKDAY[4] MTH[4] MTH[10] wks[5] /*AMT2*/ PRO-DESP.SUB#
               WKDAY[5] MTH[5] MTH[11] PRO-DESP.CL-DATE PRO-DESP.ROUTE#
               WKDAY[6] MTH[6] MTH[12] /*COMMIS*/ SUB-CON WKDAY[7] PRO-DESP.PO#
               WITH FRAME X.
        end.
if noter then do:
   message "".
   message color BLINK-Y
"                           ___N O T E S   E X I S T___                          ".
end.
       END.
       RELEASE PRO-DESP.
   END.
END.
