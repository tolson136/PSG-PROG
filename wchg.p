DEFINE SHARED VARIABLE XCOM AS INTEGER FORMAT "ZZ".
DEFINE SHARED VARIABLE XDIV AS INTEGER FORMAT "ZZ".
DEFINE SHARED VARIABLE XCOM-N AS CHAR FORMAT "X(30)".
DEFINE SHARED VARIABLE XDIV-N AS CHAR FORMAT "X(30)".
DEFINE SHARED VARIABLE XOPR AS CHAR FORMAT "XXX".
DEFINE VARIABLE BEG# AS INTEGER FORMAT "ZZ"
  LABEL "MONTH".
DEFINE VARIABLE F-CUST AS DECIMAL FORMAT "ZZZZZZZZZZ"
    LABEL "CUST #".
DEFINE VARIABLE F-PROP AS DECIMAL FORMAT "ZZZZZZZZZZ"
    LABEL "PROP #".
DEFINE VARIABLE F-ITEM AS INTEGER FORMAT "ZZZZ"
    LABEL "ITEM #".
DEFINE VARIABLE F-INDX AS INTEGER FORMAT "ZZ"
    LABEL "TICK INDEX".
define variable f-wk as integer format "ZZ"
    label "WEEK #".
REPEAT:
DISPLAY SPACE(1)
"W E E K L Y   S I N G L E   T I C K E T   M A I N T E N A N C E   S C R E E N"
    SKIP(0)
XCOM-N LABEL "CO" XDIV-N LABEL "DV" XOPR LABEL "OPER"
SKIP(1) WITH FRAME X NO-BOX SIDE-LABELS.
PROMPT-FOR TICKET.CUST#
           TICKET.PROPSL#
           TICKET.ITEM#
           TICKET.MONTH#
           ticket.wk#
           TICKET.T-INDX WITH FRAME X NO-BOX SIDE-LABELS.
F-CUST = INPUT TICKET.CUST#.
BEG# = INPUT TICKET.MONTH#.
F-PROP = INPUT TICKET.PROPSL#.
F-ITEM = INPUT TICKET.ITEM#.
F-INDX = INPUT TICKET.T-INDX.
f-wk = input ticket.wk#.
FIND FIRST TICKET WHERE TICKET.COMP# = XCOM AND
                        TICKET.DIV# = XDIV AND
                        TICKET.CUST# = F-CUST AND
                        TICKET.PROPSL# = F-PROP AND
                        TICKET.ITEM# = F-ITEM AND
                        TICKET.MONTH# = BEG# AND
                        ticket.wk# = f-wk and
                        TICKET.T-INDX = F-INDX NO-ERROR.
IF NOT AVAILABLE TICKET THEN DO:
    MESSAGE "THIS TICKET DOES NOT EXIST OR HAS ALREADY BEEN PROCESSED".
    NEXT.
END.
TICKET.COMP# = XCOM.
TICKET.DIV# = XDIV.
DISPLAY TICKET.CUST# LABEL "CUST #"
        TICKET.PROPSL# LABEL "PROP #"
        TICKET.ITEM# LABEL "ITEM #"
        TICKET.MONTH# LABEL "MONTH"
        ticket.wk# label "WEEK #"
        TICKET.T-INDX LABEL "TICK INDEX" SKIP(0)
        TICKET.M-NUM LABEL "MANAGER #" SPACE(11)
        TICKET.M-NAME LABEL "MANAGER NAME" SKIP(0)
        ticket.freq LABEL "FREQUENCY" SKIP(0) SPACE(25)
        "BALANCES & VOLUMES" SPACE(10)
        "EQUIPMENT" SKIP(0) SPACE(25)
        "------------------" SPACE(10)
        "---------" SKIP(0)
        TICKET.PRT LABEL "PRINTED?   " SPACE(11)
        TICKET.DL-BUD LABEL "D/L BUD  " SPACE(1)
        TICKET.CABLE LABEL "CABLE REQ'D?" SKIP(0)
        TICKET.DATE-PRT LABEL "DATE       " SPACE(2)
        TICKET.DL LABEL "D/L USE  " SPACE(1)
        TICKET.CABLE-AMT LABEL "CABLE AMT" SKIP(0)
        TICKET.RET LABEL "TICK RET'D?" SPACE(11)
        TICKET.DL-BAL LABEL "D/L BAL  " SPACE(1)
        TICKET.SCAFFOLD LABEL "SCAFF REQ'D?" SKIP(0)
        TICKET.DATE-RET LABEL "DATE       " SPACE(2)
        TICKET.WK-DL-BUD LABEL "WK DL BUD" SPACE(1)
        TICKET.SCAFF-AMT LABEL "SCAFF AMT" SKIP(0)
        TICKET.T-STAT LABEL "STATUS     " SPACE(11)
        TICKET.WK-DL LABEL "WK DL USE" SPACE(1)
        TICKET.OTHER LABEL "OTHER REQ'D?" SKIP(0) SPACE(25)
        TICKET.WK-DL-BAL LABEL "WK DL BAL" SPACE(1)
        TICKET.OTH-AMT LABEL "OTHER AMT" SKIP(0) SPACE(25)
        TICKET.TOT-AMT LABEL "ORIG VOL " SKIP(0) SPACE(25)
        TICKET.TOT-AMT-REM LABEL "REMAINING" SKIP(0) SPACE(25)
        TICKET.WK-START LABEL "WEEK STRT" SKIP(0) SPACE(25)
        TICKET.WK-USED LABEL "WEEK USED" SKIP(0) SPACE(25)
        TICKET.WK-BAL LABEL "WEEK BAL " SKIP(0) SPACE(25)
        TICKET.MTD-VOL LABEL "MTD VOL  " SKIP(0) SPACE(25)
        TICKET.WTD-VOL LABEL "TTD VOL  " SKIP(0)
        WITH FRAME X NO-BOX NO-HIDE SIDE-LABELS.
        UPDATE TICKET.M-NUM WITH FRAME X.
        FIND FIRST SALESMAN WHERE SALESMAN.SLS# = TICKET.M-NUM.
        TICKET.M-NAME = SALESMAN.SLSNAME.
        DISPLAY TICKET.M-NAME WITH FRAME X NO-BOX SIDE-LABELS.
  REPEAT:
       UPDATE ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
       IF ticket.freq BEGINS "D" OR ticket.freq = "DAILY"
       THEN DO:
             ticket.freq = "DAILY".
             DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
             LEAVE.
       END.
       ELSE       
       IF ticket.freq BEGINS "2XW" OR ticket.freq = "TWO_TIMES_PER_WEEK"
       THEN DO:
         ticket.freq = "TWO_TIMES_PER_WEEK".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "3XW" OR ticket.freq = "THREE_TIMES_PER_WEEK"
       THEN DO:
         ticket.freq = "THREE_TIMES_PER_WEEK".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "4XW" OR ticket.freq = "FOUR_TIMES_PER_WEEK"
       THEN DO:
         ticket.freq = "FOUR_TIMES_PER_WEEK".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "5XW" OR ticket.freq = "FIVE_TIMES_PER_WEEK"
       THEN DO:
         ticket.freq = "FIVE_TIMES_PER_WEEK".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "6XW" OR ticket.freq = "SIX_TIMES_PER_WEEK"
       THEN DO:
         ticket.freq = "SIX_TIMES_PER_WEEK".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "W" OR ticket.freq = "WEEKLY"
       THEN DO:
         ticket.freq = "WEEKLY".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "2W" OR ticket.freq = "EVERY_TWO_WEEKS"
       THEN DO:
         ticket.freq = "EVERY_TWO_WEEKS".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "3W" OR ticket.freq = "EVERY_THREE_WEEKS"
       THEN DO:
         ticket.freq = "EVERY_THREE_WEEKS".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "4W" OR ticket.freq = "EVERY_FOUR_WEEKS"
       THEN DO:
         ticket.freq = "EVERY_FOUR_WEEKS".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "5W" OR ticket.freq = "EVERY_FIVE_WEEKS"
       THEN DO:
         ticket.freq = "EVERY_FIVE_WEEKS".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "6W" OR ticket.freq = "EVERY_SIX_WEEKS"
       THEN DO:
         ticket.freq = "EVERY_SIX_WEEKS".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "8W" OR ticket.freq = "EVERY_EIGHT_WEEKS"
       THEN DO:
         ticket.freq = "EVERY_EIGHT_WEEKS".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "M" OR ticket.freq = "MONTHLY"
       THEN DO:
         ticket.freq = "MONTHLY".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "Q" OR ticket.freq = "QUARTERLY"
       THEN DO:
         ticket.freq = "QUARTERLY".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "2XM" OR ticket.freq = "TWO_TIMES_PER_MONTH"
       THEN DO:
         ticket.freq = "TWO_TIMES_PER_MONTH".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "A" OR ticket.freq = "ANNUAL"
       THEN DO:
         ticket.freq = "ANNUAL".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "2XYR" OR ticket.freq = "TWO_TIMES_PER_YEAR"
       THEN DO:
         ticket.freq = "TWO_TIMES_PER_YEAR".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "3XYR" OR ticket.freq = "THREE_TIMES_PER_YEAR"
       THEN DO:
         ticket.freq = "THREE_TIMES_PER_YEAR".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "4XYR" OR ticket.freq = "FOUR_TIMES_PER_YEAR"
       THEN DO:
         ticket.freq = "FOUR_TIMES_PER_YEAR".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "5XYR" OR ticket.freq = "FIVE_TIMES_PER_YEAR"
       THEN DO:
         ticket.freq = "FIVE_TIMES_PER_YEAR".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "6XYR" OR ticket.freq = "SIX_TIMES_PER_YEAR"
       THEN DO:
         ticket.freq = "SIX_TIMES_PER_YEAR".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "7XYR" OR ticket.freq = "SEVEN_TIMES_PER_YEAR"
       THEN DO:
         ticket.freq = "SEVEN_TIMES_PER_YEAR".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "8XYR" OR ticket.freq = "EIGHT_TIMES_PER_YEAR"
       THEN DO:
         ticket.freq = "EIGHT_TIMES_PER_YEAR".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "9XYR" OR ticket.freq = "NINE_TIMES_PER_YEAR"
       THEN DO:
         ticket.freq = "NINE_TIMES_PER_YEAR".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "10XYR" OR ticket.freq = "TEN_TIMES_PER_YEAR"
       THEN DO:
         ticket.freq = "TEN_TIMES_PER_YEAR".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "11XYR" OR ticket.freq = "ELEVEN_TIMES_PER_YEAR"
       THEN DO:
         ticket.freq = "ELEVEN_TIMES_PER_YEAR".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "1X2YR" OR ticket.freq = "EVERY_TWO_YEARS"
       THEN DO:
         ticket.freq = "EVERY_TWO_YEARS".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "1X3YR" OR ticket.freq = "EVERY_THREE_YEARS"
       THEN DO:
         ticket.freq = "EVERY_THREE_YEARS".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "1X4YR" OR ticket.freq = "EVERY_FOUR_YEARS"
       THEN DO:
         ticket.freq = "EVERY_FOUR_YEARS".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "1X5YR" OR ticket.freq = "EVERY_FIVE_YEARS"
       THEN DO:
         ticket.freq = "EVERY_FIVE_YEARS".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "1X18M" OR ticket.freq = "EVERY_EIGHTEEN_MONTHS"
       THEN DO:
         ticket.freq = "EVERY_EIGHTEEN_MONTHS".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "SPCL" OR ticket.freq = "SPECIAL_CLEANING"
       THEN DO:
         ticket.freq = "SPECIAL_CLEANING".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "OC" OR ticket.freq = "ON_CALL"
       THEN DO:
         ticket.freq = "ON_CALL".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "IC" OR ticket.freq = "INITIAL_CLEANING"
       THEN DO:
         ticket.freq = "INITIAL_CLEANING".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE
       IF ticket.freq BEGINS "1X" OR ticket.freq = "ONE_TIME_CLEANING"
       THEN DO:
         ticket.freq = "ONE_TIME_CLEANING".
         DISPLAY ticket.freq WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
         LEAVE.
       END.
       ELSE MESSAGE "INCORRECT ticket.freqUENCY".
   END.
IF (USERID <> "OPERATIONS") AND (USERID <> "LANDMARK") AND (USERID <> "GARCIA")
THEN DO:
        UPDATE TICKET.PRT TICKET.DATE-PRT TICKET.RET
               TICKET.DATE-RET TICKET.T-STAT TICKET.DL-BUD
               TICKET.DL TICKET.DL-BAL TICKET.WK-DL-BUD TICKET.WK-DL
               TICKET.WK-DL-BAL TICKET.TOT-AMT TICKET.TOT-AMT-REM
               TICKET.WK-START TICKET.WK-USED TICKET.WK-BAL
               TICKET.MTD-VOL TICKET.WTD-VOL
               TICKET.CABLE
               TICKET.CABLE-AMT TICKET.SCAFFOLD TICKET.SCAFF-AMT
               TICKET.OTHER TICKET.OTH-AMT
                   WITH FRAME X COLOR DISPLAY NORMAL PROMPT W/MA.
        RELEASE TICKET.
END.
END.
