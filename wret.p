DEF SHARED VAR XCOM AS INTEGER FORMAT "ZZ".
DEFINE SHARED VARIABLE XDIV AS INTEGER FORMAT "ZZ".
DEFINE SHARED VARIABLE XCOM-N AS CHAR FORMAT "X(30)".
DEFINE SHARED VARIABLE XDIV-N AS CHAR FORMAT "X(30)".
DEFINE SHARED VARIABLE XOPR AS CHAR FORMAT "XXX".
DEFINE VAR ANS AS LOGICAL
    LABEL "ARE YOU SURE THIS INFORMATION IS CORRECT ?".
DEFINE VAR SAN AS LOGICAL
    LABEL "IS THIS TICKET/JOB COMPLETED ?".
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
DEFINE VAR F-NUM AS CHAR FORMAT "X(4)".
DEFINE VAR F-MAN AS INTEGER FORMAT "ZZZ".
DEF VAR F-DATE AS DATE FORMAT "99/99/9999".
def var x-date as date format "99/99/9999".
def var ctr as int.
def var counter as int.
def var oldchoice as char.
def var oddchoice as char.
def var cl-d as int.
def var cl-m as int.
def var cl-y as int format "9999".
def var xweek as int.
IF (USERID = "OPERATIONS") OR (USERID = "LANDMARK") OR (USERID = "GARCIA")
THEN DO:
    MESSAGE "YOU ARE NOT AUTHORIZED TO RUN THIS PROCEDURE".
    RETURN.
END.
REPEAT:
DISPLAY SPACE(5)
"W E E K L Y   R E T U R N E D   T I C K E T   I N P U T   S C R E E N"
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
xweek = input ticket.wk#.
FIND FIRST TICKET WHERE TICKET.COMP# = XCOM AND
                  TICKET.DIV# = XDIV AND
                  TICKET.CUST# = F-CUST AND
                  TICKET.PROPSL# = F-PROP AND
                  TICKET.ITEM# = F-ITEM AND
                  TICKET.MONTH# = BEG# AND
                  ticket.wk# = xweek and
                  TICKET.T-INDX = F-INDX AND
                  TICKET.T-STAT <> "X" AND
                  TICKET.T-STAT <> "W" NO-ERROR.
IF NOT AVAILABLE TICKET THEN DO:
    MESSAGE "THIS TICKET DOES NOT EXIST OR HAS ALREADY BEEN PROCESSED".
    UNDO, RETRY.
END.
FIND FIRST PRO-DESP WHERE PRO-DESP.COMP# = XCOM AND
                    PRO-DESP.DIV# = XDIV AND
                    PRO-DESP.CUST# = F-CUST AND
                    PRO-DESP.PROPSL# = F-PROP AND
                    PRO-DESP.ITEM# = F-ITEM NO-ERROR.
IF NOT AVAILABLE PRO-DESP THEN DO:
    MESSAGE "THIS PROPOSAL DESCRIPTION NO LONGER EXISTS".
    UNDO, RETRY.
END.
DISPLAY TICKET.CUST# LABEL "CUST #"
        TICKET.PROPSL# LABEL "PROP #"
        TICKET.ITEM# LABEL "ITEM #"
        TICKET.MONTH# LABEL "MONTH"
        ticket.wk# label "WEEK" format "z"
        TICKET.T-INDX LABEL "T. INDX" SKIP(1)
        TICKET.M-NUM LABEL "MANAGER #" SPACE(1)
        TICKET.M-NAME LABEL "MANAGER NAME" SKIP(0)
        TICKET.FREQ LABEL "FREQUENCY" SPACE(5)
        PRO-DESP.CL-DATE LABEL "COMPLETION DATE OF CLEANING" SKIP(0) SPACE(10)
        TICKET.DL-BUD LABEL "D/L BUD  " SPACE(10)
        TICKET.WK-DL-BUD LABEL "WK DL BUD" SKIP(0) SPACE(10)
        TICKET.DL LABEL "D/L USE  " SPACE(10)
        TICKET.WK-DL LABEL "WK DL USE" SKIP(0) SPACE(10)
        TICKET.DL-BAL LABEL "D/L BAL  " SPACE(10)
        TICKET.WK-DL-BAL SKIP(1) SPACE(20)
        WITH FRAME X NO-BOX SIDE-LABELS.
TICKET.COMP# = XCOM.
TICKET.DIV# = XDIV.
ticket.ret = "Y".
TICKET.WK-DL = TICKET.WK-DL-BAL.
TICKET.WK-DL-BAL = 0.
TICKET.M-NUM = 1.
TICKET.M-NAME = "PPS".
update pro-desp.cl-date with frame x color display normal prompt w/ma.
x-date = pro-desp.cl-date.
ticket.date-ret = x-date.
ticket.t-stat = "C".
display ticket.m-num ticket.m-name ticket.wk-dl ticket.wk-dl-bal 
        with frame x no-box side-labels.
        ANS = no.
        UPDATE ANS WITH ROW FRAME-ROW + 14 + FRAME-LINE + 5 COLUMN 20
                   WITH SIDE-LABELS OVERLAY FRAME DEL-FRAME NO-BOX
                   COLOR DISPLAY W/RED PROMPT RED/W.
        IF NOT ANS THEN DO:
            UNDO, RETRY.
            hide frame del-frame.
        END.
        HIDE FRAME DEL-FRAME.
            find first cl-date use-index indx06 where
                                     cl-date.comp# = ticket.comp# and
                                     cl-date.div# = ticket.div# and
                                     cl-date.cust# = ticket.cust# and
                                     cl-date.propsl# = ticket.propsl# and
                                     cl-date.month# = ticket.month# and
                                     cl-date.wk# = ticket.wk# and
                                     cl-date.item# = ticket.item# and
                                     cl-date.t-indx = ticket.t-indx and
                                     (cl-date.date-ret = x-date or
                                      cl-date.date-ret = 01/01/9999) no-error.
            if not available cl-date then do:
                CREATE CL-DATE.
                    CL-DATE.CL-DATE = x-date.
                    CL-DATE.COMP# = TICKET.COMP#.
                    CL-DATE.CUST# = TICKET.CUST#.
                    CL-DATE.DATE-RET = TICKET.DATE-RET.
                    CL-DATE.DIV# = TICKET.DIV#.
                    CL-DATE.FREQ = TICKET.FREQ.
                    CL-DATE.ITEM# = TICKET.ITEM#.
                    CL-DATE.M-NUM = TICKET.M-NUM.
                    CL-DATE.MONTH# = TICKET.MONTH#.
                    cl-date.wk# = ticket.wk#.
                    CL-DATE.PROPSL# = TICKET.PROPSL#.
                    CL-DATE.ROUTE# = TICKET.ROUTE#.
                    CL-DATE.SUB# = TICKET.SUB#.
                    CL-DATE.T-INDX = TICKET.T-INDX.
                RELEASE CL-DATE.
            end.
            else do:
                update cl-date.date-ret = ticket.date-ret.
                update cl-date.cl-date = x-date.
                update cl-date.m-num = ticket.m-num.
            end.
END.
