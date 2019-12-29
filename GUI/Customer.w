&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          psg              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrowseByTicket

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TICKET PROPSL PRO-DESP

/* Definitions for BROWSE BrowseByTicket                                */
&Scoped-define FIELDS-IN-QUERY-BrowseByTicket TICKET.SUB# TICKET.ROUTE# ~
TICKET.ITEM# TICKET.TicketDate TICKET.MONTH# TICKET.TOT-AMT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrowseByTicket 
&Scoped-define QUERY-STRING-BrowseByTicket FOR EACH TICKET NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrowseByTicket OPEN QUERY BrowseByTicket FOR EACH TICKET NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrowseByTicket TICKET
&Scoped-define FIRST-TABLE-IN-QUERY-BrowseByTicket TICKET


/* Definitions for BROWSE BrowseProposal                                */
&Scoped-define FIELDS-IN-QUERY-BrowseProposal PROPSL.CUST# PROPSL.PROPSL# ~
PROPSL.C-NAME PROPSL.ADDR1 PROPSL.ADDR2 PROPSL.ADDR3 PROPSL.ADDR4 ~
PROPSL.ADDR5 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrowseProposal 
&Scoped-define QUERY-STRING-BrowseProposal FOR EACH PROPSL NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrowseProposal OPEN QUERY BrowseProposal FOR EACH PROPSL NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrowseProposal PROPSL
&Scoped-define FIRST-TABLE-IN-QUERY-BrowseProposal PROPSL


/* Definitions for BROWSE BrowseProposals                               */
&Scoped-define FIELDS-IN-QUERY-BrowseProposals PRO-DESP.PROPSL# ~
PRO-DESP.ITEM# PRO-DESP.ROUTE# PRO-DESP.FREQ PRO-DESP.wks[1] ~
PRO-DESP.wks[2] PRO-DESP.wks[3] PRO-DESP.wks[4] PRO-DESP.wks[5] ~
PRO-DESP.WKDAY[1] PRO-DESP.WKDAY[2] PRO-DESP.WKDAY[3] PRO-DESP.WKDAY[4] ~
PRO-DESP.WKDAY[5] PRO-DESP.WKDAY[6] PRO-DESP.WKDAY[7] 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrowseProposals 
&Scoped-define QUERY-STRING-BrowseProposals FOR EACH PRO-DESP NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrowseProposals OPEN QUERY BrowseProposals FOR EACH PRO-DESP NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrowseProposals PRO-DESP
&Scoped-define FIRST-TABLE-IN-QUERY-BrowseProposals PRO-DESP


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BrowseByTicket}~
    ~{&OPEN-QUERY-BrowseProposals}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 ComboDiv FillProposalNumber ~
FillCustNum FillCustName BrowseProposal BrowseProposals BrowseByTicket 
&Scoped-Define DISPLAYED-OBJECTS ComboDiv FillProposalNumber FillCustNum ~
FillCustName FillCustomer FillAddress1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE ComboDiv AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Div" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FillAddress1 AS CHARACTER FORMAT "X(25)":U 
     LABEL "Street" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE FillCustName AS CHARACTER FORMAT "X(20)":U 
     LABEL "Customer Name" 
     VIEW-AS FILL-IN 
     SIZE 46 BY 1 NO-UNDO.

DEFINE VARIABLE FillCustNum AS CHARACTER FORMAT "X(6)":U 
     LABEL "Customer Number" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE FillCustomer AS CHARACTER FORMAT "X(25)":U 
     LABEL "Customer" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE FillProposalNumber AS CHARACTER FORMAT "X(10)":U 
     LABEL "Proposal Number" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 205 BY 1.91.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrowseByTicket FOR 
      TICKET SCROLLING.

DEFINE QUERY BrowseProposal FOR 
      PROPSL SCROLLING.

DEFINE QUERY BrowseProposals FOR 
      PRO-DESP SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrowseByTicket
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrowseByTicket C-Win _STRUCTURED
  QUERY BrowseByTicket NO-LOCK DISPLAY
      TICKET.SUB# FORMAT "ZZ":U
      TICKET.ROUTE# FORMAT "ZZ":U
      TICKET.ITEM# FORMAT "ZZZZ":U
      TICKET.TicketDate FORMAT "99/99/9999":U
      TICKET.MONTH# FORMAT "ZZ":U
      TICKET.TOT-AMT FORMAT "$->>>,>>>,>>>.99":U WIDTH 13
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 64 BY 4.52 FIT-LAST-COLUMN.

DEFINE BROWSE BrowseProposal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrowseProposal C-Win _STRUCTURED
  QUERY BrowseProposal NO-LOCK DISPLAY
      PROPSL.CUST# FORMAT "ZZZZZZZZZZ":U
      PROPSL.PROPSL# FORMAT "ZZZZZZZZZZ":U
      PROPSL.C-NAME FORMAT "X(25)":U
      PROPSL.ADDR1 FORMAT "X(25)":U
      PROPSL.ADDR2 FORMAT "X(25)":U
      PROPSL.ADDR3 FORMAT "X(25)":U
      PROPSL.ADDR4 FORMAT "X(25)":U
      PROPSL.ADDR5 FORMAT "X(25)":U WIDTH 36.8
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 200 BY 6.67 FIT-LAST-COLUMN.

DEFINE BROWSE BrowseProposals
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrowseProposals C-Win _STRUCTURED
  QUERY BrowseProposals NO-LOCK DISPLAY
      PRO-DESP.PROPSL# FORMAT "ZZZZZZZZZZ":U
      PRO-DESP.ITEM# FORMAT "ZZZZ":U
      PRO-DESP.ROUTE# FORMAT "ZZ":U
      PRO-DESP.FREQ FORMAT "X(25)":U
      PRO-DESP.wks[1] FORMAT "yes/no":U
      PRO-DESP.wks[2] FORMAT "yes/no":U
      PRO-DESP.wks[3] FORMAT "yes/no":U
      PRO-DESP.wks[4] FORMAT "yes/no":U
      PRO-DESP.wks[5] FORMAT "yes/no":U
      PRO-DESP.WKDAY[1] FORMAT "yes/no":U
      PRO-DESP.WKDAY[2] FORMAT "yes/no":U
      PRO-DESP.WKDAY[3] FORMAT "yes/no":U
      PRO-DESP.WKDAY[4] FORMAT "yes/no":U
      PRO-DESP.WKDAY[5] FORMAT "yes/no":U
      PRO-DESP.WKDAY[6] FORMAT "yes/no":U
      PRO-DESP.WKDAY[7] FORMAT "yes/no":U WIDTH 7.4
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 142 BY 7.86 ROW-HEIGHT-CHARS .52 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     ComboDiv AT ROW 1.24 COL 18 COLON-ALIGNED WIDGET-ID 6
     FillProposalNumber AT ROW 3.14 COL 21 COLON-ALIGNED WIDGET-ID 8
     FillCustNum AT ROW 3.14 COL 68 COLON-ALIGNED WIDGET-ID 10
     FillCustName AT ROW 3.38 COL 118 COLON-ALIGNED WIDGET-ID 16
     BrowseProposal AT ROW 5.29 COL 10 WIDGET-ID 200
     BrowseProposals AT ROW 12.91 COL 61 WIDGET-ID 300
     FillCustomer AT ROW 13.14 COL 19 COLON-ALIGNED WIDGET-ID 12
     FillAddress1 AT ROW 14.33 COL 19 COLON-ALIGNED WIDGET-ID 18
     BrowseByTicket AT ROW 22.91 COL 16 WIDGET-ID 400
     RECT-1 AT ROW 2.67 COL 4 WIDGET-ID 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 350.6 BY 40.76 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 40.76
         WIDTH              = 350.6
         MAX-HEIGHT         = 48.43
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 48.43
         VIRTUAL-WIDTH      = 384
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BrowseProposal FillCustName DEFAULT-FRAME */
/* BROWSE-TAB BrowseProposals BrowseProposal DEFAULT-FRAME */
/* BROWSE-TAB BrowseByTicket FillAddress1 DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN FillAddress1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FillCustomer IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrowseByTicket
/* Query rebuild information for BROWSE BrowseByTicket
     _TblList          = "psg.TICKET"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = psg.TICKET.SUB#
     _FldNameList[2]   = psg.TICKET.ROUTE#
     _FldNameList[3]   = psg.TICKET.ITEM#
     _FldNameList[4]   = psg.TICKET.TicketDate
     _FldNameList[5]   = psg.TICKET.MONTH#
     _FldNameList[6]   > psg.TICKET.TOT-AMT
"TOT-AMT" ? ? "decimal" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BrowseByTicket */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrowseProposal
/* Query rebuild information for BROWSE BrowseProposal
     _TblList          = "psg.PROPSL"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = psg.PROPSL.CUST#
     _FldNameList[2]   = psg.PROPSL.PROPSL#
     _FldNameList[3]   = psg.PROPSL.C-NAME
     _FldNameList[4]   = psg.PROPSL.ADDR1
     _FldNameList[5]   = psg.PROPSL.ADDR2
     _FldNameList[6]   = psg.PROPSL.ADDR3
     _FldNameList[7]   = psg.PROPSL.ADDR4
     _FldNameList[8]   > psg.PROPSL.ADDR5
"PROPSL.ADDR5" ? ? "character" ? ? ? ? ? ? no ? no no "36.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrowseProposal */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrowseProposals
/* Query rebuild information for BROWSE BrowseProposals
     _TblList          = "psg.PRO-DESP"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = psg.PRO-DESP.PROPSL#
     _FldNameList[2]   = psg.PRO-DESP.ITEM#
     _FldNameList[3]   = psg.PRO-DESP.ROUTE#
     _FldNameList[4]   = psg.PRO-DESP.FREQ
     _FldNameList[5]   = psg.PRO-DESP.wks[1]
     _FldNameList[6]   = psg.PRO-DESP.wks[2]
     _FldNameList[7]   = psg.PRO-DESP.wks[3]
     _FldNameList[8]   = psg.PRO-DESP.wks[4]
     _FldNameList[9]   = psg.PRO-DESP.wks[5]
     _FldNameList[10]   = psg.PRO-DESP.WKDAY[1]
     _FldNameList[11]   = psg.PRO-DESP.WKDAY[2]
     _FldNameList[12]   = psg.PRO-DESP.WKDAY[3]
     _FldNameList[13]   = psg.PRO-DESP.WKDAY[4]
     _FldNameList[14]   = psg.PRO-DESP.WKDAY[5]
     _FldNameList[15]   = psg.PRO-DESP.WKDAY[6]
     _FldNameList[16]   > psg.PRO-DESP.WKDAY[7]
"WKDAY[7]" ? ? "logical" ? ? ? ? ? ? no ? no no "7.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BrowseProposals */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BrowseProposal
&Scoped-define SELF-NAME BrowseProposal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BrowseProposal C-Win
ON VALUE-CHANGED OF BrowseProposal IN FRAME DEFAULT-FRAME
DO:
        RUN DisplayProposal.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BrowseProposals
&Scoped-define SELF-NAME BrowseProposals
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BrowseProposals C-Win
ON VALUE-CHANGED OF BrowseProposals IN FRAME DEFAULT-FRAME
DO:
  RUN displaytickets.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FillCustName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FillCustName C-Win
ON LEAVE OF FillCustName IN FRAME DEFAULT-FRAME /* Customer Name */
DO:
  RUN OpenQueryProposal.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FillCustNum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FillCustNum C-Win
ON LEAVE OF FillCustNum IN FRAME DEFAULT-FRAME /* Customer Number */
DO:
   RUN OpenQueryProposal.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FillProposalNumber
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FillProposalNumber C-Win
ON LEAVE OF FillProposalNumber IN FRAME DEFAULT-FRAME /* Proposal Number */
DO:
   RUN OpenQueryProposal.
 /*
 IF FillProposalNumber:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN
      FIND Propsl WHERE
      Propsl.Propsl# = INTEGER(FillProposalNumber:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(Propsl) THEN DO:
         MESSAGE "Cannot find Proposal: " FillProposalNumber:SCREEN-VALUE.
         APPLY "Entry" TO FillProposalNumber.
         LEAVE.
      END.
      RUN DisplayCustomer.
   */   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BrowseByTicket
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN FillCombos.
  APPLY "Entry" TO FillProposalNumber.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayCustomer C-Win 
PROCEDURE DisplayCustomer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN 
         FillCustomer:SCREEN-VALUE IN FRAME {&FRAME-NAME} = PROPSL.C-NAME.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayProposal C-Win 
PROCEDURE DisplayProposal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
        ASSIGN FillCustomer:SCREEN-VALUE IN FRAME {&frame-name} = PROPSL.C-NAME
               FillAddress1:SCREEN-VALUE = PROPSL.ADDR1.
               
        OPEN QUERY BrowseProposals FOR EACH
         Pro-Desp WHERE Pro-Desp.propsl# = Propsl.Propsl# AND
         pro-desp.cust# = propsl.cust#
         NO-LOCK.
               .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayTickets C-Win 
PROCEDURE DisplayTickets :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      OPEN QUERY BrowseByTicket FOR EACH Ticket WHERE 
        TICKET.COMP# = pro-desp.comp# AND
        TICKET.DIV#  = PRO-DESP.DIV# AND
        TICKET.SUB#  = pro-desp.sub# AND
        TICKET.CUST# = pro-desp.cust# AND
        TICKET.PROPSL# = pro-desp.propsl#
        NO-LOCK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY ComboDiv FillProposalNumber FillCustNum FillCustName FillCustomer 
          FillAddress1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 ComboDiv FillProposalNumber FillCustNum FillCustName 
         BrowseProposal BrowseProposals BrowseByTicket 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillCombos C-Win 
PROCEDURE FillCombos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH div-file:
       ComboDiv:ADD-LAST(Div-FILE.DV-NAME,div-FILE.DIV#) IN FRAME {&frame-name}.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQueryProposal C-Win 
PROCEDURE OpenQueryProposal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  OPEN QUERY BrowseProposal 
     FOR EACH Propsl WHERE propsl.Propsl# = 
        (IF FillProposalNumber:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN INTEGER(FillProposalNumber:SCREEN-VALUE )
          ELSE Propsl.Propsl#) AND
        (Propsl.Cust# = IF FillCustNum:SCREEN-VALUE NE "" THEN INTEGER(FillCustNum:SCREEN-VALUE)
                       ELSE propsl.cust#)    AND
        (PROPSL.C-NAME BEGINS IF FillCustName:SCREEN-VALUE NE "" THEN FillCustName:SCREEN-VALUE 
                         ELSE propsl.c-name)
        NO-LOCK.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

