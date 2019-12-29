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
DEFINE VARIABLE Response AS LOGICAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FrameProposal

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES PROPSL

/* Definitions for FRAME FrameProposal                                  */
&Scoped-define FIELDS-IN-QUERY-FrameProposal PROPSL.LADDR03 PROPSL.CUST-TRF ~
PROPSL.MLABEL PROPSL.DATE-P PROPSL.DIV# PROPSL.ROUTE# PROPSL.EMAIL ~
PROPSL.StartDate PROPSL.email1 PROPSL.email2 PROPSL.SUB# PROPSL.EndDate ~
PROPSL.TOT-AMT PROPSL.PROPSL# PROPSL.ACTIVE PROPSL.ADDR1 PROPSL.ADDR2 ~
PROPSL.ADDR3 PROPSL.ADDR4 PROPSL.JANITOR PROPSL.ADDR5 PROPSL.L-COMMENTS ~
PROPSL.L-NAME PROPSL.COMP# PROPSL.L-STATE PROPSL.L-TELE PROPSL.L-TELE2 ~
PROPSL.LADDR01 PROPSL.LADDR02 
&Scoped-define ENABLED-FIELDS-IN-QUERY-FrameProposal PROPSL.LADDR03 ~
PROPSL.CUST-TRF PROPSL.MLABEL PROPSL.DATE-P PROPSL.DIV# PROPSL.ROUTE# ~
PROPSL.EMAIL PROPSL.StartDate PROPSL.email1 PROPSL.email2 PROPSL.SUB# ~
PROPSL.EndDate PROPSL.TOT-AMT PROPSL.PROPSL# PROPSL.ACTIVE PROPSL.JANITOR ~
PROPSL.L-COMMENTS PROPSL.L-NAME PROPSL.COMP# PROPSL.L-STATE PROPSL.L-TELE ~
PROPSL.L-TELE2 PROPSL.LADDR01 PROPSL.LADDR02 
&Scoped-define ENABLED-TABLES-IN-QUERY-FrameProposal PROPSL
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-FrameProposal PROPSL
&Scoped-define QUERY-STRING-FrameProposal FOR EACH PROPSL SHARE-LOCK
&Scoped-define OPEN-QUERY-FrameProposal OPEN QUERY FrameProposal FOR EACH PROPSL SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-FrameProposal PROPSL
&Scoped-define FIRST-TABLE-IN-QUERY-FrameProposal PROPSL


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS PROPSL.LADDR03 PROPSL.CUST-TRF PROPSL.MLABEL ~
PROPSL.DATE-P PROPSL.DIV# PROPSL.ROUTE# PROPSL.EMAIL PROPSL.StartDate ~
PROPSL.email1 PROPSL.email2 PROPSL.SUB# PROPSL.EndDate PROPSL.TOT-AMT ~
PROPSL.PROPSL# PROPSL.ACTIVE PROPSL.JANITOR PROPSL.L-COMMENTS PROPSL.L-NAME ~
PROPSL.COMP# PROPSL.L-STATE PROPSL.L-TELE PROPSL.L-TELE2 PROPSL.LADDR01 ~
PROPSL.LADDR02 
&Scoped-define ENABLED-TABLES PROPSL
&Scoped-define FIRST-ENABLED-TABLE PROPSL
&Scoped-Define ENABLED-OBJECTS SearchProposal ButtonClose 
&Scoped-Define DISPLAYED-FIELDS PROPSL.LADDR03 PROPSL.CUST-TRF ~
PROPSL.MLABEL PROPSL.DATE-P PROPSL.DIV# PROPSL.ROUTE# PROPSL.EMAIL ~
PROPSL.StartDate PROPSL.email1 PROPSL.email2 PROPSL.SUB# PROPSL.EndDate ~
PROPSL.TOT-AMT PROPSL.PROPSL# PROPSL.ACTIVE PROPSL.ADDR1 PROPSL.ADDR2 ~
PROPSL.ADDR3 PROPSL.ADDR4 PROPSL.JANITOR PROPSL.ADDR5 PROPSL.L-COMMENTS ~
PROPSL.L-NAME PROPSL.COMP# PROPSL.L-STATE PROPSL.L-TELE PROPSL.L-TELE2 ~
PROPSL.LADDR01 PROPSL.LADDR02 
&Scoped-define DISPLAYED-TABLES PROPSL
&Scoped-define FIRST-DISPLAYED-TABLE PROPSL
&Scoped-Define DISPLAYED-OBJECTS SearchProposal FillCust# FillCustName ~
FillAddr1 FillState-2 FillAddr2 FillAddr3 FillAddr4 FillAddr5 FillState ~
FillZip FillTelephone FillFax FillExtension 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON ButtonCancel 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON ButtonClose 
     LABEL "Close" 
     SIZE 15 BY 1.14.

DEFINE BUTTON ButtonSave 
     LABEL "&Save" 
     SIZE 15 BY 1.14.

DEFINE BUTTON ButtonUpdate 
     LABEL "&Update" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE FillAddr1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Street" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE FillAddr2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Address 2" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE FillAddr3 AS CHARACTER FORMAT "X(256)":U 
     LABEL "City" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE FillAddr4 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Contact 1" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE FillAddr5 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Contact 2" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE FillCust# AS CHARACTER FORMAT "X(256)":U 
     LABEL "Customer Number" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE FillCustName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Customer" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE FillExtension AS CHARACTER FORMAT "X(256)":U 
     LABEL "Extension" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE FillFax AS CHARACTER FORMAT "(999) 999-9999":U 
     LABEL "Fax" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE FillState AS CHARACTER FORMAT "X(2)":U 
     LABEL "State" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FillState-2 AS CHARACTER FORMAT "X(2)":U 
     LABEL "State" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FillTelephone AS CHARACTER FORMAT "(999) 999-9999":U 
     LABEL "Telephone" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE FillZip AS CHARACTER FORMAT "X(10)":U 
     LABEL "Zip" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE SearchProposal AS CHARACTER FORMAT "X(256)":U 
     LABEL "Proposal Number" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY FrameProposal FOR 
      PROPSL SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FrameProposal
     SearchProposal AT ROW 2.19 COL 25 COLON-ALIGNED WIDGET-ID 74
     FillCust# AT ROW 4.33 COL 25 COLON-ALIGNED WIDGET-ID 80
     FillCustName AT ROW 5.76 COL 17 COLON-ALIGNED WIDGET-ID 82
     PROPSL.LADDR03 AT ROW 6 COL 113 COLON-ALIGNED WIDGET-ID 54
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     FillAddr1 AT ROW 6.95 COL 17 COLON-ALIGNED WIDGET-ID 84
     PROPSL.CUST-TRF AT ROW 6.95 COL 145 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     PROPSL.MLABEL AT ROW 7 COL 113 COLON-ALIGNED WIDGET-ID 56
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     FillState-2 AT ROW 7 COL 166 COLON-ALIGNED WIDGET-ID 98
     PROPSL.DATE-P AT ROW 7.95 COL 145 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     FillAddr2 AT ROW 8.14 COL 18 COLON-ALIGNED WIDGET-ID 86
     PROPSL.DIV# AT ROW 8.95 COL 145 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     PROPSL.ROUTE# AT ROW 9 COL 113 COLON-ALIGNED WIDGET-ID 60
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     FillAddr3 AT ROW 9.33 COL 17 COLON-ALIGNED WIDGET-ID 88
     PROPSL.EMAIL AT ROW 9.95 COL 145 COLON-ALIGNED WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     PROPSL.StartDate AT ROW 10 COL 113 COLON-ALIGNED WIDGET-ID 62
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     FillAddr4 AT ROW 10.52 COL 17 COLON-ALIGNED WIDGET-ID 90
     PROPSL.email1 AT ROW 10.95 COL 145 COLON-ALIGNED WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     FillAddr5 AT ROW 11.71 COL 17 COLON-ALIGNED WIDGET-ID 92
     PROPSL.email2 AT ROW 11.95 COL 145 COLON-ALIGNED WIDGET-ID 32
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     PROPSL.SUB# AT ROW 12 COL 113 COLON-ALIGNED WIDGET-ID 66
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     PROPSL.EndDate AT ROW 12.95 COL 145 COLON-ALIGNED WIDGET-ID 34
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     FillState AT ROW 13.14 COL 17 COLON-ALIGNED WIDGET-ID 96
     FillZip AT ROW 13.14 COL 36 COLON-ALIGNED WIDGET-ID 100
     PROPSL.TOT-AMT AT ROW 14 COL 113 COLON-ALIGNED WIDGET-ID 70
          VIEW-AS FILL-IN 
          SIZE 24.4 BY 1
     FillTelephone AT ROW 14.81 COL 17 COLON-ALIGNED WIDGET-ID 102
     FillFax AT ROW 16.48 COL 17 COLON-ALIGNED WIDGET-ID 104
     PROPSL.PROPSL# AT ROW 16.95 COL 113 COLON-ALIGNED WIDGET-ID 58
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     PROPSL.ACTIVE AT ROW 17.67 COL 85 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     FillExtension AT ROW 17.91 COL 17 COLON-ALIGNED WIDGET-ID 106
     PROPSL.ADDR1 AT ROW 19.57 COL 113 COLON-ALIGNED WIDGET-ID 6
          LABEL "Street"
          VIEW-AS FILL-IN 
          SIZE 36 BY 1
     PROPSL.ADDR2 AT ROW 20.57 COL 113 COLON-ALIGNED WIDGET-ID 8
          LABEL "Address2"
          VIEW-AS FILL-IN 
          SIZE 36 BY 1
     PROPSL.ADDR3 AT ROW 21.57 COL 113 COLON-ALIGNED WIDGET-ID 10
          LABEL "City"
          VIEW-AS FILL-IN 
          SIZE 36 BY 1
     PROPSL.ADDR4 AT ROW 22.57 COL 113 COLON-ALIGNED WIDGET-ID 12
          LABEL "Contact 1"
          VIEW-AS FILL-IN 
          SIZE 36 BY 1
     PROPSL.JANITOR AT ROW 23 COL 17 COLON-ALIGNED WIDGET-ID 38
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 208.6 BY 31.14
         BGCOLOR 8  WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FrameProposal
     PROPSL.ADDR5 AT ROW 23.57 COL 113 COLON-ALIGNED WIDGET-ID 14
          LABEL "Contact 2"
          VIEW-AS FILL-IN 
          SIZE 36 BY 1
     PROPSL.L-COMMENTS AT ROW 24 COL 17 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     PROPSL.L-NAME AT ROW 25 COL 17 COLON-ALIGNED WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     PROPSL.COMP# AT ROW 25.29 COL 112 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     PROPSL.L-STATE AT ROW 26 COL 17 COLON-ALIGNED WIDGET-ID 44
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     PROPSL.L-TELE AT ROW 26.95 COL 15 COLON-ALIGNED WIDGET-ID 46
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     PROPSL.L-TELE2 AT ROW 28 COL 17 COLON-ALIGNED WIDGET-ID 48
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     ButtonUpdate AT ROW 28.38 COL 91 WIDGET-ID 76
     ButtonSave AT ROW 28.38 COL 108 WIDGET-ID 78
     ButtonCancel AT ROW 28.38 COL 125 WIDGET-ID 94
     ButtonClose AT ROW 28.38 COL 142 WIDGET-ID 108
     PROPSL.LADDR01 AT ROW 29 COL 17 COLON-ALIGNED WIDGET-ID 50
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     PROPSL.LADDR02 AT ROW 30 COL 17 COLON-ALIGNED WIDGET-ID 52
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 208.6 BY 31.14
         BGCOLOR 8  WIDGET-ID 100.


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
         HEIGHT             = 31.14
         WIDTH              = 208.6
         MAX-HEIGHT         = 31.14
         MAX-WIDTH          = 208.6
         VIRTUAL-HEIGHT     = 31.14
         VIRTUAL-WIDTH      = 208.6
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
/* SETTINGS FOR FRAME FrameProposal
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN PROPSL.ADDR1 IN FRAME FrameProposal
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN PROPSL.ADDR2 IN FRAME FrameProposal
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN PROPSL.ADDR3 IN FRAME FrameProposal
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN PROPSL.ADDR4 IN FRAME FrameProposal
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN PROPSL.ADDR5 IN FRAME FrameProposal
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR BUTTON ButtonCancel IN FRAME FrameProposal
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON ButtonSave IN FRAME FrameProposal
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON ButtonUpdate IN FRAME FrameProposal
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FillAddr1 IN FRAME FrameProposal
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FillAddr2 IN FRAME FrameProposal
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FillAddr3 IN FRAME FrameProposal
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FillAddr4 IN FRAME FrameProposal
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FillAddr5 IN FRAME FrameProposal
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FillCust# IN FRAME FrameProposal
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FillCustName IN FRAME FrameProposal
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FillExtension IN FRAME FrameProposal
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FillFax IN FRAME FrameProposal
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FillState IN FRAME FrameProposal
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FillState-2 IN FRAME FrameProposal
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FillTelephone IN FRAME FrameProposal
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FillZip IN FRAME FrameProposal
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FrameProposal
/* Query rebuild information for FRAME FrameProposal
     _TblList          = "psg.PROPSL"
     _Query            is OPENED
*/  /* FRAME FrameProposal */
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


&Scoped-define SELF-NAME ButtonCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ButtonCancel C-Win
ON CHOOSE OF ButtonCancel IN FRAME FrameProposal /* Cancel */
DO:
  FIND CURRENT Propsl NO-LOCK NO-ERROR.
  IF AVAILABLE(Propsl) THEN RUN DisplayProposal.
  RUN DisableWidgets.
  DISABLE ButtonSave ButtonCancel WITH FRAME {&FRAME-NAME}.
  ENABLE ButtonUpdate SearchProposal WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ButtonClose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ButtonClose C-Win
ON CHOOSE OF ButtonClose IN FRAME FrameProposal /* Close */
DO:
  RUN DisableWidgets.
  APPLY "Close" TO {&WINDOW-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ButtonSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ButtonSave C-Win
ON CHOOSE OF ButtonSave IN FRAME FrameProposal /* Save */
DO:
  /*
  FIND CURRENT Propsl EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVALABLE(Propsl) THEN DO:
      MESSAGE "Record locked.  Wait or cancel?"
       VIEW-AS ALERT-BOX BUTTONS OK-CANCEL UPDATE Response.
      CASE Response.
        WHEN TRUE :
      END CASE.
  END.
  */
  ASSIGN
   PROPSL.ADDR1  = FillAddr1:SCREEN-VALUE 
   PROPSL.ADDR2  = FillAddr2:SCREEN-VALUE
   PROPSL.ADDR3  = FillAddr3:SCREEN-VALUE
   PROPSL.ADDR4  = FillAddr4:SCREEN-VALUE
   PROPSL.ADDR5  = FillAddr5:SCREEN-VALUE
   PROPSL.C-NAME = FillCustName:SCREEN-VALUE
   propsl.State  = FillState:SCREEN-VALUE
   Propsl.Zip    = FillZip:SCREEN-VALUE
   Propsl.Tele   = FillTelephone:SCREEN-VALUE
   Propsl.Fax    = FillFax:SCREEN-VALUE
   Propsl.Email  = FillExtension:SCREEN-VALUE
   .
  RUN DisableWidgets.
  DISABLE ButtonSave  ButtonCancel WITH FRAME {&FRAME-NAME}.
  ENABLE ButtonUpdate SearchProposal WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ButtonUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ButtonUpdate C-Win
ON CHOOSE OF ButtonUpdate IN FRAME FrameProposal /* Update */
DO:
  FIND CURRENT Propsl EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
  IF NOT AVAILABLE(Propsl) THEN DO: 
  
      MESSAGE "Proposal is in use pleae try later" ERROR-STATUS:GET-MESSAGE( 3 )
       VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
  END.
  RUN EnableWidgets.
  
  ENABLE ButtonSave ButtonCancel WITH FRAME {&frame-name}.
  DISABLE ButtonUpdate SearchProposal WITH FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SearchProposal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SearchProposal C-Win
ON LEAVE OF SearchProposal IN FRAME FrameProposal /* Proposal Number */
DO:
   RUN DisplayProposal.
     END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisableWidgets C-Win 
PROCEDURE DisableWidgets :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DISABLE
   FillAddr1 
   FillAddr2 
   FillAddr3 
   FillAddr4 
   FillAddr5 
   FillCustName 
   FillState
   FillZip
   FillTelephone
   FillFax
   FillExtension
    WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayProposal C-Win 
PROCEDURE DisplayProposal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND FIRST Propsl WHERE 
    Propsl.Propsl# = INTEGER(SearchProposal:SCREEN-VALUE IN FRAME {&FRAME-NAME})
    NO-LOCK NO-ERROR.
    
   IF NOT AVAILABLE(Propsl) THEN DO: .
      MESSAGE "No Proposal found for " SearchProposal:SCREEN-VALUE.
      RETURN NO-APPLY.
END.
   ASSIGN
   FillCust#:SCREEN-VALUE IN FRAME {&frame-name}    = STRING(Propsl.Cust#) 
   FillCustName:SCREEN-VALUE  =  Propsl.C-Name
   FillAddr1:SCREEN-VALUE     = Propsl.Addr1
   FillAddr2:SCREEN-VALUE     = Propsl.Addr2
   FillAddr3:SCREEN-VALUE     = Propsl.Addr3
   FillAddr4:SCREEN-VALUE     = Propsl.Addr4
   FillAddr5:SCREEN-VALUE     = Propsl.Addr5
   FillState:SCREEN-VALUE     = Propsl.State
   FillZip:SCREEN-VALUE       = Propsl.Zip
   FillTelephone:SCREEN-VALUE = Propsl.Tele
   FillFax:SCREEN-VALUE       = Propsl.Fax  
   FillExtension:SCREEN-VALUE = Propsl.Email
   .
   ENABLE ButtonUpdate WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnableWidgets C-Win 
PROCEDURE EnableWidgets :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ENABLE
   FillCustName
   FillAddr1 
   FillAddr2 
   FillAddr3 
   FillAddr4 
   FillAddr5 
   FillState
   FillZip
   FillTelephone
   FillFax
   FillExtension
    WITH FRAME {&FRAME-NAME}.
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

  {&OPEN-QUERY-FrameProposal}
  GET FIRST FrameProposal.
  DISPLAY SearchProposal FillCust# FillCustName FillAddr1 FillState-2 FillAddr2 
          FillAddr3 FillAddr4 FillAddr5 FillState FillZip FillTelephone FillFax 
          FillExtension 
      WITH FRAME FrameProposal IN WINDOW C-Win.
  IF AVAILABLE PROPSL THEN 
    DISPLAY PROPSL.LADDR03 PROPSL.CUST-TRF PROPSL.MLABEL PROPSL.DATE-P PROPSL.DIV# 
          PROPSL.ROUTE# PROPSL.EMAIL PROPSL.StartDate PROPSL.email1 
          PROPSL.email2 PROPSL.SUB# PROPSL.EndDate PROPSL.TOT-AMT PROPSL.PROPSL# 
          PROPSL.ACTIVE PROPSL.ADDR1 PROPSL.ADDR2 PROPSL.ADDR3 PROPSL.ADDR4 
          PROPSL.JANITOR PROPSL.ADDR5 PROPSL.L-COMMENTS PROPSL.L-NAME 
          PROPSL.COMP# PROPSL.L-STATE PROPSL.L-TELE PROPSL.L-TELE2 
          PROPSL.LADDR01 PROPSL.LADDR02 
      WITH FRAME FrameProposal IN WINDOW C-Win.
  ENABLE SearchProposal PROPSL.LADDR03 PROPSL.CUST-TRF PROPSL.MLABEL 
         PROPSL.DATE-P PROPSL.DIV# PROPSL.ROUTE# PROPSL.EMAIL PROPSL.StartDate 
         PROPSL.email1 PROPSL.email2 PROPSL.SUB# PROPSL.EndDate PROPSL.TOT-AMT 
         PROPSL.PROPSL# PROPSL.ACTIVE PROPSL.JANITOR PROPSL.L-COMMENTS 
         PROPSL.L-NAME PROPSL.COMP# PROPSL.L-STATE PROPSL.L-TELE PROPSL.L-TELE2 
         ButtonClose PROPSL.LADDR01 PROPSL.LADDR02 
      WITH FRAME FrameProposal IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FrameProposal}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

