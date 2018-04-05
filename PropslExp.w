&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 Character
&ANALYZE-RESUME
/* Connected Databases 
          psg              PROGRESS
*/
&Scoped-define WINDOW-NAME ttyWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS ttyWin 
/*  PropslExp.p */

/* Reports on upcoming propsal expirations */

/*****************************************/
/*  4/11/2017  TO initial implementation */
/*****************************************/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE SHARED VARIABLE XCOM AS INTEGER FORMAT "ZZ".
DEFINE SHARED VARIABLE XDIV AS INTEGER FORMAT "ZZ".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrowsePropsl

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES PROPSL

/* Definitions for BROWSE BrowsePropsl                                  */
&Scoped-define FIELDS-IN-QUERY-BrowsePropsl PROPSL.CUST# PROPSL.C-NAME ~
PROPSL.PROPSL# PROPSL.StartDate PROPSL.EndDate 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrowsePropsl 
&Scoped-define QUERY-STRING-BrowsePropsl FOR EACH PROPSL NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrowsePropsl OPEN QUERY BrowsePropsl FOR EACH PROPSL NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrowsePropsl PROPSL
&Scoped-define FIRST-TABLE-IN-QUERY-BrowsePropsl PROPSL


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BrowsePropsl}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FillLookAhead FillLookBack BrowsePropsl 
&Scoped-Define DISPLAYED-OBJECTS FillLookAhead FillLookBack 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR ttyWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE FillLookAhead AS INTEGER FORMAT ">>9":U INITIAL 60 
     LABEL "Days Ahead" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE FillLookBack AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Days Back" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1 &ENDIF NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrowsePropsl FOR 
      PROPSL SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrowsePropsl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrowsePropsl ttyWin _STRUCTURED
  QUERY BrowsePropsl NO-LOCK DISPLAY
      PROPSL.CUST# FORMAT "ZZZZZZZZZZ":U
      PROPSL.C-NAME FORMAT "X(25)":U
      PROPSL.PROPSL# FORMAT "ZZZZZZZZZZ":U
      PROPSL.StartDate FORMAT "99/99/9999":U
      PROPSL.EndDate FORMAT "99/99/9999":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 71 BY 20 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FillLookAhead
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 17 COLON-ALIGNED
          &ELSE AT ROW 2.99 COL 17 COLON-ALIGNED &ENDIF
     FillLookBack
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 43 COLON-ALIGNED
          &ELSE AT ROW 2.99 COL 43 COLON-ALIGNED &ENDIF
     BrowsePropsl
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 8
          &ELSE AT ROW 6 COL 8 &ENDIF
     "Ctrl-P to print" VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 19 BY 1
          &ELSE SIZE 19 BY 1 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 59
          &ELSE AT ROW 2.99 COL 59 &ENDIF
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 30.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW ttyWin ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 30.36
         WIDTH              = 80.43
         MAX-HEIGHT         = 30.36
         MAX-WIDTH          = 80.43
         VIRTUAL-HEIGHT     = 30.36
         VIRTUAL-WIDTH      = 80.43
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = yes
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BrowsePropsl FillLookBack DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(ttyWin)
THEN ttyWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrowsePropsl
/* Query rebuild information for BROWSE BrowsePropsl
     _TblList          = "psg.PROPSL"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = psg.PROPSL.CUST#
     _FldNameList[2]   = psg.PROPSL.C-NAME
     _FldNameList[3]   = psg.PROPSL.PROPSL#
     _FldNameList[4]   = psg.PROPSL.StartDate
     _FldNameList[5]   = psg.PROPSL.EndDate
     _Query            is OPENED
*/  /* BROWSE BrowsePropsl */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME ttyWin
&Scoped-define SELF-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME FillLookAhead
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FillLookAhead ttyWin
ON LEAVE OF FillLookAhead IN FRAME DEFAULT-FRAME /* Days Ahead */
DO:
  RUN OpenPropslQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FillLookBack
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FillLookBack ttyWin
ON LEAVE OF FillLookBack IN FRAME DEFAULT-FRAME /* Days Back */
DO:
  RUN OpenPropslQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BrowsePropsl
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK ttyWin 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   
  RUN enable_UI.
  RUN OpenPropslQuery.
  ON CTRL-P ANYWHERE RUN PrintList.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI ttyWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME DEFAULT-FRAME.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI ttyWin  _DEFAULT-ENABLE
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
  DISPLAY FillLookAhead FillLookBack 
      WITH FRAME DEFAULT-FRAME IN WINDOW ttyWin.
  ENABLE FillLookAhead FillLookBack BrowsePropsl 
      WITH FRAME DEFAULT-FRAME IN WINDOW ttyWin.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW ttyWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenPropslQuery ttyWin 
PROCEDURE OpenPropslQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   OPEN QUERY BrowsePropsl 
     FOR EACH Propsl WHERE
       /*PRO-DESP.COMP# = xcom AND
       PRO-DESP.DIV#  = xdiv AND*/
       Propsl.EndDate GE TODAY - INTEGER(FillLookBack:SCREEN-VALUE IN FRAME {&FRAME-NAME}) AND
       Propsl.EndDate LE TODAY + INTEGER(FillLookAhead:SCREEN-VALUE)
       NO-LOCK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintList ttyWin 
PROCEDURE PrintList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
message "p" view-as alert-box.
   OUTPUT TO printer.
   FOR EACH Propsl WHERE
       Propsl.EndDate GE TODAY - INTEGER(FillLookBack:SCREEN-VALUE IN FRAME {&FRAME-NAME}) AND
       Propsl.EndDate LE TODAY + INTEGER(FillLookAhead:SCREEN-VALUE)
       NO-LOCK:
         DISPLAY
            Propsl.Cust#
            Propsl.C-Name
            Propsl.L-Name
            Propsl.Propsl#
            Propsl.StartDate
            Propsl.EndDate.
    END. 
    OUTPUT CLOSE.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

