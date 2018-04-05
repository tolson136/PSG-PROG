&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 Character
&ANALYZE-RESUME
/* Connected Databases 
          psg              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME ttyDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS ttyDialog 
/*------------------------------------------------------------------------

  File:   custsearch.w 

  Description: Searches within all name and address fields
               Returns propsl#

  Input Parameters:
      

  Output Parameters:
      <none>

  Author: 

  Created: 02/21/95 -  4:18 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER SearchPropsl# LIKE propsl.propsl#.
/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME ttyDialog
&Scoped-define BROWSE-NAME BrowseSearch

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES BigSearch PROPSL PRO-DESP

/* Definitions for BROWSE BrowseSearch                                  */
&Scoped-define FIELDS-IN-QUERY-BrowseSearch BigSearch.PROPSL# PROPSL.C-NAME ~
PROPSL.L-NAME BigSearch.Val BigSearch.Fld 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrowseSearch 
&Scoped-define QUERY-STRING-BrowseSearch FOR EACH BigSearch NO-LOCK, ~
      EACH PROPSL WHERE BigSearch.PROPSL# = propsl.propsl# NO-LOCK, ~
      EACH PRO-DESP OF PROPSL NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrowseSearch OPEN QUERY BrowseSearch FOR EACH BigSearch NO-LOCK, ~
      EACH PROPSL WHERE BigSearch.PROPSL# = propsl.propsl# NO-LOCK, ~
      EACH PRO-DESP OF PROPSL NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrowseSearch BigSearch PROPSL PRO-DESP
&Scoped-define FIRST-TABLE-IN-QUERY-BrowseSearch BigSearch
&Scoped-define SECOND-TABLE-IN-QUERY-BrowseSearch PROPSL
&Scoped-define THIRD-TABLE-IN-QUERY-BrowseSearch PRO-DESP


/* Definitions for DIALOG-BOX ttyDialog                                 */
&Scoped-define OPEN-BROWSERS-IN-QUERY-ttyDialog ~
    ~{&OPEN-QUERY-BrowseSearch}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FillSearch BrowseSearch Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS FillSearch 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO DEFAULT 
     LABEL "OK" 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 6 BY 1
     &ELSE SIZE 6 BY 1 &ENDIF.

DEFINE VARIABLE FillSearch AS CHARACTER FORMAT "X(50)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 29 BY 1
     &ELSE SIZE 29 BY 1 &ENDIF NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrowseSearch FOR 
      BigSearch, 
      PROPSL, 
      PRO-DESP SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrowseSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrowseSearch ttyDialog _STRUCTURED
  QUERY BrowseSearch NO-LOCK DISPLAY
      BigSearch.PROPSL# FORMAT "ZZZZZZZZZZ":U
      PROPSL.C-NAME FORMAT "X(20)":U
      PROPSL.L-NAME FORMAT "X(20)":U
      BigSearch.Val FORMAT "x(20)":U
      BigSearch.Fld FORMAT "x(12)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 96 BY 20 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME ttyDialog
     FillSearch
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 1
          &ELSE AT ROW 1 COL 1 &ENDIF
     BrowseSearch
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 4 COL 2
          &ELSE AT ROW 4 COL 2 &ENDIF
     Btn_OK
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 36 COL 8
          &ELSE AT ROW 36 COL 8 &ENDIF
     SPACE(86.00) SKIP(4.00)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
        TITLE "Mega Search"
         DEFAULT-BUTTON Btn_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX ttyDialog
   FRAME-NAME                                                           */
/* BROWSE-TAB BrowseSearch FillSearch ttyDialog */
ASSIGN 
       FRAME ttyDialog:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN FillSearch IN FRAME ttyDialog
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrowseSearch
/* Query rebuild information for BROWSE BrowseSearch
     _TblList          = "psg.BigSearch,psg.PROPSL WHERE psg.BigSearch ... ...,psg.PRO-DESP OF psg.PROPSL"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _JoinCode[2]      = "BigSearch.PROPSL# = propsl.propsl#"
     _FldNameList[1]   = psg.BigSearch.PROPSL#
     _FldNameList[2]   > psg.PROPSL.C-NAME
"PROPSL.C-NAME" ? "X(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > psg.PROPSL.L-NAME
"PROPSL.L-NAME" ? "X(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > psg.BigSearch.Val
"BigSearch.Val" ? "x(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = psg.BigSearch.Fld
     _Query            is OPENED
*/  /* BROWSE BrowseSearch */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME ttyDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttyDialog ttyDialog
ON WINDOW-CLOSE OF FRAME ttyDialog /* Mega Search */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  ASSIGN SearchPropsl# = propsl.propsl#.
  /*RETURN.*/
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BrowseSearch
&Scoped-define SELF-NAME BrowseSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BrowseSearch ttyDialog
ON RETURN OF BrowseSearch IN FRAME ttyDialog
DO:
 ASSIGN SearchPropsl# = propsl.propsl#.
 APPLY "GO" to FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FillSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FillSearch ttyDialog
ON ANY-KEY OF FillSearch IN FRAME ttyDialog /* Search */
DO:
 /*if lastkey = 13 then apply "GO" to Btn_OK.*/
 OPEN QUERY BrowseSearch
   FOR EACH BigSearch WHERE
      BigSearch.Val begins FillSearch:SCREEN-VALUE IN FRAME {&FRAME-NAME},
       EACH Propsl WHERE Propsl.Propsl# = BigSearch.Propsl#   
       NO-LOCK,
         FIRST pro-desp WHERE pro-desp.propsl# = BigSearch.Propsl#
         NO-LOCK
       .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK ttyDialog 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
ASSIGN SearchPropsl# = propsl.propsl#.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI ttyDialog  _DEFAULT-DISABLE
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
  HIDE FRAME ttyDialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI ttyDialog  _DEFAULT-ENABLE
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
  DISPLAY FillSearch 
      WITH FRAME ttyDialog.
  ENABLE FillSearch BrowseSearch Btn_OK 
      WITH FRAME ttyDialog.
  {&OPEN-BROWSERS-IN-QUERY-ttyDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Search ttyDialog 
PROCEDURE Search :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /*OPEN QUERY BrowseCust
   FOR EACH Propsl WHERE
      IF FillNameSearch:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN
      ( propsl.c-name begins FillNameSearch:SCREEN-VALUE IN FRAME {&FRAME-NAME} OR
        propsl.l-name BEGINS FillNameSearch:SCREEN-VALUE
       ) AND
       (
         PROPSL.ADDR1 BEGINS FillAddressSearch:SCREEN-VALUE OR
         PROPSL.ADDR2 BEGINS FillAddressSearch:SCREEN-VALUE OR 
         PROPSL.ADDR3 BEGINS FillAddressSearch:SCREEN-VALUE OR 
         PROPSL.ADDR4 BEGINS FillAddressSearch:SCREEN-VALUE OR 
         PROPSL.ADDR5 BEGINS FillAddressSearch:SCREEN-VALUE OR 
         PROPSL.LADDR01 BEGINS FillAddressSearch:SCREEN-VALUE OR 
         PROPSL.LADDR02 BEGINS FillAddressSearch:SCREEN-VALUE OR 
         PROPSL.LADDR03 BEGINS FillAddressSearch:SCREEN-VALUE
        ) 
      NO-LOCK.
*/  
 /* 
  
  OPEN QUERY BrowseCust
   FOR EACH Propsl WHERE
      ( propsl.c-name  BEGINS FillSearch:SCREEN-VALUE IN FRAME {&FRAME-NAME} OR
        propsl.l-name  BEGINS FillSearch:SCREEN-VALUE OR
        PROPSL.ADDR1   BEGINS FillSearch:SCREEN-VALUE OR
        PROPSL.ADDR2   BEGINS FillSearch:SCREEN-VALUE OR 
        PROPSL.ADDR3   BEGINS FillSearch:SCREEN-VALUE OR 
        PROPSL.ADDR4   BEGINS FillSearch:SCREEN-VALUE OR 
        PROPSL.ADDR5   BEGINS FillSearch:SCREEN-VALUE OR 
        PROPSL.LADDR01 BEGINS FillSearch:SCREEN-VALUE OR 
        PROPSL.LADDR02 BEGINS FillSearch:SCREEN-VALUE OR 
        PROPSL.LADDR03 BEGINS FillSearch:SCREEN-VALUE
        ) 
      NO-LOCK.
*/      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

