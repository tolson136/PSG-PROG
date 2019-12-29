&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          psg              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"smartproposal/dproposal.i"}.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS vTable 
/*------------------------------------------------------------------------

  File:

  Description: from viewer.w - Template for SmartDataViewer objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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

{src/adm2/widgetprto.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "smartproposal/dproposal.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.CUST# RowObject.COMP# ~
RowObject.DIV# RowObject.PROPSL# RowObject.ROUTE# RowObject.C-NAME ~
RowObject.L-NAME RowObject.SUB# RowObject.ADDR1 RowObject.LADDR01 ~
RowObject.ADDR2 RowObject.LADDR02 RowObject.ADDR3 RowObject.LADDR03 ~
RowObject.ADDR4 RowObject.ADDR5 RowObject.L-STATE RowObject.L-TELE ~
RowObject.L-TELE2 RowObject.STATE RowObject.ZIP RowObject.TELE ~
RowObject.L-COMMENTS RowObject.FAX-TELE RowObject.TOT-AMT RowObject.EMAIL ~
RowObject.DATE-P RowObject.JANITOR RowObject.CUST-TRF RowObject.MLABEL ~
RowObject.ACTIVE RowObject.email1 RowObject.email2 RowObject.StartDate ~
RowObject.EndDate 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define DISPLAYED-FIELDS RowObject.CUST# RowObject.COMP# ~
RowObject.DIV# RowObject.PROPSL# RowObject.ROUTE# RowObject.C-NAME ~
RowObject.L-NAME RowObject.SUB# RowObject.ADDR1 RowObject.LADDR01 ~
RowObject.ADDR2 RowObject.LADDR02 RowObject.ADDR3 RowObject.LADDR03 ~
RowObject.ADDR4 RowObject.ADDR5 RowObject.L-STATE RowObject.L-TELE ~
RowObject.L-TELE2 RowObject.STATE RowObject.ZIP RowObject.TELE ~
RowObject.L-COMMENTS RowObject.FAX-TELE RowObject.TOT-AMT RowObject.EMAIL ~
RowObject.DATE-P RowObject.JANITOR RowObject.CUST-TRF RowObject.MLABEL ~
RowObject.ACTIVE RowObject.email1 RowObject.email2 RowObject.StartDate ~
RowObject.EndDate 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.CUST# AT ROW 1.48 COL 21 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     RowObject.COMP# AT ROW 1.48 COL 75 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     RowObject.DIV# AT ROW 1.48 COL 90 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     RowObject.PROPSL# AT ROW 1.48 COL 127 COLON-ALIGNED WIDGET-ID 56
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.ROUTE# AT ROW 1.48 COL 167 COLON-ALIGNED WIDGET-ID 58
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     RowObject.C-NAME AT ROW 2.91 COL 21 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     RowObject.L-NAME AT ROW 3.14 COL 98 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     RowObject.SUB# AT ROW 3.14 COL 167 COLON-ALIGNED WIDGET-ID 64
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     RowObject.ADDR1 AT ROW 4.57 COL 21 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     RowObject.LADDR01 AT ROW 4.57 COL 98 COLON-ALIGNED WIDGET-ID 48
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     RowObject.ADDR2 AT ROW 5.57 COL 21 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     RowObject.LADDR02 AT ROW 5.76 COL 98 COLON-ALIGNED WIDGET-ID 50
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     RowObject.ADDR3 AT ROW 6.57 COL 21 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     RowObject.LADDR03 AT ROW 7.19 COL 98 COLON-ALIGNED WIDGET-ID 52
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     RowObject.ADDR4 AT ROW 7.57 COL 21 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     RowObject.ADDR5 AT ROW 8.57 COL 21 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     RowObject.L-STATE AT ROW 8.62 COL 98 COLON-ALIGNED WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     RowObject.L-TELE AT ROW 8.62 COL 118 COLON-ALIGNED WIDGET-ID 44
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     RowObject.L-TELE2 AT ROW 9.81 COL 98 COLON-ALIGNED WIDGET-ID 46
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     RowObject.STATE AT ROW 10.05 COL 21 COLON-ALIGNED WIDGET-ID 62
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     RowObject.ZIP AT ROW 10.05 COL 32 COLON-ALIGNED WIDGET-ID 70
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.TELE AT ROW 11.24 COL 21 COLON-ALIGNED WIDGET-ID 66
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     RowObject.L-COMMENTS AT ROW 11.24 COL 98 COLON-ALIGNED WIDGET-ID 38
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     RowObject.FAX-TELE AT ROW 12.43 COL 21 COLON-ALIGNED WIDGET-ID 34
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     RowObject.TOT-AMT AT ROW 12.67 COL 98 COLON-ALIGNED WIDGET-ID 68
          VIEW-AS FILL-IN 
          SIZE 24.4 BY 1
     RowObject.EMAIL AT ROW 13.62 COL 21 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     RowObject.DATE-P AT ROW 14.1 COL 98 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RowObject.JANITOR AT ROW 14.81 COL 21 COLON-ALIGNED WIDGET-ID 36
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     RowObject.CUST-TRF AT ROW 15.52 COL 98 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     RowObject.MLABEL AT ROW 16 COL 21 COLON-ALIGNED WIDGET-ID 54
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     RowObject.ACTIVE AT ROW 16.95 COL 98 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     RowObject.email1 AT ROW 17.43 COL 21 COLON-ALIGNED WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     RowObject.email2 AT ROW 18.86 COL 21 COLON-ALIGNED WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     RowObject.StartDate AT ROW 20.29 COL 21 COLON-ALIGNED WIDGET-ID 60
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.EndDate AT ROW 20.29 COL 51 COLON-ALIGNED WIDGET-ID 32
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "smartproposal\dproposal.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {smartproposal/dproposal.i}
      END-FIELDS.
   END-TABLES.
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW vTable ASSIGN
         HEIGHT             = 23.52
         WIDTH              = 210.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB vTable 
/* ************************* Included-Libraries *********************** */

{src/adm2/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW vTable
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK vTable 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI vTable  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

