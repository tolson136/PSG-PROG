&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          psg              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS dTables 
/*------------------------------------------------------------------------

  File:  

  Description: from DATA.W - Template For SmartData objects in the ADM

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Modified:     February 24, 1999
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

&glob DATA-LOGIC-PROCEDURE .p

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataObject
&Scoped-define DB-AWARE yes

&Scoped-define ADM-SUPPORTED-LINKS Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target


/* Db-Required definitions. */
&IF DEFINED(DB-REQUIRED) = 0 &THEN
    &GLOBAL-DEFINE DB-REQUIRED TRUE
&ENDIF
&GLOBAL-DEFINE DB-REQUIRED-START   &IF {&DB-REQUIRED} &THEN
&GLOBAL-DEFINE DB-REQUIRED-END     &ENDIF


&Scoped-define QUERY-NAME Query-Main

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES PROPSL

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  ACTIVE ADDR1 ADDR2 ADDR3 ADDR4 ADDR5 C-NAME COMP# CUST# CUST-TRF DATE-P~
 DIV# EMAIL email1 email2 EndDate FAX-TELE JANITOR L-COMMENTS L-NAME L-STATE~
 L-TELE L-TELE2 LADDR01 LADDR02 LADDR03 MLABEL PROPSL# ROUTE# StartDate~
 STATE SUB# TELE TOT-AMT ZIP
&Scoped-define ENABLED-FIELDS-IN-PROPSL ACTIVE ADDR1 ADDR2 ADDR3 ADDR4 ~
ADDR5 C-NAME COMP# CUST# CUST-TRF DATE-P DIV# EMAIL email1 email2 EndDate ~
FAX-TELE JANITOR L-COMMENTS L-NAME L-STATE L-TELE L-TELE2 LADDR01 LADDR02 ~
LADDR03 MLABEL PROPSL# ROUTE# StartDate STATE SUB# TELE TOT-AMT ZIP 
&Scoped-Define DATA-FIELDS  ACTIVE ADDR1 ADDR2 ADDR3 ADDR4 ADDR5 C-NAME COMP# CUST# CUST-TRF DATE-P~
 DIV# EMAIL email1 email2 EndDate FAX-TELE JANITOR L-COMMENTS L-NAME L-STATE~
 L-TELE L-TELE2 LADDR01 LADDR02 LADDR03 MLABEL PROPSL# ROUTE# StartDate~
 STATE SUB# TELE TOT-AMT ZIP
&Scoped-define DATA-FIELDS-IN-PROPSL ACTIVE ADDR1 ADDR2 ADDR3 ADDR4 ADDR5 ~
C-NAME COMP# CUST# CUST-TRF DATE-P DIV# EMAIL email1 email2 EndDate ~
FAX-TELE JANITOR L-COMMENTS L-NAME L-STATE L-TELE L-TELE2 LADDR01 LADDR02 ~
LADDR03 MLABEL PROPSL# ROUTE# StartDate STATE SUB# TELE TOT-AMT ZIP 
&Scoped-Define MANDATORY-FIELDS  PROPSL#
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "SmartProposal/dProposal.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH PROPSL NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH PROPSL NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main PROPSL
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main PROPSL


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      PROPSL SCROLLING.
&ANALYZE-RESUME
{&DB-REQUIRED-END}


/* ************************  Frame Definitions  *********************** */


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataObject
   Allow: Query
   Frames: 0
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER DB-AWARE
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
  CREATE WINDOW dTables ASSIGN
         HEIGHT             = 1.62
         WIDTH              = 46.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB dTables 
/* ************************* Included-Libraries *********************** */

{src/adm2/data.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW dTables
  VISIBLE,,RUN-PERSISTENT                                               */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY Query-Main
/* Query rebuild information for SmartDataObject Query-Main
     _TblList          = "psg.PROPSL"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > psg.PROPSL.ACTIVE
"ACTIVE" "ACTIVE" ? ? "character" ? ? ? ? ? ? yes ? no 7.6 yes ?
     _FldNameList[2]   > psg.PROPSL.ADDR1
"ADDR1" "ADDR1" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes ?
     _FldNameList[3]   > psg.PROPSL.ADDR2
"ADDR2" "ADDR2" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes ?
     _FldNameList[4]   > psg.PROPSL.ADDR3
"ADDR3" "ADDR3" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes ?
     _FldNameList[5]   > psg.PROPSL.ADDR4
"ADDR4" "ADDR4" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes ?
     _FldNameList[6]   > psg.PROPSL.ADDR5
"ADDR5" "ADDR5" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes ?
     _FldNameList[7]   > psg.PROPSL.C-NAME
"C-NAME" "C-NAME" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes ?
     _FldNameList[8]   > psg.PROPSL.COMP#
"COMP#" "COMP#" ? ? "integer" ? ? ? ? ? ? yes ? no 7.6 yes ?
     _FldNameList[9]   > psg.PROPSL.CUST#
"CUST#" "CUST#" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[10]   > psg.PROPSL.CUST-TRF
"CUST-TRF" "CUST-TRF" ? ? "logical" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[11]   > psg.PROPSL.DATE-P
"DATE-P" "DATE-P" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ?
     _FldNameList[12]   > psg.PROPSL.DIV#
"DIV#" "DIV#" ? ? "integer" ? ? ? ? ? ? yes ? no 5 yes ?
     _FldNameList[13]   > psg.PROPSL.EMAIL
"EMAIL" "EMAIL" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes ?
     _FldNameList[14]   > psg.PROPSL.email1
"email1" "email1" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes ?
     _FldNameList[15]   > psg.PROPSL.email2
"email2" "email2" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes ?
     _FldNameList[16]   > psg.PROPSL.EndDate
"EndDate" "EndDate" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ?
     _FldNameList[17]   > psg.PROPSL.FAX-TELE
"FAX-TELE" "FAX-TELE" ? ? "character" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[18]   > psg.PROPSL.JANITOR
"JANITOR" "JANITOR" ? ? "logical" ? ? ? ? ? ? yes ? no 9.2 yes ?
     _FldNameList[19]   > psg.PROPSL.L-COMMENTS
"L-COMMENTS" "L-COMMENTS" ? ? "character" ? ? ? ? ? ? yes ? no 14.2 yes ?
     _FldNameList[20]   > psg.PROPSL.L-NAME
"L-NAME" "L-NAME" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes ?
     _FldNameList[21]   > psg.PROPSL.L-STATE
"L-STATE" "L-STATE" ? ? "character" ? ? ? ? ? ? yes ? no 8.8 yes ?
     _FldNameList[22]   > psg.PROPSL.L-TELE
"L-TELE" "L-TELE" ? ? "character" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[23]   > psg.PROPSL.L-TELE2
"L-TELE2" "L-TELE2" ? ? "character" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[24]   > psg.PROPSL.LADDR01
"LADDR01" "LADDR01" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes ?
     _FldNameList[25]   > psg.PROPSL.LADDR02
"LADDR02" "LADDR02" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes ?
     _FldNameList[26]   > psg.PROPSL.LADDR03
"LADDR03" "LADDR03" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes ?
     _FldNameList[27]   > psg.PROPSL.MLABEL
"MLABEL" "MLABEL" ? ? "logical" ? ? ? ? ? ? yes ? no 8.4 yes ?
     _FldNameList[28]   > psg.PROPSL.PROPSL#
"PROPSL#" "PROPSL#" ? ? "decimal" ? ? ? ? ? ? yes ? yes 14 yes ?
     _FldNameList[29]   > psg.PROPSL.ROUTE#
"ROUTE#" "ROUTE#" ? ? "integer" ? ? ? ? ? ? yes ? no 9 yes ?
     _FldNameList[30]   > psg.PROPSL.StartDate
"StartDate" "StartDate" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ?
     _FldNameList[31]   > psg.PROPSL.STATE
"STATE" "STATE" ? ? "character" ? ? ? ? ? ? yes ? no 7 yes ?
     _FldNameList[32]   > psg.PROPSL.SUB#
"SUB#" "SUB#" ? ? "integer" ? ? ? ? ? ? yes ? no 5.8 yes ?
     _FldNameList[33]   > psg.PROPSL.TELE
"TELE" "TELE" ? ? "character" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[34]   > psg.PROPSL.TOT-AMT
"TOT-AMT" "TOT-AMT" ? ? "decimal" ? ? ? ? ? ? yes ? no 16.8 yes ?
     _FldNameList[35]   > psg.PROPSL.ZIP
"ZIP" "ZIP" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ?
     _Design-Parent    is WINDOW dTables @ ( 1.14 , 2.6 )
*/  /* QUERY Query-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK dTables 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI dTables  _DEFAULT-DISABLE
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
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

