/*::***************************************************************************/
/*::                                                                          */
/*::                             C O M B O . I                                */
/*::                                                                          */
/*::**************************************************************************/
/*::                                                                          */
/*::          Version: 3.05j                                                  */
/*::           Author: David A. Read                                          */
/*::    Creation Date: 25 June, 1994                                          */
/*::      Description: This include file is used to "intelligently" create    */
/*::                   combo boxes and other specific types of help function  */
/*::                   for any interactive Progress V7 application. The types */
/*::                   of help which are supported are as follows:            */
/*::                   1) List Selection; the traditional "combo box",        */
/*::                      displays allowed values from a validation table     */
/*::                      or other pre-defined list ("Lookup" or default)     */
/*::                   2) File Name; for character fields whose value must be */
/*::                      the name of a file on disk, displays the standard   */
/*::                      Windows file selection dialog box ("File")          */
/*::                   3) Calendar; for date fields, displays a pop-up        */
/*::                      calendar ("Date")                                   */
/*::                   4) Calculator; for numeric fields which require any    */
/*::                      type of algebraic calculation, displays a pop-up    */
/*::                      calculator "Calculation")                           */
/*::                   5) Color Palette; for integer fields whose value       */
/*::                      must correspond to an entry in the defined Color    */
/*::                      Palette, displays the standard Progress Color       */
/*::                      selection dialog box ("Color")                      */
/*::                   6) Font Table; for integer fields whose value must     */
/*::                      correspond to an entry in the defined Font Table,   */
/*::                      displays the standard Progress Font selection       */
/*::                      dialog box ("Font")                                 */
/*::                                                                          */
/*:===========================================================================*/
/*: MODIFICATION HISTORY                                                      */
/*:===========================================================================*/
/*: DATE      VERSION BY  DESCRIPTION                                         */
/*: --------- ------- --- --------------------------------------------------- */
/*: 06 APR 95 3.01a   DAR Substantial re-write of major sections to make use  */
/*:                       of the V7.3A capability to convert widget handles   */
/*:                       to strings and back to widget handles;              */
/*:                       Added "AllowInsensitive" preprocesor variable to    */
/*:                       allow combo boxes on insensitive fill-ins;          */
/*:                       Added "NextWidget" preprocessor variable to allow   */
/*:                       the developer to specify which widget should        */
/*:                       receive focus after a selection is made;            */
/*:                       Added "DisplayDelimiter" preprocessor variable to   */
/*:                       allow the developer to override the default comma   */
/*:                       delimiter for selection of multiple values.         */
/*: 26 APR 95 3.02a   DAR Added "&NoItemsMessage" argument by which the       */
/*:                       developer can specify a message to display if there */
/*:                       are no items to select from for the combo box.      */
/*: 12 MAY 95 3.02b   DAR Make sure that ROW and COLUMN of the selection list */
/*:                       are at least "1".  There still needs to be work     */
/*:                       done on size and position of the selection list.    */
/*: 19 MAY 95 3.02c   DAR Store MultipleSelect option with each combo box so  */
/*:                       that it works independently for different combo     */
/*:                       buttons.                                            */
/*: 08 JUN 95 3.02d   DAR Do not display duplicate selection list entries for */
/*:                       any combo box which is built from a database table. */
/*: 20 JUN 95 3.02e   DAR Reset the "BLANK" attribute of the selection        */
/*:                       fill-in to "no" when no values are available for    */
/*:                       the pull-down selection list.                       */
/*: 24 JAN 96 3.02f   DAR Use widget-handle strings in FillInList and         */
/*:                       ComboButtonList so that individual elements of      */
/*:                       array fields can be distinguished from each other   */
/*:                       (previously used the :NAME attribute, which is the  */
/*:                       same value for all elements of an array).           */
/*: 18 OCT 96 3.02g   DAR For MultipleSelect option, if original field value  */
/*:                       is unknown "?" then replace it, rather than append  */
/*:                       the selected value to the unknwon value.            */
/*: 07 DEC 96 3.02h   DAR In ON RETURN, MOUSE-SELECT-CLICK OF (SelectionList) */
/*:                       now use "SelectionIndex" variable to calculate and  */
/*:                       hold the selected row of the selection list.        */
/*: 06 MAY 98 3.02i   DAR In ON RETURN, MOUSE-SELECT-CLICK OF (SelectionList) */
/*:                       now forcing a LEAVE event on the selection list so  */
/*:                       that if some other processing (such as with smart   */
/*:                       objects) has caused focus to be on anoher widget,   */
/*:                       the LEAVE trigger will still fire.                  */
/*:                       Also, when building the selection list, ignore any  */
/*:                       items with unknown value (?) so that the entire     */
/*:                       selection list does not end up as unknown value.    */
/*: 17 OCT 98 3.02j   DAR The logical "EnteringCombo" was being properly set  */
/*:                       back to "no" after leaving drop-down list of        */
/*:                       "lookup" type combo (see 2.03c above), but was NOT  */
/*:                       being set for any other type of combo (file, font,  */
/*:                       etc.).  This has now been FIXED!                    */
/*: 07 DEC 98 3.03a   DAR Added ability to specify &InnerLines pre-processor  */
/*:                       variable to control maximum size of combo selection */
/*:                       list.                                               */
/*: 07 DEC 98 3.03b   DAR Forgot to specify preprocesor ENDIF in previous     */
/*:                       change.  Sorry Mike!                                */
/*: 09 AUG 99 3.04a   DAR Changed all references to PRIVATE-DATA to use the   */
/*:                       "GetPrivateData" and "SetPrivateData" routines in   */
/*:                       ClassUtilities.                                     */
/*: 09 AUG 99 3.04b   DAR Added more pixels to width of drop down selection   */
/*:                       widget in never-ending quest to eliminate horizon-  */
/*:                       tal scroll bars.                                    */
/*: 11 AUG 99 3.05a   DAR Added argument "FirstCharacterSearch" which sets    */
/*:                       corresponding fill-in to act like Progress combo    */
/*:                       box in that typing a printable character in the     */
/*:                       fill-in does a search (and display if found) for    */
/*:                       next defined selection element beginning with that  */
/*:                       character. For example, if selection list is:       */
/*:                       "Fred,Ferdinand,Fyodr" then typing "F"s will cause  */
/*:                       the following to be displayed in the fill-in:       */
/*:                         1st F: "Fred"                                     */
/*:                         2nd F: "Ferdinand"                                */
/*:                         3rd F: "Fyodr"                                    */
/*:                         4th F: "Fred" (back to first "F")                 */
/*:                       This method only works if there is a defined        */
/*:                       "DisplayString" or "SelectionString" which is a     */
/*:                       literal selection list string or a variable         */
/*:                       containing a selection list string.  Calculated or  */
/*:                       lookup lists CANNOT be used with this argument.     */
/*: 14 JAN 00 3.05b   DAR Changed name of variable "SearchString" to          */
/*:                       "ComboSearchString" to avoid naming conflict with   */
/*:                       programs in which this file is included.            */
/*: 15 MAR 00 3.05c   DAR Fixed Multiple Selection stuff which apparently got */
/*:                       messed up in 3.04a.                                 */
/*: 17 SEP 01 3.05d   DAR Added new "NO-FOCUS" option to combo button.        */
/*: 23 OCT 01 3.05e   DAR After running AfterSelection program, now apply     */
/*:                       "LEAVE" and then "ENTRY" events to the fill-in so   */
/*:                       that these triggers (which may do validations,      */
/*:                       lookups, etc.) will fire.                           */
/*: 04 DEC 01 3.05f   DAR Fixed previous change to directly call "Validate-   */
/*:                       Input" instead of doing LEAVE/ENTRY triggers, which */
/*:                       were cxausing other problems.                       */
/*: 11 JUL 03 3.05g   DAR Replaced calls to _setcurs.p with SESSION:SET-WAIT- */
/*:                       STATE in preparation for conversion to V9.          */
/*: 25 MAR 04 3.05h   DAR Added more room to bottom for selection list.       */
/*: 13 MAY 04 3.05i   DAR Variable amount of room left on bottom depending    */
/*:                       on frame height.                                    */
/*: 25 JUN 04 3.05j   DAR In File selection, now replace forward slashes with */
/*:                       back slashes to satisfy DOS type directory names.   */
/*::***************************************************************************/

/*----------------------------------------------------------------------------*/
/* Define variables, procedures, etc. which are to be defined only once (the  */
/* first time combo.i is referenced only):                                    */
/*----------------------------------------------------------------------------*/

&IF DEFINED( ComboButtonUsed ) = 0 &THEN

   &GLOBAL-DEFINE ComboButtonUsed           yes
   &GLOBAL-DEFINE DefaultSelectionFont      22
   &GLOBAL-DEFINE DefaultSelectionDelimiter `
   &GLOBAL-DEFINE DefaultDisplayDelimiter   ,

   DEFINE VARIABLE SelectionList             AS CHARACTER      NO-UNDO.
   DEFINE VARIABLE SelectedComboName         AS CHARACTER      NO-UNDO.
   DEFINE VARIABLE SelectedComboHandle       AS WIDGET-HANDLE  NO-UNDO.
   DEFINE VARIABLE AllowInsensitiveSelection AS LOGICAL        NO-UNDO.
   DEFINE VARIABLE SelectionNextWidget       AS WIDGET-HANDLE  NO-UNDO.
   DEFINE VARIABLE SelectionMultiple         AS LOGICAL        NO-UNDO.
   DEFINE VARIABLE FillWidget                AS WIDGET-HANDLE  NO-UNDO.
   DEFINE VARIABLE FillFrame                 AS WIDGET-HANDLE  NO-UNDO.
   DEFINE VARIABLE FillInList                AS CHARACTER      NO-UNDO.
   DEFINE VARIABLE ComboButtonList           AS CHARACTER      NO-UNDO.
   DEFINE VARIABLE FillInPointer             AS INTEGER        NO-UNDO.
   DEFINE VARIABLE ComboProcedure            AS CHARACTER      NO-UNDO.
   DEFINE VARIABLE AfterProcedure            AS CHARACTER      NO-UNDO.
   DEFINE VARIABLE SelectionWidget           AS WIDGET-HANDLE  NO-UNDO.
   DEFINE VARIABLE DidScroll                 AS LOGICAL        NO-UNDO.
   DEFINE VARIABLE ComboChange               AS LOGICAL        NO-UNDO.
   DEFINE VARIABLE StartScrollUp             AS LOGICAL        NO-UNDO.
   DEFINE VARIABLE LastSearchCharacter       AS CHARACTER      NO-UNDO INITIAL ?.
   DEFINE VARIABLE LastSearchPosition        AS INTEGER        NO-UNDO INITIAL 0.
   DEFINE VARIABLE SearchIndex               AS INTEGER        NO-UNDO.
   DEFINE VARIABLE SearchElement             AS CHARACTER      NO-UNDO.
   DEFINE VARIABLE ComboSearchString         AS CHARACTER      NO-UNDO.
   DEFINE VARIABLE SearchDelimiter           AS CHARACTER      NO-UNDO.
   DEFINE VARIABLE StartSearchPosition       AS INTEGER        NO-UNDO.
   DEFINE VARIABLE EndSearchPosition         AS INTEGER        NO-UNDO.
   DEFINE VARIABLE SelectionSection          AS INTEGER        NO-UNDO.
   DEFINE VARIABLE SelectionStartValue       AS CHARACTER      NO-UNDO.
   DEFINE VARIABLE EnteringCombo             AS LOGICAL        NO-UNDO INITIAL no.

   /*-------------------------------------------------------------------------*/
   /* Define the procedure which will invoke the combo processing procedure   */
   /* corresponding to the specified FILL_IN.  This procedure is designed to  */
   /* be called from outside combo.i so that combo processing may be done     */
   /* without having to pass "FOCUS" to the combo button:                     */
   /*-------------------------------------------------------------------------*/

   PROCEDURE ProcessComboButton:
      DEFINE INPUT PARAMETER ThisFillIn     AS WIDGET-HANDLE  NO-UNDO.
      DEFINE VARIABLE IsValid               AS LOGICAL        NO-UNDO.
      DEFINE VARIABLE PrivateString         AS CHARACTER      NO-UNDO.
      RUN GetComboButton ( INPUT ThisFillIn, OUTPUT SelectedComboName, OUTPUT SelectedComboHandle ).
      IF NOT VALID-HANDLE( SelectedComboHandle ) THEN DO:
         MESSAGE "ERROR: Could not process the combo box for this fill-in??" VIEW-AS ALERT-BOX ERROR.
         RETURN.
      END.
      RUN GetPrivateData IN UtilityProcedure (
       "FRAME-HANDLE", SelectedComboHandle, "SELF", OUTPUT PrivateString ).
      FillFrame = WIDGET-HANDLE( PrivateString ).
      RUN GetPrivateData IN UtilityProcedure (
       "SELECTION-WIDGET", SelectedComboHandle, "SELF", OUTPUT PrivateString ).
      SelectionWidget = WIDGET-HANDLE( PrivateString ).
      RUN GetPrivateData IN UtilityProcedure (
       "AllowInsensitive", SelectedComboHandle, "SELF", OUTPUT PrivateString ).
      AllowInsensitiveSelection = PrivateString EQ "yes".
      RUN GetPrivateData IN UtilityProcedure (
       "COMBO-PROCEDURE", SelectedComboHandle, "SELF", OUTPUT ComboProcedure ).
      RUN GetPrivateData IN UtilityProcedure (
       "AFTER-PROCEDURE", SelectedComboHandle, "SELF", OUTPUT AfterProcedure ).
      RUN GetPrivateData IN UtilityProcedure (
       "SELECTION-NEXT-WIDGET", SelectedComboHandle, "SELF", OUTPUT PrivateString ).
      SelectionNextWidget = WIDGET-HANDLE( PrivateString ).
      RUN GetPrivateData IN UtilityProcedure (
       "SelectionMultiple", SelectedComboHandle, "SELF", OUTPUT PrivateString ).
      SelectionMultiple = PrivateString EQ "yes".
      IF ( AllowInsensitiveSelection EQ no AND ThisFillIn:SENSITIVE EQ no ) OR
       ThisFillIn:HIDDEN THEN RETURN.
      ASSIGN
       SelectionStartValue = ThisFillIn:SCREEN-VALUE
       StartScrollUp       = IF KEYFUNCTION( LASTKEY ) = "CURSOR-UP" THEN yes ELSE no
       .
      IF ThisFillIn:SENSITIVE AND ThisFillIn:SCREEN-VALUE EQ "" THEN DO:
         ASSIGN ThisFillIn:BLANK = yes.
         APPLY "RECALL" TO ThisFillIn.
      END.
      ELSE IF ThisFillIn:SENSITIVE THEN DO:
         IsValid = ThisFillIn:VALIDATE() NO-ERROR.
         IF NOT IsValid THEN DO:
            ASSIGN ThisFillIn:BLANK = yes.
            APPLY "RECALL" TO ThisFillIn.
         END.
      END.
      ASSIGN
       FillWidget          = ThisFillIn
       EnteringCombo       = yes
       SelectionNextWidget = IF SelectionNextWidget:SENSITIVE THEN SelectionNextWidget ELSE SelectedComboHandle
       .
      RUN VALUE( ComboProcedure ).
   END PROCEDURE.  /* ProcessComboButton */

   /*-------------------------------------------------------------------------*/
   /* Define the procedure which will return the corresponding combo button   */
   /* name and handle (if one exists) for a specified FILL-IN widget:         */
   /*-------------------------------------------------------------------------*/

   PROCEDURE GetComboButton:
      DEFINE INPUT  PARAMETER ThisFillIn       AS WIDGET-HANDLE NO-UNDO.
      DEFINE OUTPUT PARAMETER ThisComboName    AS CHARACTER     NO-UNDO.
      DEFINE OUTPUT PARAMETER ThisComboHandle  AS WIDGET-HANDLE NO-UNDO.
      DEFINE VARIABLE ComboHandleString        AS CHARACTER     NO-UNDO.
      ASSIGN
       ThisComboName   = ?
       ThisComboHandle = ?
       .
      IF ThisFillIn:TYPE NE "FILL-IN" THEN RETURN.
      RUN GetPrivateData IN UtilityProcedure
       ( "COMBO-BUTTON", ThisFillIn, "SELF", OUTPUT ComboHandleString ).
      ASSIGN
       ThisComboHandle  = WIDGET-HANDLE( ComboHandleString )
       ThisComboName    = IF VALID-HANDLE( ThisComboHandle ) THEN ThisComboHandle:NAME
                          ELSE ?
       .
   END PROCEDURE.  /* GetComboButton */

&ENDIF  /* &IF DEFINED( ComboButtonUsed ) = 0 ... */

/*----------------------------------------------------------------------------*/
/* If the combo selection type is not specified, default it to "Lookup":      */
/*----------------------------------------------------------------------------*/

&IF DEFINED( SelectionType ) = 0 &THEN
   &SCOPED-DEFINE SelectionType Lookup
&ENDIF

/*----------------------------------------------------------------------------*/
/* Define variables which are used with "Lookup" combo boxes; these will only */
/* be defined once, and then only if a "Lookup" selection is used:            */
/*----------------------------------------------------------------------------*/

&IF "{&SelectionType}" = "Lookup" AND DEFINED( LookupVariables ) = 0 &THEN

   &GLOBAL-DEFINE LookupVariables yes

   DEFINE VARIABLE NumItems              AS INTEGER        NO-UNDO.
   DEFINE VARIABLE SelectionDelimiter    AS CHARACTER      NO-UNDO.
   DEFINE VARIABLE SelectionItem         AS CHARACTER      NO-UNDO.
   DEFINE VARIABLE SelectionFirst        AS INTEGER        NO-UNDO.
   DEFINE VARIABLE SelectionString       AS CHARACTER      NO-UNDO.
   DEFINE VARIABLE SelectionLength       AS INTEGER        NO-UNDO.
   DEFINE VARIABLE SelectionIndex        AS INTEGER        NO-UNDO.
   DEFINE VARIABLE SelectionFont         AS INTEGER        NO-UNDO.
   DEFINE VARIABLE StartingPosition      AS INTEGER        NO-UNDO.
   DEFINE VARIABLE DisplayDelimiter      AS CHARACTER      NO-UNDO.
   DEFINE VARIABLE DisplayItem           AS CHARACTER      NO-UNDO.
   DEFINE VARIABLE DisplayString         AS CHARACTER      NO-UNDO.
   DEFINE VARIABLE DisplayIndex          AS INTEGER        NO-UNDO.
   DEFINE VARIABLE RoomOnLeft            AS INTEGER        NO-UNDO.
   DEFINE VARIABLE RoomOnRight           AS INTEGER        NO-UNDO.
   DEFINE VARIABLE RoomOnTop             AS INTEGER        NO-UNDO.
   DEFINE VARIABLE RoomOnBottom          AS INTEGER        NO-UNDO.
   DEFINE VARIABLE HorizontalDirection   AS CHARACTER      NO-UNDO.
   DEFINE VARIABLE VerticalDirection     AS CHARACTER      NO-UNDO.
   DEFINE VARIABLE PixelWidth            AS INTEGER        NO-UNDO.
   DEFINE VARIABLE InnerLines            AS INTEGER        NO-UNDO.
   DEFINE VARIABLE DidMoveToTop          AS LOGICAL        NO-UNDO.

&ENDIF

/*----------------------------------------------------------------------------*/
/* Define a number which will be unique to this instance of combo.i; this     */
/* number will be used for various definitions which must be unique:          */
/*----------------------------------------------------------------------------*/

&SCOPED-DEFINE Num {&Sequence}

/*----------------------------------------------------------------------------*/
/* If the frame of the Fill-In/Combo is not specified, default it to the UIB  */
/* defined FRAME-NAME;  if this is not specified, put up an error:            */
/*----------------------------------------------------------------------------*/

&IF DEFINED( ComboFrame ) = 0 &THEN
   &SCOPED-DEFINE ComboFrame {&FRAME-NAME}
&ENDIF

/*----------------------------------------------------------------------------*/
/* The following section applies to "Lookup" type only; it will be compiled   */
/* ONCE PER FRAME. Determine if this frame has been referenced, and if not,   */
/* define a new Selection-List widget and its associated triggers:            */
/*----------------------------------------------------------------------------*/

&SCOPED-DEFINE FramePointer LOOKUP( "{&ComboFrame}", "{&FrameList}" )

&IF "{&SelectionType}" = "Lookup" AND {&FramePointer} = 0 &THEN

   &SCOPED-DEFINE SelectionWidget    SelectionWidget{&Num}

   &IF DEFINED( FrameList ) = 0 &THEN
      &SCOPED-DEFINE LocalFrameList  {&ComboFrame}
   &ELSE
      &SCOPED-DEFINE LocalFrameList  {&FrameList},{&ComboFrame}
      &UNDEFINE FrameList
   &ENDIF
   &GLOBAL-DEFINE FrameList       {&LocalFrameList}
   &SCOPED-DEFINE FramePointer    LOOKUP( "{&ComboFrame}", "{&FrameList}" )

   /*-------------------------------------------------------------------------*/
   /* Define the SelectionList widget for this frame and its associated       */
   /* triggers:                                                               */
   /*-------------------------------------------------------------------------*/

   DEFINE VARIABLE {&SelectionWidget} AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE 
     SIZE 1 BY 1 NO-UNDO.

   FORM
    {&SelectionWidget} AT ROW 1 COLUMN 1 NO-LABEL
    WITH FRAME {&ComboFrame}.

   ASSIGN
    SelectionWidget                       = {&SelectionWidget}:HANDLE IN FRAME {&ComboFrame}
    SelectionWidget:HIDDEN                = yes
    SelectionWidget:SENSITIVE             = no
    SelectionWidget:SCROLLBAR-HORIZONTAL  = no
    SelectionWidget:SCROLLBAR-VERTICAL    = yes
    SelectionList                         = ( IF SelectionList = "" THEN "" ELSE SelectionList + "," ) +
                                            STRING( SelectionWidget )
    .

   ON RETURN, MOUSE-SELECT-CLICK OF {&SelectionWidget} IN FRAME {&ComboFrame} DO:
      ASSIGN
       SelectionIndex          = {&SelectionWidget}:LOOKUP( {&SelectionWidget}:SCREEN-VALUE )
       FillWidget:SCREEN-VALUE = ( IF SelectionMultiple AND
                                      FillWidget:SCREEN-VALUE NE "" AND
                                      FillWidget:SCREEN-VALUE NE "?"
                                 THEN FillWidget:SCREEN-VALUE + DisplayDelimiter ELSE "" ) +
                                 ENTRY( SelectionIndex, DisplayString, DisplayDelimiter )
       FillWidget:BLANK        = no
       ComboChange             = yes
       .
      APPLY "ENTRY" TO SelectionNextWidget.
      RETURN NO-APPLY.
   END.

   ON ANY-PRINTABLE OF {&SelectionWidget} IN FRAME {&ComboFrame} DO:
      StartingPosition = {&SelectionWidget}:LOOKUP( {&SelectionWidget}:SCREEN-VALUE ).
      IF StartingPosition = ? THEN StartingPosition = {&SelectionWidget}:NUM-ITEMS.
      DO SelectionIndex = StartingPosition + 1 TO {&SelectionWidget}:NUM-ITEMS:
         IF {&SelectionWidget}:ENTRY( SelectionIndex ) BEGINS KEYFUNCTION( LASTKEY ) THEN DO:
            ASSIGN {&SelectionWidget}:SCREEN-VALUE = {&SelectionWidget}:ENTRY( SelectionIndex ).
            RETURN NO-APPLY.
         END.
      END.
      DO SelectionIndex = 1 TO StartingPosition:
         IF {&SelectionWidget}:ENTRY( SelectionIndex ) BEGINS KEYFUNCTION( LASTKEY ) THEN DO:
            ASSIGN {&SelectionWidget}:SCREEN-VALUE = {&SelectionWidget}:ENTRY( SelectionIndex ).
            RETURN NO-APPLY.
         END.
      END.
      RETURN NO-APPLY.
   END.

   ON END-ERROR OF {&SelectionWidget} IN FRAME {&ComboFrame} DO:
      APPLY "ENTRY" TO SelectionNextWidget.
      RETURN NO-APPLY.
   END.

   ON LEAVE OF {&SelectionWidget} IN FRAME {&ComboFrame} DO:
      DEFINE VARIABLE WasValid   AS LOGICAL   NO-UNDO.
      ASSIGN
       {&SelectionWidget}:HIDDEN IN FRAME {&ComboFrame} = yes
       {&SelectionWidget}:SENSITIVE                     = no
       EnteringCombo                                    = no
       .
      IF NOT ComboChange AND FillWidget:BLANK THEN ASSIGN
       FillWidget:SCREEN-VALUE = SelectionStartValue
       FillWidget:BLANK        = no
       .
      WasValid = FillWidget:VALIDATE().
      IF NOT WasValid THEN RETURN NO-APPLY.
      IF ComboChange AND AfterProcedure NE "" THEN RUN VALUE( AfterProcedure ).
      IF LOOKUP( "ValidateInput", THIS-PROCEDURE:INTERNAL-ENTRIES ) GT 0 THEN RUN ValidateInput( {&SelectionFill}:NAME ).
   END.

&ENDIF

/*----------------------------------------------------------------------------*/
/* Determine a unique name for the combo-box button:                          */
/*----------------------------------------------------------------------------*/

&SCOPED-DEFINE ComboButton ComboButton{&Num}

/*----------------------------------------------------------------------------*/
/* Define internal procedure names for processing of each combo button, and   */
/* for any processing which is specified to occur after the normal combo      */
/* processing has completed.  For Lookup and File type selections a different */
/* combo processing procedure will be required for each instance, while for   */
/* all other types a single procedure will suffice for all references:        */
/*----------------------------------------------------------------------------*/

&IF "{&SelectionType}" = "Lookup" &THEN
   &SCOPED-DEFINE ComboProcedureName C_Lookup_P{&Num}
&ELSEIF "{&SelectionType}" = "File" &THEN
   &SCOPED-DEFINE ComboProcedureName C_File_P{&Num}
&ELSE
   &SCOPED-DEFINE ComboProcedureName C_{&SelectionType}_P
&ENDIF
&IF DEFINED( AfterSelection ) GT 0 &THEN
   &IF NUM-ENTRIES( "{&Afterselection}", " " ) EQ 1 AND KEYWORD( "{&AfterSelection}" ) EQ ? &THEN
      &SCOPED-DEFINE AfterProcedureName {&AfterSelection}
   &ELSE
      &SCOPED-DEFINE AfterProcedureName A_P_{&Num}
      PROCEDURE {&AfterProcedureName}:
         {&AfterSelection}
      END PROCEDURE.
   &ENDIF
&ENDIF

/*----------------------------------------------------------------------------*/
/* Define the combo-box button and position it in its frame:                  */
/*----------------------------------------------------------------------------*/

DEFINE BUTTON {&ComboButton}
 IMAGE FILE "combo.bmp" IMAGE-SIZE-PIXELS 16 BY 18 FROM X 1 Y 1
 NO-FOCUS.

FORM
 {&ComboButton}
 WITH FRAME {&ComboFrame}.

DO WITH FRAME {&ComboFrame}:
   ASSIGN
    ComboButtonList             = ( IF ComboButtonList = "" THEN "" ELSE ComboButtonList + "," ) +
                                  STRING( {&ComboButton}:HANDLE )
    FillInList                  = ( IF FillInList = "" THEN "" ELSE FillInList + "," ) +
                                  STRING( {&SelectionFill}:HANDLE ) + FRAME {&ComboFrame}:NAME
    {&ComboButton}:SENSITIVE    = yes
    {&ComboButton}:ROW          = {&SelectionFill}:ROW
    {&ComboButton}:COLUMN       = {&SelectionFill}:COLUMN + {&SelectionFill}:WIDTH
    .
   RUN SetPrivateData IN UtilityProcedure
    ( "COMBO-BUTTON", {&SelectionFill}:HANDLE, "SELF", STRING( {&ComboButton}:HANDLE ) ).
   RUN SetPrivateData IN UtilityProcedure (
    "FRAME-HANDLE", {&ComboButton}:HANDLE, "SELF", STRING( FRAME {&ComboFrame}:HANDLE ) ).
   RUN SetPrivateData IN UtilityProcedure (
    "SELECTION-WIDGET", {&ComboButton}:HANDLE, "SELF",
    &IF "{&SelectionType}" EQ "Lookup" &THEN ENTRY( {&FramePointer}, SelectionList )
    &ELSE "" &ENDIF ).
   RUN SetPrivateData IN UtilityProcedure (
    "", {&ComboButton}:HANDLE, "SELF", &IF DEFINED( AllowInsensitive ) GT 0
    &THEN IF {&AllowInsensitive} THEN "AllowInsensitive" ELSE "" &ELSE "" &ENDIF ).
   RUN SetPrivateData IN UtilityProcedure (
    "COMBO-PROCEDURE", {&ComboButton}:HANDLE, "SELF", "{&ComboProcedureName}" ).
   RUN SetPrivateData IN UtilityProcedure (
    "AFTER-PROCEDURE", {&ComboButton}:HANDLE, "SELF", "{&AfterProcedureName}" ).
   RUN SetPrivateData IN UtilityProcedure (
    "SELECTION-NEXT-WIDGET", {&ComboButton}:HANDLE, "SELF",
    STRING( &IF DEFINED( NextWidget ) GT 0 &THEN 
             &IF "{&NextWidget}" EQ "TAB" &THEN {&SelectionFill}:NEXT-TAB-ITEM
             &ELSE {&NextWidget}:HANDLE
             &ENDIF
            &ELSE {&SelectionFill}:HANDLE
            &ENDIF ) ).
   RUN SetPrivateData IN UtilityProcedure (
    "", {&ComboButton}:HANDLE, "SELF", &IF DEFINED( MultipleSelect ) GT 0
    &THEN IF {&MultipleSelect} THEN "SelectionMultiple" ELSE "" &ELSE "" &ENDIF ).
END.

/*----------------------------------------------------------------------------*/
/* Define trigger to invoke combo processing for this combo button:           */
/*----------------------------------------------------------------------------*/

ON CHOOSE OF {&ComboButton} IN FRAME {&ComboFrame} OR
 CURSOR-UP,CURSOR-DOWN OF {&SelectionFill} IN FRAME {&ComboFrame} DO:
   RUN ProcessComboButton ( INPUT {&SelectionFill}:HANDLE ).
   RETURN NO-APPLY.
END.  /* ON CHOOSE OF {&ComboButton} ... */

/*----------------------------------------------------------------------------*/
/* Define trigger to allow "first character" search/select of selection list  */
/* whereby pressing any printable character in the fill-in causes the next    */
/* selection list item which begins with that character (if any) to be        */
/* displayed.  This requires that a pre-defined selection (or display) list   */
/* be defined to combo.i, either as a literal string or as the name of a      */
/* variable which contains a literal string... this will NOT work for         */
/* selection lists which are constructed from database records, etc.          */
/*----------------------------------------------------------------------------*/

&IF "{&FirstCharacterSearch}" EQ "yes" &THEN
   ON ANY-PRINTABLE OF {&SelectionFill} IN FRAME {&ComboFrame} DO:
      ASSIGN
       ComboSearchString   = &IF DEFINED( DisplayString ) GT 0 &THEN {&DisplayString}
                             &ELSE {&SelectionString} &ENDIF
       SearchDelimiter     = &IF DEFINED( DisplayString ) GT 0 &THEN
                              &IF DEFINED( DisplayDelimiter ) GT 0
                              &THEN "{&DisplayDelimiter}"
                              &ELSE ","
                              &ENDIF
                             &ELSE
                              &IF DEFINED( SelectionDelimiter ) GT 0
                              &THEN "{&SelectionDelimiter}"
                              &ELSE ","
                              &ENDIF
                             &ENDIF
       EndSearchPosition   = NUM-ENTRIES( ComboSearchString, SearchDelimiter )
       StartSearchPosition = IF LastSearchPosition GE EndSearchPosition THEN 1
                             ELSE IF LastSearchCharacter EQ KEYFUNCTION( LASTKEY )
                              THEN LastSearchPosition + 1
                             ELSE 1
       LastSearchCharacter = KEYFUNCTION( LASTKEY )
       .
      DO SelectionSection = 1 TO ( IF StartSearchPosition LE 1 THEN 1 ELSE 2):
         DO SearchIndex = ( IF SelectionSection EQ 1 THEN StartSearchPosition ELSE 1 ) TO
          ( IF SelectionSection EQ 1 THEN EndSearchPosition ELSE StartSearchPosition - 1):
            SearchElement = ENTRY( SearchIndex, ComboSearchString, SearchDelimiter ).
            IF SearchElement BEGINS LastSearchCharacter THEN DO:
               ASSIGN
                {&SelectionFill}:SCREEN-VALUE = SearchElement
                LastSearchPosition            = SearchIndex
                .
               RETURN NO-APPLY.
            END.
         END.
      END.
      ASSIGN
       {&SelectionFill}:SCREEN-VALUE = ""
       LastSearchPosition            = 0
       .
      BELL.
      RETURN NO-APPLY.
   END.  /* ON CHOOSE OF {&ComboButton} ... */
&ENDIF

/*----------------------------------------------------------------------------*/
/* Define the procedures which will actually do the combo processing:         */
/*----------------------------------------------------------------------------*/

&IF "{&SelectionType}" EQ "File" &THEN
   PROCEDURE {&ComboProcedureName}:
      DEFINE VARIABLE SelectedFileName  AS CHARACTER     NO-UNDO.
      DEFINE VARIABLE DidSelectFile     AS LOGICAL       NO-UNDO.
      ASSIGN EnteringCombo = no.
      SYSTEM-DIALOG GET-FILE SelectedFileName
       &IF DEFINED( SelectionFilters ) GT 0 &THEN FILTERS {&SelectionFilters} &ENDIF
       &IF DEFINED( SelectionDirectory ) GT 0 &THEN INITIAL-DIR REPLACE( {&SelectionDirectory}, "/", "\" ) &ENDIF
       &IF DEFINED( SelectionTitle ) GT 0 &THEN TITLE "{&SelectionTitle}" &ENDIF
       &IF "{&SelectionMustExist}" EQ "yes" &THEN MUST-EXIST &ENDIF
       UPDATE DidSelectFile.
      IF DidSelectFile THEN DO:
         ASSIGN
          FillWidget:SCREEN-VALUE = ( IF SelectionMultiple AND FillWidget:SCREEN-VALUE NE "" THEN 
                                      FillWidget:SCREEN-VALUE + "," ELSE "" ) +
                                    ( IF "{&SelectionFullPath}" NE "yes" AND
                                      NUM-ENTRIES( SelectedFileName, "\" ) > 0 THEN
                                      ENTRY( NUM-ENTRIES( SelectedFileName, "\" ), SelectedFileName, "\" )
                                      ELSE SelectedFileName )
          FillWidget:BLANK        = no
          ComboChange             = yes
          .
         IF AfterProcedure NE "" THEN DO:
            RUN VALUE( AfterProcedure ).
            APPLY "LEAVE" TO FillWidget.
            APPLY "ENTRY" TO FillWidget.
         END.
      END.
      ELSE ASSIGN
       FillWidget:SCREEN-VALUE = SelectionStartValue
       FillWidget:BLANK        = no
       .
      APPLY "ENTRY" TO SelectionNextWidget.
   END PROCEDURE.
   
&ELSEIF "{&SelectionType}" EQ "Date" AND DEFINED( DateComboProcedure ) EQ 0 &THEN

   &GLOBAL-DEFINE DateComboProcedure yes
   PROCEDURE {&ComboProcedureName}:
      DEFINE VARIABLE SelectionDate     AS DATE          NO-UNDO.
      ASSIGN EnteringCombo = no.
      SelectionDate = DATE( SelectionStartValue ) NO-ERROR.
      IF ERROR-STATUS:ERROR OR SelectionDate EQ ? OR MONTH( SelectionDate ) EQ 0 OR
       MONTH( SelectionDate ) EQ ? THEN SelectionDate = TODAY.
      RUN tools/calendar.p ( INPUT-OUTPUT SelectionDate, INPUT FillWidget ).
      IF SelectionDate NE ? THEN DO:
         ASSIGN 
          FillWidget:SCREEN-VALUE = ( IF "{&MultipleSelect}" = "yes" AND FillWidget:SCREEN-VALUE <> "" THEN 
                                      FillWidget:SCREEN-VALUE + "," ELSE "" ) + STRING( SelectionDate )
          FillWidget:BLANK        = no
          ComboChange             = yes
          .
         IF AfterProcedure NE "" THEN DO:
            RUN VALUE( AfterProcedure ).
            APPLY "LEAVE" TO FillWidget.
            APPLY "ENTRY" TO FillWidget.
         END.
      END.
      ELSE ASSIGN
       FillWidget:SCREEN-VALUE = SelectionStartValue
       FillWidget:BLANK        = no
       .
      APPLY "ENTRY" TO SelectionNextWidget.
   END PROCEDURE.

&ELSEIF "{&SelectionType}" EQ "Calculation" AND DEFINED( CalcComboProcedure ) EQ 0 &THEN

   &GLOBAL-DEFINE CalcComboProcedure yes
   PROCEDURE {&ComboProcedureName}:
      DEFINE VARIABLE CalcResult AS CHARACTER     NO-UNDO.
      ASSIGN EnteringCombo = no.
      ASSIGN CalcResult = SelectionStartValue.
      RUN tools/clacalc.p ( INPUT-OUTPUT CalcResult, INPUT FillWidget ).
      IF CalcResult NE ? THEN DO:
         ASSIGN
          FillWidget:SCREEN-VALUE = ( IF "{&MultipleSelect}" EQ "yes" AND FillWidget:SCREEN-VALUE <> "" THEN 
                                      FillWidget:SCREEN-VALUE + "," ELSE "" ) + CalcResult
          FillWidget:BLANK        = no
          ComboChange             = yes
          .
         IF AfterProcedure NE "" THEN DO:
            RUN VALUE( AfterProcedure ).
            APPLY "LEAVE" TO FillWidget.
            APPLY "ENTRY" TO FillWidget.
         END.
      END.
      ELSE ASSIGN
       FillWidget:SCREEN-VALUE = SelectionStartValue
       FillWidget:BLANK        = no
       .
      APPLY "ENTRY" TO SelectionNextWidget.
   END PROCEDURE.

&ELSEIF "{&SelectionType}" EQ "Color" AND DEFINED( ColorComboProcedure ) EQ 0 &THEN

   &GLOBAL-DEFINE ColorComboProcedure yes
   PROCEDURE {&ComboProcedureName}:
      DEFINE VARIABLE SelectionColor    AS INTEGER       NO-UNDO.
      DEFINE VARIABLE DidSelectColor    AS LOGICAL       NO-UNDO.
      ASSIGN EnteringCombo = no.
      SelectionColor = INTEGER( SelectionStartValue ) NO-ERROR.
      IF ERROR-STATUS:ERROR OR SelectionColor = ? THEN SelectionColor = 0.
      RUN tools/getcolor.w ( INPUT-OUTPUT SelectionColor, OUTPUT DidSelectColor ).
      IF DidSelectColor THEN DO:
         ASSIGN
          FillWidget:SCREEN-VALUE = ( IF "{&MultipleSelect}" = "yes" AND FillWidget:SCREEN-VALUE <> "" THEN 
                                      FillWidget:SCREEN-VALUE + "," ELSE "" ) + STRING( SelectionColor )
          FillWidget:BLANK        = no
          ComboChange             = yes
          .
         IF AfterProcedure NE "" THEN DO:
            RUN VALUE( AfterProcedure ).
            APPLY "LEAVE" TO FillWidget.
            APPLY "ENTRY" TO FillWidget.
         END.
      END.
      ELSE ASSIGN
       FillWidget:SCREEN-VALUE = SelectionStartValue
       FillWidget:BLANK        = no
       .
      APPLY "ENTRY" TO SelectionNextWidget.
   END PROCEDURE.

&ELSEIF "{&SelectionType}" EQ "Font" AND DEFINED( FontComboProcedure ) EQ 0 &THEN

   &GLOBAL-DEFINE FontComboProcedure yes
   PROCEDURE {&ComboProcedureName}:
      DEFINE VARIABLE SelectionFontNum  AS INTEGER       NO-UNDO.
      DEFINE VARIABLE DidSelectFont     AS LOGICAL       NO-UNDO.
      ASSIGN EnteringCombo = no.
      SelectionFontNum = INTEGER( SelectionStartValue ) NO-ERROR.
      IF ERROR-STATUS:ERROR OR SelectionFontNum EQ ? THEN SelectionFontNum = 0.
      RUN tools/getfont.w ( INPUT-OUTPUT SelectionFontNum, OUTPUT DidSelectFont ).
      IF DidSelectFont THEN DO:
         ASSIGN
          FillWidget:SCREEN-VALUE = ( IF "{&MultipleSelect}" EQ "yes" AND FillWidget:SCREEN-VALUE NE "" THEN 
                                      FillWidget:SCREEN-VALUE + "," ELSE "" ) + STRING( SelectionFontNum )
          FillWidget:BLANK        = no
          ComboChange             = yes
          .
         IF AfterProcedure NE "" THEN DO:
            RUN VALUE( AfterProcedure ).
            APPLY "LEAVE" TO FillWidget.
            APPLY "ENTRY" TO FillWidget.
         END.
      END.
      ELSE ASSIGN
       FillWidget:SCREEN-VALUE = SelectionStartValue
       FillWidget:BLANK        = no
       .
      APPLY "ENTRY" TO SelectionNextWidget.
   END PROCEDURE.

&ELSEIF "{&SelectionType}" = "Lookup" &THEN

   PROCEDURE {&ComboProcedureName}:

      SESSION:SET-WAIT-STATE( "General" ).

      /*----------------------------------------------------------------------*/
      /* Assign the font for this selection widget:                           */
      /*----------------------------------------------------------------------*/

      ASSIGN
       SelectionFont = &IF DEFINED( SelectionFont ) <> 0
                       &THEN {&SelectionFont}
                       &ELSE {&DefaultSelectionFont}
                       &ENDIF
       ComboChange   = no
       .
      ASSIGN
       NumItems           = 0
       SelectionLength    = 0
       SelectionFirst     = ?
       SelectionString    = ""
       DisplayDelimiter   = &IF DEFINED( DisplayDelimiter ) <> 0 &THEN "{&DisplayDelimiter}"
                            &ELSE "{&DefaultDisplayDelimiter}" &ENDIF
       DisplayString      = ""
       .
      /*----------------------------------------------------------------------*/
      /* If the selection list is being obtained from a database table define */
      /* its entries accordingly:                                             */
      /*----------------------------------------------------------------------*/
      DO WITH FRAME {&ComboFrame}:
         &IF DEFINED( SelectionTable ) GT 0 &THEN
            &IF DEFINED( SelectionField ) EQ 0 AND DEFINED( SelectionString ) EQ 0 &THEN
               &MESSAGE ERROR: <Combo.i> Either &SelectionField OR &SelectionString must be defined.
            &ENDIF
            ASSIGN
             SelectionDelimiter = &IF DEFINED( SelectionDelimiter ) NE 0 &THEN "{&SelectionDelimiter}"
                                  &ELSE "{&DefaultSelectionDelimiter}" &ENDIF
             SelectionWidget:DELIMITER = SelectionDelimiter
             .
            FOR EACH {&SelectionTable} {&WhereExpression} {&UseIndexExpression} NO-LOCK {&ByExpression}:
               ASSIGN
                SelectionItem      = &IF DEFINED( SelectionField ) GT 0
                                     &THEN STRING( {&SelectionTable}.{&SelectionField} )
                                     &ELSE {&SelectionString}
                                     &ENDIF
                DisplayItem        = &IF DEFINED( DisplayField ) GT 0 
                                      &THEN STRING( {&SelectionTable}.{&DisplayField} )
                                     &ELSEIF DEFINED( DisplayString ) GT 0
                                      &THEN {&DisplayString}
                                     &ELSEIF DEFINED( SelectionField ) GT 0
                                      &THEN STRING( {&SelectionTable}.{&SelectionField} )
                                     &ELSE {&SelectionString}
                                     &ENDIF
                .
               IF LOOKUP( SelectionItem, SelectionString, SelectionDelimiter ) GT 0 THEN NEXT.
               IF SelectionMultiple AND LOOKUP( DisplayItem, FillWidget:SCREEN-VALUE, DisplayDelimiter ) GT 0
                THEN NEXT.
               IF SelectionItem EQ ? THEN NEXT.
               ASSIGN
                SelectionString    = ( IF SelectionString NE "" THEN SelectionString + SelectionDelimiter
                                       ELSE "" ) + SelectionItem
                DisplayString      = ( IF DisplayString NE "" THEN DisplayString + DisplayDelimiter
                                       ELSE "" ) + DisplayItem
                SelectionLength    = MAX( SelectionLength, FONT-TABLE:GET-TEXT-WIDTH-PIXELS( SelectionItem,
                                     SelectionFont ) )
                NumItems           = NumItems + 1
                .
               IF SelectionStartValue <> "" AND SelectionFirst = ? AND
                ( FillWidget:DATA-TYPE =  "CHARACTER" AND DisplayItem BEGINS SelectionStartValue OR
                  FillWidget:DATA-TYPE <> "CHARACTER" AND DisplayItem = SelectionStartValue )
                THEN ASSIGN SelectionFirst  = NumItems.
            END.  /* FOR EACH {&SelectionTable} ... */
         /*-------------------------------------------------------------------*/
         /* If the selection list is being obtained from SelectionString      */
         /* define its entries accordingly:                                   */
         /*-------------------------------------------------------------------*/
         &ELSEIF DEFINED( SelectionString ) GT 0 &THEN
            ASSIGN
             SelectionDelimiter        = &IF DEFINED( SelectionDelimiter ) NE 0 &THEN "{&SelectionDelimiter}"
                                         &ELSE "," &ENDIF
             SelectionWidget:DELIMITER = SelectionDelimiter
             SelectionString           = {&SelectionString}
             DisplayString             = &IF DEFINED( DisplayString ) GT 0 &THEN {&DisplayString}
                                         &ELSE {&SelectionString}
                                         &ENDIF
             .
            IF SelectionMultiple THEN
             DO DisplayIndex = 1 TO NUM-ENTRIES( FillWidget:SCREEN-VALUE, DisplayDelimiter ):
               ASSIGN
                DisplayItem    = ENTRY( DisplayIndex, FillWidget:SCREEN-VALUE, DisplayDelimiter )
                SelectionIndex = LOOKUP( DisplayItem, DisplayString, DisplayDelimiter )
                .
               IF SelectionIndex GT 0 THEN DO:
                  {include/delentry.i SelectionIndex SelectionString SelectionDelimiter }
                  {include/delentry.i SelectionIndex DisplayString   DisplayDelimiter }
               END.
            END.
            DO SelectionIndex = 1 TO NUM-ENTRIES( SelectionString, SelectionWidget:DELIMITER ):
               ASSIGN
                SelectionItem      = ENTRY( SelectionIndex, SelectionString, SelectionWidget:DELIMITER )
                SelectionLength    = MAX( SelectionLength, FONT-TABLE:GET-TEXT-WIDTH-PIXELS( SelectionItem,
                                     SelectionFont ) )
                NumItems           = NumItems + 1
                .
               IF SelectionStartValue NE "" AND SelectionFirst = ? AND
                ( FillWidget:DATA-TYPE EQ "CHARACTER" AND DisplayItem BEGINS SelectionStartValue OR
                  FillWidget:DATA-TYPE NE "CHARACTER" AND DisplayItem EQ SelectionStartValue )
                THEN ASSIGN SelectionFirst = NumItems.
            END.  /* DO SelectionIndex = 1 TO ... */
         /*-------------------------------------------------------------------*/
         /* If the selection list is undefined generate a compile-time        */
         /* message:                                                          */
         /*-------------------------------------------------------------------*/
         &ELSE
            &MESSAGE ERROR: <Combo.i> No selection list can be determined.
         &ENDIF
         /*-------------------------------------------------------------------*/
         /* If no items are available for selection put up message and leave: */
         /*-------------------------------------------------------------------*/
         IF NumItems = 0 THEN DO:
            SESSION:SET-WAIT-STATE( "" ).
            &IF DEFINED( NoItemsMessage ) &THEN
               MESSAGE "{&NoItemsMessage}" VIEW-AS ALERT-BOX WARNING BUTTONS OK.
            &ELSE
               MESSAGE ( IF SelectionMultiple THEN "No other allowed values are defined"
                ELSE "No values defined for Selection" )
                VIEW-AS ALERT-BOX WARNING BUTTONS OK.
            &ENDIF
            ASSIGN FillWidget:BLANK = no.
            RETURN.
         END.  /* IF NumItems = 0 THEN ... */
         /*-------------------------------------------------------------------*/
         /* Determine the optimal size of the selection-list widget based on  */
         /* its location, the number of entries, and the size of the entries  */
         /* and of the frame:                                                 */
         /*-------------------------------------------------------------------*/

         ASSIGN
          RoomOnLeft          = FillWidget:X + FillWidget:WIDTH-PIXELS
          RoomOnRight         = FillFrame:WIDTH-PIXELS - FillWidget:X
          RoomOnTop           = FillWidget:ROW - 1
          RoomOnBottom        = FillFrame:HEIGHT - FillWidget:ROW - FillWidget:HEIGHT -
                                ( IF FillFrame:HEIGHT GT 5 THEN 1.0 ELSE .01 )
          HorizontalDirection = &IF DEFINED( HorizontalDirection ) GT 0 &THEN "{&HorizontalDirection}"
                                &ELSE IF RoomOnLeft GT RoomOnRight THEN "LEFT" ELSE "RIGHT"
                                &ENDIF
          VerticalDirection   = &IF DEFINED( VerticalDirection ) GT 0 &THEN "{&VerticalDirection}"
                                &ELSE IF RoomOnBottom GT RoomOnTop THEN "BOTTOM" ELSE "TOP"
                                &ENDIF
          PixelWidth          = IF HorizontalDirection EQ "RIGHT" THEN
                                ( IF RoomOnRight GT ( SelectionLength + 44 ) THEN SelectionLength + 44
                                  ELSE RoomOnRight - 5 )
                                ELSE
                                ( IF RoomOnLeft GT ( SelectionLength + 44 ) THEN SelectionLength + 44
                                  ELSE RoomOnLeft - 5 )
          InnerLines          = IF VerticalDirection EQ "TOP" THEN
                                MINIMUM( RoomOnTop / FONT-TABLE:GET-TEXT-HEIGHT-CHARS( SelectionFont ),
                                 NumItems )
                                ELSE MINIMUM( RoomOnBottom / FONT-TABLE:GET-TEXT-HEIGHT-CHARS( 
                                 SelectionFont ), NumItems )
          InnerLines          = &IF DEFINED( InnerLines ) GT 0 &THEN
                                MINIMUM( InnerLines, {&InnerLines } )
                                &ELSE InnerLines &ENDIF
          SelectionWidget:BGCOLOR      = &IF DEFINED( SelectionBGColor ) &THEN {&SelectionBGColor}
                                         &ELSE 15
                                         &ENDIF
          SelectionWidget:FGCOLOR      = &IF DEFINED( SelectionFGColor ) &THEN {&SelectionFGColor}
                                         &ELSE 0
                                         &ENDIF
          SelectionWidget:FONT         = SelectionFont
          SelectionWidget:WIDTH-PIXELS = PixelWidth
          SelectionWidget:INNER-LINES  = InnerLines 
          SelectionWidget:COLUMN       = IF HorizontalDirection = "RIGHT" THEN FillWidget:COLUMN
                                         ELSE MAX( FillWidget:COLUMN + FillWidget:WIDTH -
                                          SelectionWidget:WIDTH-CHARS, 1 )
          SelectionWidget:ROW          = IF VerticalDirection = "TOP" THEN 
                                         MAX( FillWidget:ROW - SelectionWidget:HEIGHT-CHARS, 1 )
                                         ELSE FillWidget:ROW + FillWidget:HEIGHT
          SelectionWidget:LIST-ITEMS   = SelectionString
          DidMoveToTop                 = SelectionWidget:MOVE-TO-TOP()
          NO-ERROR.
         IF SelectionFirst = ? THEN SelectionFirst = IF StartScrollUp THEN NumItems ELSE 1.
         SelectionWidget:SCREEN-VALUE = SelectionWidget:ENTRY( SelectionFirst ).
         ASSIGN
          SelectionWidget:SENSITIVE    = yes
          SelectionWidget:VISIBLE      = yes
          .
         SESSION:SET-WAIT-STATE( "" ).
         APPLY "ENTRY" TO SelectionWidget.

      END.  /* DO WITH FRAME ... */

   END PROCEDURE.

&ENDIF
