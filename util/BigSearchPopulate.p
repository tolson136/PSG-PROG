/**************************************************/
/* BigSearchPopulate.p                            */
/*                                                */
/* Build dataset to support BigSearch             */
/*                                                */
/*  4/20/2018    TO   Initial Development         */
/*                                                */
/**************************************************/

DEFINE VARIABLE EntryCount AS INT NO-UNDO.
DEFINE VARIABLE TempInt    AS INT NO-UNDO.

FOR EACH BigSearch EXCLUSIVE-LOCK:
   DELETE BigSearch.
END.   

FOR EACH propsl /*where c-name begins "cbre"*/ NO-LOCK:
      CREATE BigSearch.
      ASSIGN BigSearch.propsl# = Propsl.Propsl#
             BigSearch.Comp#   = Propsl.Comp#
             BigSearch.Div#    = Propsl.Div#
             BigSearch.Cust#   = Propsl.Cust#
             BigSearch.Fld     = "Company Name"
             BigSearch.Val     = Propsl.c-name
             .                             
       REPEAT EntryCount = 1 to NUM-ENTRIES(propsl.l-name," "): 
         CREATE BigSearch.       
         ASSIGN BigSearch.propsl# = Propsl.Propsl#
                BigSearch.Comp#   = Propsl.Comp#
                BigSearch.Div#    = Propsl.Div#
                BigSearch.Cust#   = Propsl.Cust#
                BigSearch.Fld     = "Location Name"
                BigSearch.Val     = ENTRY(EntryCount,Propsl.l-name," ")
                .
      END.
                      
      IF propsl.Addr1 NE "" THEN REPEAT EntryCount = 1 to NUM-ENTRIES(propsl.addr1," "):    
         IF ENTRY(EntryCount,Propsl.Addr1," ") = "" THEN NEXT.
         ASSIGN TempInt = INTEGER(ENTRY(EntryCount,Propsl.Addr1," ")) NO-ERROR.
         IF NOT ERROR-STATUS:ERROR THEN NEXT.
         CREATE BigSearch.
         ASSIGN BigSearch.propsl# = Propsl.Propsl#
                BigSearch.Comp#   = Propsl.Comp#
                BigSearch.Div#    = Propsl.Div#
                BigSearch.Cust#   = Propsl.Cust#
                BigSearch.Fld     = "Address 1"
                BigSearch.Val     = ENTRY(EntryCount,Propsl.Addr1," ")
                .
      END.

      IF propsl.Addr2 NE "" THEN REPEAT EntryCount = 1 to NUM-ENTRIES(propsl.addr2," "):    
         IF ENTRY(EntryCount,Propsl.Addr2," ") = "" THEN NEXT.
         ASSIGN TempInt = INTEGER(ENTRY(EntryCount,Propsl.Addr2," ")) NO-ERROR.
         IF NOT ERROR-STATUS:ERROR THEN NEXT.
         CREATE BigSearch.
         ASSIGN BigSearch.propsl# = Propsl.Propsl#
                BigSearch.Comp#   = Propsl.Comp#
                BigSearch.Div#    = Propsl.Div#
                BigSearch.Cust#   = Propsl.Cust#
                BigSearch.Fld     = "Address 2"
                BigSearch.Val     = ENTRY(EntryCount,Propsl.Addr2," ")
                .
      END.                
      
      IF propsl.Addr3 NE "" THEN REPEAT EntryCount = 1 to NUM-ENTRIES(propsl.addr3," "):    
         IF ENTRY(EntryCount,Propsl.Addr3," ") = "" THEN NEXT.
         ASSIGN TempInt = INTEGER(ENTRY(EntryCount,Propsl.Addr3," ")) NO-ERROR.
         IF NOT ERROR-STATUS:ERROR THEN NEXT.
         CREATE BigSearch.
         ASSIGN BigSearch.propsl# = Propsl.Propsl#
                BigSearch.Comp#   = Propsl.Comp#
                BigSearch.Div#    = Propsl.Div#
                BigSearch.Cust#   = Propsl.Cust#
                BigSearch.Fld     = "Address 3"
                BigSearch.Val     = ENTRY(EntryCount,Propsl.Addr3," ")
                .
      END. 

      IF propsl.Addr4 NE "" THEN REPEAT EntryCount = 1 to NUM-ENTRIES(propsl.addr4," "):    
         IF ENTRY(EntryCount,Propsl.Addr4," ") = "" THEN NEXT.
         ASSIGN TempInt = INTEGER(ENTRY(EntryCount,Propsl.Addr4," ")) NO-ERROR.
         IF NOT ERROR-STATUS:ERROR THEN NEXT.
         CREATE BigSearch.
         ASSIGN BigSearch.propsl# = Propsl.Propsl#
                BigSearch.Comp#   = Propsl.Comp#
                BigSearch.Div#    = Propsl.Div#
                BigSearch.Cust#   = Propsl.Cust#
                BigSearch.Fld     = "Address 4"
                BigSearch.Val     = ENTRY(EntryCount,Propsl.Addr4," ")
                .
      END. 

      IF propsl.Addr5 NE "" THEN REPEAT EntryCount = 1 to NUM-ENTRIES(propsl.addr5," "):    
         IF ENTRY(EntryCount,Propsl.Addr5," ") = "" THEN NEXT.
         ASSIGN TempInt = INTEGER(ENTRY(EntryCount,Propsl.Addr5," ")) NO-ERROR.
         IF NOT ERROR-STATUS:ERROR THEN NEXT.
         CREATE BigSearch.
         ASSIGN BigSearch.propsl# = Propsl.Propsl#
                BigSearch.Comp#   = Propsl.Comp#
                BigSearch.Div#    = Propsl.Div#
                BigSearch.Cust#   = Propsl.Cust#
                BigSearch.Fld     = "Address 5"
                BigSearch.Val     = ENTRY(EntryCount,Propsl.Addr5," ")
                .
      END. 
      
      IF propsl.laddr01 NE "" THEN REPEAT EntryCount = 1 to NUM-ENTRIES(propsl.laddr01," "):  
         IF ENTRY(EntryCount,Propsl.laddr01," ") = "" THEN NEXT.
         ASSIGN TempInt = INTEGER(ENTRY(EntryCount,Propsl.lAddr01," ")) NO-ERROR.
         IF NOT ERROR-STATUS:ERROR THEN NEXT.  
         CREATE BigSearch.
         ASSIGN BigSearch.propsl# = Propsl.Propsl#
                BigSearch.Comp#   = Propsl.Comp#
                BigSearch.Div#    = Propsl.Div#
                BigSearch.Cust#   = Propsl.Cust#
                BigSearch.Fld     = "Loc Addr1"
                BigSearch.Val     = ENTRY(EntryCount,Propsl.laddr01," ")
                .
      END. 

      IF propsl.laddr02 NE "" THEN REPEAT EntryCount = 1 to NUM-ENTRIES(propsl.laddr02," "):  
         IF ENTRY(EntryCount,Propsl.laddr02," ") = "" THEN NEXT.  
         ASSIGN TempInt = INTEGER(ENTRY(EntryCount,Propsl.lAddr02," ")) NO-ERROR.
         IF NOT ERROR-STATUS:ERROR THEN NEXT.
         CREATE BigSearch.
         ASSIGN BigSearch.propsl# = Propsl.Propsl#
                BigSearch.Comp#   = Propsl.Comp#
                BigSearch.Div#    = Propsl.Div#
                BigSearch.Cust#   = Propsl.Cust#
                BigSearch.Fld     = "Loc Addr2"
                BigSearch.Val     = ENTRY(EntryCount,Propsl.laddr02," ")
                .
      END. 

      IF propsl.laddr03 NE "" THEN REPEAT EntryCount = 1 to NUM-ENTRIES(propsl.laddr03," "):    
         IF ENTRY(EntryCount,Propsl.laddr03," ") = "" THEN NEXT.
         ASSIGN TempInt = INTEGER(ENTRY(EntryCount,Propsl.lAddr03," ")) NO-ERROR.
         IF NOT ERROR-STATUS:ERROR THEN NEXT.
         CREATE BigSearch.
         ASSIGN BigSearch.propsl# = Propsl.Propsl#
                BigSearch.Comp#   = Propsl.Comp#
                BigSearch.Div#    = Propsl.Div#
                BigSearch.Cust#   = Propsl.Cust#
                BigSearch.Fld     = "Loc Addr3"
                BigSearch.Val     = ENTRY(EntryCount,Propsl.laddr03," ")
                .
      END. 

END. /* for each propsl */

      
