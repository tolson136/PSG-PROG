FOR EACH BigSearch EXCLUSIVE-LOCK:
   DELETE BigSearch.
END.   

FOR EACH propsl where c-name begins "cbre" NO-LOCK:
      CREATE BigSearch.
      ASSIGN BigSearch.propsl# = Propsl.Propsl#
             BigSearch.Fld     = "Company Name"
             BigSearch.Val     = Propsl.c-name
             .
      CREATE BigSearch.       
      ASSIGN BigSearch.propsl# = Propsl.Propsl#
             BigSearch.Fld     = "Location Name"
             BigSearch.Val     = Propsl.l-name
             .
      CREATE BigSearch.   
      IF propsl.Addr1 NE "" THEN     
      ASSIGN BigSearch.propsl# = Propsl.Propsl#
             BigSearch.Fld     = "Address 1"
             BigSearch.Val     = Propsl.Addr1
             .
      CREATE BigSearch.       
      IF propsl.Addr2 NE "" THEN 
      ASSIGN BigSearch.propsl# = Propsl.Propsl#
             BigSearch.Fld     = "Address 2"
             BigSearch.Val     = Propsl.Addr2
             .         
      CREATE BigSearch. 
      IF propsl.Addr3 NE "" THEN           
      ASSIGN BigSearch.propsl# = Propsl.Propsl#
             BigSearch.Fld     = "Address 3"
             BigSearch.Val     = Propsl.Addr3
             .         
      CREATE BigSearch.
      IF propsl.Addr4 NE "" THEN            
      ASSIGN BigSearch.propsl# = Propsl.Propsl#
             BigSearch.Fld     = "Address 4"
             BigSearch.Val     = Propsl.Addr4
             .         
      CREATE BigSearch.   
      IF propsl.lAddr01 NE "" THEN          
      ASSIGN BigSearch.propsl# = Propsl.Propsl#
             BigSearch.Fld     = "Loc Address 1"
             BigSearch.Val     = Propsl.lAddr01
             .         
      CREATE BigSearch.           
      IF propsl.lAddr02 NE "" THEN
      ASSIGN BigSearch.propsl# = Propsl.Propsl#
             BigSearch.Fld     = "Loc Address 2"
             BigSearch.Val     = Propsl.lAddr02
             . 
      CREATE BigSearch.       
      IF propsl.lAddr03 NE "" THEN
      ASSIGN BigSearch.propsl# = Propsl.Propsl#
             BigSearch.Fld     = "Loc Address 3"
             BigSearch.Val     = Propsl.lAddr03
             .              
END. /* for each propsl */

      
