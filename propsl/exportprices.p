DEF STREAM a.
OUTPUT STREAM a TO c:\tim\pro-desp-amt.dat.
    FOR EACH pro-desp WHERE sub# = 0 /*AND route# = 10*/:
    
    DISPLAY STREAM a   PRO-DESP.COMP# ","
        PRO-DESP.SUB#  ","
        PRO-DESP.DIV#  ","
        PRO-DESP.ROUTE# ","
        PRO-DESP.CUST#  ","
        PRO-DESP.PROPSL# ","
        PRO-DESP.ITEM#  ","
        PRO-DESP.AMT  format "-zzzzz9.99"  ","
        freq     
        SKIP
        with width 150.

END.
OUTPUT STREAM a CLOSE.
