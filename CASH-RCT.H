    OUTPUT TO PRINTER.
    FOR EACH WK1:
	FORM HEADER TODAY FORMAT "99/99/9999"
		    "A/R CASH RECEIPTS JOURNAL" AT 28
		    "PAGE:" AT 65 PAGE-NUMBER FORMAT ">>9" SKIP(0)
		    "COMPANY: " AT 20 XCOM-N
		    "TIME: " AT 65 STRING(TIME,"HH:MM:SS") SKIP(0)
		    "DIVISION: " AT 20 XDIV-N "OPER: " AT 65 XOPR SKIP(0)
		    "ACCOUNTING PERIOD: " P-PERIOD FORMAT "XX-XXXX" SKIP(0)
		    "--------------------------------------------------------------------------------" SKIP(0)
		    "   ACCOUNT     NAME                                 CHECK #          AMOUNT     " SKIP(0)
		    "    DATE    REFERENCE       INVOICE #     SLS #    AGE
       TYPE           " SKIP(0)
		    "--------------------------------------------------------------------------------" SKIP(0) WITH NO-BOX.
		    FIND ACCT-RCV WHERE ACCT-RCV.CUST# = W-CUST# AND
			ACCT-RCV.COMP# = XCOM AND ACCT-RCV.DIV# = XDIV NO-ERROR.
		    IF NOT AVAILABLE ACCT-RCV THEN LEAVE.
		    DISPLAY W-CUST#
			    ACCT-RCV.C-NAME AT 16
			    W-CHECK# AT 45
			    W-T-AMT AT 63 WITH NO-LABELS.
		    DISPLAY W-T-DATE
			    W-REFER AT 13
			    W-INV-NO AT 29
			    W-SLS# AT 44
			    W-AGE AT 53
			    W-T-TYPE AT 63
					  SKIP(0) "--------------------------------------------------------------------------------" WITH NO-LABELS.
END.
