 DEFINE VARIABLE chExcel          AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkbook       AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorksheet      AS COM-HANDLE NO-UNDO.

CREATE "Excel.Application" chExcel.
chWorkbook  = chExcel:Workbooks.
chExcel:Workbooks:open('c:\rollvol\RollVolTemplate.xlsm').

chExcel:Visible = TRUE.

chExcel:Application:RUN ("ImportCSVs").
