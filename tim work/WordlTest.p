DEFINE VARIABLE chWord          AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chDocument      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chMerge         AS COM-HANDLE NO-UNDO.
DEF VAR cmd AS CHAR.

           Cmd = "node c:/nodeproj/docxtemplater/ticketsnode " +
                 "c:/lasertickets/TicketData.csv " +
                 "c:/psg-prog/template/tickets2upnode.docx " + 
                 "c:/lasertickets/test.docx" /*FILENAME*/.
 OS-COMMAND VALUE(cmd).
/*
CREATE "Word.Application" chWord.
chWord:VISIBLE=TRUE.


/*chDocument  = chWord:Document.*/
chDocument = chWord:documents:open('c:\psg-prog\template\Tickets2up.dotm').
chMerge = chDocument:MailMerge.


/*
chWord:Visible = TRUE.

/*chExcel:Application:RUN ("ImportCSVs").*/
*/
*/
