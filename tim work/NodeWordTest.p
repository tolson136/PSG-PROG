DEFINE VARIABLE chWord          AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chDocument      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chMerge         AS COM-HANDLE NO-UNDO.



//OS-COMMAND SILENT "node c:\nodeproj\docxtemplater\\ticketsnode c:/psg-node/psg-node/TicketData.csv c:/psg-prog/template/tickets2upnode.docx c:/lasertickets/test.docx > c:/lasertickets/ticketsnode.err".
OS-COMMAND SILENT "node c:\nodeproj\docxtemplater\\ticketsnode c:/lasertickets//TicketData.csv c:/psg-prog/template/tickets2upnode.docx c:/lasertickets/test.docx > c:/lasertickets/ticketsnode.err".

CREATE "Word.Application" chWord.
chWord:VISIBLE=TRUE.


/*chDocument  = chWord:Document.*/
chDocument = chWord:documents:open('c:/lasertickets/test.docx').

