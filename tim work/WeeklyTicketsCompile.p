{slibooxml/slibdocx.i} 
 
 
 
do on error undo, leave: 
 
    etime(yes). 
 
    run docx_compile(         "c:\psg-prog\template\tickets2up.docx",         "c:\psg-prog\template\tickets2up.dfw"). 
 
    message "Completed (in" round(etime(no) / 1000, 3) "seconds)". 
 
end. /* on error undo, leave */ 
 
if docx_getLastErrorFlag() then 
 
    message docx_getLastErrorDesc() view-as alert-box.
