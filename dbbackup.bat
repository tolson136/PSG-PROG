FOR %%A IN (%Date%) DO (
    FOR /F "tokens=1-3 delims=/-" %%B in ("%%~A") DO (
        SET Today=%%D%%B%%C
    )
)
7z a QuickBackup%TODAY%.zip *.backup

call c:\openedge116\bin\dbman psg -stop
call timeout 30
call c:\openedge116\bin\probkup c:\psgdb11\psg c:\psgdb11\backup\psg%TODAY%.backup
timeout 120
call c:\openedge116\bin\dbman psg -start
