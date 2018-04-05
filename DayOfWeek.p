/* Procedure DayOfWeek 

  Takes Day array and return 3 letter day of week
  
  */
  
DEF INPUT PARAM DAY LIKE pro-desp.wkday.
DEF OUTPUT PARAM WEEKDAY AS CHAR.

IF DAY[1] = YES  THEN WEEKDAY = "1-Sat".
IF DAY[2] = YES  THEN WEEKDAY = WEEKDAY + " " + "2-Sun".
IF DAY[3] = YES  THEN WEEKDAY = WEEKDAY + " " + "3-Mon".
IF DAY[4] = YES  THEN WEEKDAY = WEEKDAY + " " + "4-Tue".
IF DAY[5] = YES  THEN WEEKDAY = WEEKDAY + " " + "5-Wed".
IF DAY[6] = YES  THEN WEEKDAY = WEEKDAY + " " + "6-Thu".
IF DAY[7] = YES  THEN WEEKDAY = WEEKDAY + " " + "7-Fri".

WEEKDAY = TRIM(WEEKDAY).

