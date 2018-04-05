
/********************************************************
StdUtils.i
   Standard utility proceedures to be uncluded as needed
   
   2/9/2017    TCO   Initial implementation

**********************************************************/

FUNCTION DaysInMonth RETURNS INT (
   INPUT InMonth as int,  
   INPUT InYear as int
   ):

   DEF VAR LastDay AS INT.

   IF InMonth LT 12 THEN  
      LastDay = DAY(DATE(InMonth + 1,1,InYear) - 1).
   ELSE
      LastDay = DAY(DATE(2,1,InYear) - 1).  
   RETURN LastDay.  
END FUNCTION. /* DaysInMonth */

FUNCTION ChartoDateTime RETURNS DateTime
   ( INPUT Time12  AS CHAR):  RETURN  datetime("1/1/2000 10:00 AM"). 
   
END FUNCTION. /* ChartoDateTime */


FUNCTION DateTimetoChar RETURNS Char
   ( INPUT Time24    AS DateTime ) :
   define variable t  as integer  no-undo.
   t = INTEGER( mtime(Time24) / 1000).
   RETURN string( t, "hh:mm am" ).
END FUNCTION. /* DateTimeto Char */

/* Takes in a PSG day (Sat = 1) and returns a PSC date (Sun = 1) */
FUNCTION DayPSGtoPSC RETURNS INT
   ( INPUT InDay AS INT): 
   IF InDay = 1 THEN RETURN 7.
   ELSE RETURN InDay - 1.
END. /* Function DayPSGtoPSC */   

/* Returns a date based on WeekPlanner table for a job to run.  */
FUNCTION RunDay RETURNS Date
    (INPUT InYear AS INT,
     INPUT InMonth AS INT,
     INPUT InWeek AS INT,
     INPUT InDay AS INT):
    
    DEF VAR WorkDate AS Date NO-UNDO.
    DEF VAR WorkDay AS INT NO-UNDO.
	DEF VAR MonthLastDay AS INT NO-UNDO.
	DEF VAR DayCount AS INT NO-UNDO.
	DEF VAR WeekCount AS INT NO-UNDO.
	
	/*  Revised to calculate the abolute date to return  - i.e week 1 Tues = 1st tuesday or month */
    
	/* Translate PSG day to PSC day */
	WorkDay = DayPSGtoPSC(InDay).
	
	/* Get num of days in a month */
	MonthLastDay = DaysInMonth(InMonth, InYear).
	
	REPEAT DayCount = 1 TO MonthLastDay:
	   IF WeekDay(DATE(InMonth,DayCount,InYear)) = WorkDay AND 
	      TRUNCATE((DayCount - 1) / 7,0) EQ InWeek - 1  
		  THEN RETURN DATE(InMonth,DayCount,InYear). 
	END. /* Repeat */
	
	/*
    FIND WeekPlanner WHERE 
       WeekPlanner.Year      = InYear  AND
       WeekPlanner.Month     = InMonth AND
       WeekPlanner.MonthWeek = InWeek
       NO-LOCK NO-ERROR.
     IF NOT AVAILABLE WeekPlanner THEN RETURN ERROR.
     REPEAT WorkDate = WeekPlanner.StartDate TO WeekPlanner.EndDate:
	    
        IF WEEKDAY(WorkDate) = InDay THEN RETURN WorkDate.
     END.  
     */

END FUNCTION. /* RunDay */
