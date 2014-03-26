SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO


CREATE	proc dbo.usp_QoS_DetermineDates
	@CustID INT
, 	@PreAdviceDate NVARCHAR(10) 
	

as

SET NOCOUNT ON

ALTER  TABLE #PreAdviceDay (
	  PreAdviceDay CHAR(10)
	, DayC DATETIME
	, DayD DATETIME
	, WireNumber CHAR(4)
	, EVDACDays INT
	, DayCOutofAreaExtension INT
	, DayDOutofAreaExtension INT
	, NoSatDelivery BIT
	, OriginalDayC INT
	, OriginalDayD INT
	)

--SET @CustID = 3
--SET @PreAdviceDate = '2009-03-05'

DECLARE @sql NVARCHAR(4000), @date_tmp DATETIME, @PreAdviceReportingDays INT
DECLARE @DayCTime DATETIME, @DayDTime DATETIME, @WireNumber CHAR(4)

SELECT TOP 1 @date_tmp = DatabaseDateTime FROM EventsDB_MaxTimestamp  --2009-12-03 09:30:04

SELECT @WireNumber = WireNumber FROM dbo.Customer WHERE [ID] = @CustID

IF DATEPART(hh,@date_tmp) < 8 
BEGIN
	SET @date_tmp = DATEADD(dd, (-1), @date_tmp) 
END

SET @PreAdviceReportingDays = 0

IF (@PreAdviceDate = '' OR @PreAdviceDate IS NULL)
BEGIN
	
	INSERT INTO #PreAdviceDay
	SELECT 
	  LEFT(CONVERT(NVARCHAR,DATEADD(dd,(-PreAdviceDay),@date_tmp),121),10) 'PreAdviceDay'
	, CONVERT(DATETIME,LEFT(CONVERT(NVARCHAR,DATEADD(dd,(-DayC),@date_tmp),121),10)+' '+DayCTime+'.000',121) 'DayC'
	, CONVERT(DATETIME,LEFT(CONVERT(NVARCHAR,DATEADD(dd,(-DayD),@date_tmp),121),10)+' '+DayDTime+'.000',121) 'DayD'
	, @WireNumber 'WireNumber'
	, EVDACDay
	, DayCOutofAreaExtension
	, DayDOutofAreaExtension
	, NoSatDelivery
	, DayC
	, DayD
	FROM dbo.tblQoSReportingDays (NOLOCK) 
	WHERE ReportDay = DATENAME(dw,@date_tmp) 
	AND CustomerID = @CustID

	SET @PreAdviceReportingDays = @@ROWCOUNT
	SELECT TOP 1 @PreAdviceDate = PreAdviceDay FROM #PreAdviceDay
END
ELSE
BEGIN

	INSERT INTO #PreAdviceDay
	SELECT 
	  @PreAdviceDate 'PreAdviceDay'
	, CONVERT(DATETIME,LEFT(CONVERT(NVARCHAR,DATEADD(dd,PreAdviceDay,DATEADD(dd,(-DayC),@PreAdviceDate)),121),10)+' '+DayCTime+'.000',121) 'DayC'
	, CONVERT(DATETIME,LEFT(CONVERT(NVARCHAR,DATEADD(dd,PreAdviceDay,DATEADD(dd,(-DayD),@PreAdviceDate)),121),10)+' '+DayDTime+'.000',121) 'DayD'
	, @WireNumber 'WireNumber'
	, EVDACDay
	, DayCOutofAreaExtension
	, DayDOutofAreaExtension
	, NoSatDelivery
	, DayC
	, DayD
	FROM dbo.tblQoSReportingDays (NOLOCK) 
	WHERE PreAdvicedayOfWeek = DATENAME(dw,CONVERT(DATETIME,@PreAdviceDate,121))  
	AND CustomerID = @CustID

	SET @PreAdviceReportingDays = @@ROWCOUNT
END


--Calculate affect of any Bank Holidays
DECLARE @OldDayC CHAR(10), @OldDayD CHAR(10), @DayC INT, @DayD INT, @DayCExt INT, @DayDExt INT, @NewDayC DATETIME, @NewDayD DATETIME, @NewDayCExt INT, @NewDayDExt INT, @i INT
DECLARE @OriginalDayC INT, @OriginalDayD INT

SELECT @DayC =SUM(DaysToAdd)
FROM #PreAdviceDay T (NOLOCK) 
JOIN dbo.tblQoSReporting_BankHolidays BH_C ON BH_C.BankHolidayDate BETWEEN @PreAdviceDate AND DayC
WHERE PreAdviceDay = @PreAdviceDate
AND WireNumber = @WireNumber

SELECT @DayD = SUM(DaysToAdd)
FROM #PreAdviceDay T (NOLOCK) 
JOIN dbo.tblQoSReporting_BankHolidays BH_C ON BH_C.BankHolidayDate BETWEEN @PreAdviceDate AND DayD
WHERE PreAdviceDay = @PreAdviceDate
AND WireNumber = @WireNumber

--Display results prior to Bank Holiday calculations
--SELECT  PreAdviceDay, DayC, DayD , WireNumber , EVDACDays , DayCOutofAreaExtension , DayDOutofAreaExtension FROM #PreAdviceDay

SET @NewDayCExt = 0
SET @NewDayDExt = 0

IF @DayC > 0
BEGIN
	WHILE @DayC > 0
	BEGIN
	
		SELECT @OldDayC = LEFT(CONVERT(NVARCHAR, DayC, 121),10) 
		, @DayCExt = DayCOutofAreaExtension
		FROM #PreAdviceDay (NOLOCK)
		WHERE PreAdviceDay = @PreAdviceDate
		AND WireNumber = @WireNumber
		
		SELECT @NewDayC = CASE 
			WHEN NoSatDelivery = 1
			AND DayC = DayD
			AND DATENAME(dw, DATEADD(dd, @DayC, DayC)) = 'Sunday'
				THEN DATEADD(dd, (@DayC+2), DayC)
			WHEN NoSatDelivery = 1
			AND DATENAME(dw, DATEADD(dd, @DayC, DayC)) = 'Saturday'
				THEN DATEADD(dd, (@DayC+2), DayC)

			WHEN DATENAME(dw, DATEADD(dd, @DayC, DayC)) = 'Sunday' 

				THEN DATEADD(dd, (@DayC+1), DayC) 
			ELSE DATEADD(dd, @DayC, DayC) END
		FROM #PreAdviceDay (NOLOCK)
		WHERE PreAdviceDay = @PreAdviceDate
		AND WireNumber = @WireNumber
	
		IF @DayCExt > 0
		BEGIN

			SELECT @NewDayCExt = 
			CASE 
			WHEN DATENAME(dw, DATEADD(dd, @DayCExt, @NewDayC)) = 'Sunday'
				THEN @DayCExt + 1
			WHEN @DayCExt = 2
				THEN @DayCExt - 1
			ELSE
				@DayCExt
			END
		
			SELECT @i = COUNT(*) FROM dbo.tblQoSReporting_BankHolidays BH_C (NOLOCK)
			WHERE BH_C.BankHolidayDate = LEFT(CONVERT(NVARCHAR, DATEADD(dd, @NewDayCExt, @NewDayC), 121),10)
			
			WHILE @i > 0
			BEGIN
				SET @NewDayCExt = @NewDayCExt + 1
		
				SELECT @i = COUNT(*) FROM dbo.tblQoSReporting_BankHolidays BH_C (NOLOCK)
				WHERE BH_C.BankHolidayDate = LEFT(CONVERT(NVARCHAR, DATEADD(dd, @NewDayCExt, @NewDayC), 121),10)
			END
		END
	
		UPDATE #PreAdviceDay
		SET DayC = @NewDayC
		, DayCOutofAreaExtension = @NewDayCExt
		WHERE PreAdviceDay = @PreAdviceDate
		AND WireNumber = @WireNumber
		
		SELECT @DayC = SUM(DaysToAdd)
		FROM #PreAdviceDay T (NOLOCK)
--		JOIN dbo.tblQoSReporting_BankHolidays BH_C ON BH_C.BankHolidayDate = LEFT(CONVERT(NVARCHAR, DayC, 121),10)
		JOIN dbo.tblQoSReporting_BankHolidays BH_C (NOLOCK) ON BH_C.BankHolidayDate 
			BETWEEN LEFT(CONVERT(NVARCHAR,DATEADD(dd,1,@OldDayC),121),10) 
			AND LEFT(CONVERT(NVARCHAR, DayC, 121),10)
		WHERE PreAdviceDay = @PreAdviceDate
		AND WireNumber = @WireNumber
	
	END
END
ELSE
BEGIN
-- Day C not changed - but Day C Extension maybe affected
	SELECT @i = SUM(DaysToAdd)
	FROM #PreAdviceDay T (NOLOCK) 
	JOIN dbo.tblQoSReporting_BankHolidays BH_C (NOLOCK) 
	ON BH_C.BankHolidayDate = LEFT(CONVERT(NVARCHAR, DATEADD(dd, DayCOutofAreaExtension, DayC), 121),10)	
	WHERE PreAdviceDay = @PreAdviceDate
	AND WireNumber = @WireNumber
	
	SELECT @DayCExt = DayCOutofAreaExtension
	FROM #PreAdviceDay (NOLOCK)
	WHERE PreAdviceDay = @PreAdviceDate
	AND WireNumber = @WireNumber
	
	IF @i > 0 AND @DayCExt > 0	
	BEGIN
		WHILE @i > 0
		BEGIN
			SELECT @NewDayCExt = CASE WHEN DATENAME(dw, DATEADD(dd, @i + DayCOutofAreaExtension, DayC )) = 'Sunday'
					THEN @i + DayCOutofAreaExtension + 1
					ELSE @i + DayCOutofAreaExtension END
			FROM #PreAdviceDay T (NOLOCK) 
			WHERE PreAdviceDay = @PreAdviceDate
			AND WireNumber = @WireNumber
	
			-- B.Dutton 30/12/2013 : "@i + SUM(DaysToAdd)" from "SUM(DaysToAdd)"
			SELECT @i = @i + SUM(DaysToAdd)
			FROM #PreAdviceDay T (NOLOCK) 
			JOIN dbo.tblQoSReporting_BankHolidays BH_C (NOLOCK) 
			ON BH_C.BankHolidayDate = LEFT(CONVERT(NVARCHAR, DATEADD(dd, @NewDayCExt, DayC), 121),10)	
			WHERE PreAdviceDay = @PreAdviceDate
			AND WireNumber = @WireNumber
	
		END
	
		UPDATE #PreAdviceDay
		SET DayCOutofAreaExtension = @NewDayCExt
		WHERE PreAdviceDay = @PreAdviceDate
		AND WireNumber = @WireNumber
	END
END

print('@DayD ' + CAST(@DayD AS VARCHAR))
IF @DayD > 0
BEGIN
	WHILE @DayD > 0
	BEGIN
		SELECT @OldDayD = LEFT(CONVERT(NVARCHAR, DayD, 121),10)
		, @DayDExt = DayDOutofAreaExtension
		, @OriginalDayC = OriginalDayC
		, @OriginalDayD = OriginalDayD
		FROM #PreAdviceDay (NOLOCK)
		WHERE PreAdviceDay = @PreAdviceDate
		AND WireNumber = @WireNumber
		print('@OldDayD ' + @OldDayD)
		print('@OldDayC ' + @OldDayC)
		print('@OriginalDayC ' + CAST(@OriginalDayC AS NVARCHAR))
		print('@OriginalDayD ' + CAST(@OriginalDayD AS NVARCHAR))
	
		SELECT @NewDayD = CASE
			WHEN DayD < DayC
			AND @OriginalDayC <> @OriginalDayD
			AND DATENAME(dw, DATEADD(dd, (1), dayC)) = 'Sunday'
				THEN DATEADD(dd, (2), LEFT(CONVERT(NVARCHAR,dayC,121),10)+RIGHT(CONVERT(NVARCHAR,dayD,121),13) )
			WHEN DayD < DayC
			AND @OriginalDayC <> @OriginalDayD
				THEN DATEADD(dd, (1), LEFT(CONVERT(NVARCHAR,dayC,121),10)+RIGHT(CONVERT(NVARCHAR,dayD,121),13) )
			WHEN DayD < DayC
			AND @OriginalDayC = @OriginalDayD
				THEN DATEADD(dd, (0), LEFT(CONVERT(NVARCHAR,dayC,121),10)+RIGHT(CONVERT(NVARCHAR,dayD,121),13) )

			WHEN NoSatDelivery = 1

			AND DATENAME(dw, DATEADD(dd, @DayD, DayD)) = 'Sunday'
				THEN DATEADD(dd, (@DayD+2), DayD)

			WHEN DATENAME(dw, DATEADD(dd, @DayD, DayD)) = 'Sunday' 
				THEN DATEADD(dd, (@DayD+1), DayD) 
			ELSE DATEADD(dd, @DayD, DayD) END
		, @OldDayD = CASE 
			WHEN DayD < DayC
				THEN @OldDayC
			ELSE  @OldDayD END
		FROM #PreAdviceDay (NOLOCK)
		WHERE PreAdviceDay = @PreAdviceDate
		AND WireNumber = @WireNumber
	
		IF @DayDExt > 0
		BEGIN
			SELECT @NewDayDExt = 
			CASE 
			WHEN DATENAME(dw, DATEADD(dd, @DayDExt, @NewDayD)) = 'Sunday'
				THEN @DayDExt + 1
			WHEN @DayDExt = 2
				THEN @DayDExt - 1
			ELSE
				@DayDExt
			END
		
			SELECT @i = COUNT(*) FROM dbo.tblQoSReporting_BankHolidays BH_C (NOLOCK)
			WHERE BH_C.BankHolidayDate = LEFT(CONVERT(NVARCHAR, DATEADD(dd, @NewDayDExt, @NewDayD), 121),10)
			
			WHILE @i > 0
			BEGIN
				SET @NewDayDExt = @NewDayDExt + 1
		
				SELECT @i = COUNT(*) FROM dbo.tblQoSReporting_BankHolidays BH_C (NOLOCK)
				WHERE BH_C.BankHolidayDate = LEFT(CONVERT(NVARCHAR, DATEADD(dd, @NewDayDExt, @NewDayD), 121),10)
			END
		END
	
		UPDATE #PreAdviceDay
		SET DayD = @NewDayD
		, DayDOutofAreaExtension = @NewDayDExt
		WHERE PreAdviceDay = @PreAdviceDate
		AND WireNumber = @WireNumber
	
		SELECT @DayD =SUM(DaysToAdd)
		FROM #PreAdviceDay T (NOLOCK) 
--		JOIN dbo.tblQoSReporting_BankHolidays BH_C ON BH_C.BankHolidayDate = LEFT(CONVERT(NVARCHAR, DayD, 121),10)
		JOIN dbo.tblQoSReporting_BankHolidays BH_C (NOLOCK) ON BH_C.BankHolidayDate 
			BETWEEN LEFT(CONVERT(NVARCHAR,DATEADD(dd,1,@OldDayD),121),10) 
			AND LEFT(CONVERT(NVARCHAR, DayD, 121),10)
		WHERE PreAdviceDay = @PreAdviceDate
		AND WireNumber = @WireNumber
		print('@DayD ' + CAST(@DayD AS NVARCHAR))
		print('@OldDayD ' + CAST(@OldDayD AS NVARCHAR))
		print('@NewDayD ' + CAST(@NewDayD AS NVARCHAR))
		print('@PreAdviceDate ' + CAST(@PreAdviceDate AS NVARCHAR))
		print('@WireNumber ' + @WireNumber)
		print('**')
	END
END
ELSE
BEGIN
-- Day D not changed - but Day D Extension maybe affected
	SELECT @i = SUM(DaysToAdd)
	FROM #PreAdviceDay T (NOLOCK) 
	JOIN dbo.tblQoSReporting_BankHolidays BH_C (NOLOCK) ON BH_C.BankHolidayDate = LEFT(CONVERT(NVARCHAR, DATEADD(dd, DayDOutofAreaExtension, DayD), 121),10)	
	WHERE PreAdviceDay = @PreAdviceDate
	AND WireNumber = @WireNumber
	
	SELECT @DayDExt = DayDOutofAreaExtension
	FROM #PreAdviceDay (NOLOCK)
	WHERE PreAdviceDay = @PreAdviceDate
	AND WireNumber = @WireNumber

	IF @i > 0 AND @DayDExt > 0
	BEGIN
		WHILE @i > 0
		BEGIN
			SELECT @NewDayDExt = CASE WHEN DATENAME(dw, DATEADD(dd, @i + DayDOutofAreaExtension, DayD )) = 'Sunday'
					THEN @i + DayDOutofAreaExtension + 1
					ELSE @i + DayDOutofAreaExtension END
			FROM #PreAdviceDay T 
			WHERE PreAdviceDay = @PreAdviceDate
			AND WireNumber = @WireNumber
	
			-- B.Dutton 30/12/2013 : "@i + SUM(DaysToAdd)" from "SUM(DaysToAdd)"
			SELECT @i = @i + SUM(DaysToAdd)
			FROM #PreAdviceDay T (NOLOCK) 
			JOIN dbo.tblQoSReporting_BankHolidays BH_C  (NOLOCK)
			ON BH_C.BankHolidayDate = LEFT(CONVERT(NVARCHAR, DATEADD(dd, @NewDayDExt, DayD), 121),10)	
			WHERE PreAdviceDay = @PreAdviceDate
			AND WireNumber = @WireNumber
	
		END
	
		UPDATE #PreAdviceDay
		SET DayDOutofAreaExtension = @NewDayDExt
		WHERE PreAdviceDay = @PreAdviceDate
		AND WireNumber = @WireNumber
	END
END



--temp override for ASDA fudge, B.Dutton 15-04-2009
IF @CustID =11
BEGIN

	IF @PreAdviceDate = '2009-04-24'
	BEGIN
		UPDATE #PreAdviceDay
		SET DayC = '2009-12-30 22:00:00.000'
		, DayD = '2009-12-31 08:30:00.000'
		, DayCOutofAreaExtension = 0
		, DayDOutofAreaExtension = 0
	END

	IF @PreAdviceDate = '2009-04-29'
	BEGIN
		UPDATE #PreAdviceDay
		SET DayC = '2009-12-31 22:00:00.000'
		, DayD = '2010-01-02 08:30:00.000'
		, DayCOutofAreaExtension = 1
		, DayDOutofAreaExtension = 1
	END

END
--Return Resultset
SELECT	  PreAdviceDay
	, DayC 
	, DayD 
	, WireNumber 
	, EVDACDays
	, DayCOutofAreaExtension 
	, DayDOutofAreaExtension
FROM #PreAdviceDay



DROP TABLE #PreAdviceDay



















GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO
