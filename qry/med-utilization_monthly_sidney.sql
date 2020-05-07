SELECT DISTINCT
	TO_CHAR(TRUNC(pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, 'CST'), 'MONTH'), 'YYYY-MM-DD') AS EVENT_DATE,
	CV_ENCOUTER_TYPE.DISPLAY AS ENCOUNTER_TYPE,
	CV_FACILITY.DISPLAY AS FACILITY,
	CV_EVENT.DISPLAY AS MEDICATION,
	COUNT(DISTINCT CLINICAL_EVENT.EVENT_ID) AS DOSES,
	COUNT(DISTINCT CLINICAL_EVENT.ENCNTR_ID) AS PATIENTS
FROM
	CE_MED_RESULT,
	CLINICAL_EVENT,
	CODE_VALUE CV_ENCOUTER_TYPE,
	CODE_VALUE CV_EVENT,
	CODE_VALUE CV_FACILITY,
	ENCNTR_LOC_HIST,
	ENCOUNTER
WHERE
	CLINICAL_EVENT.EVENT_CD IN (
		37556009, -- acetaminophen
		37556051, -- albumin human
		37556378, -- calcitonin
		627676649, -- ceftaroline
		1505894070, -- ceftazidime-avibactam
		1370510034, -- ceftolozane-tazobactam
		37556681, -- daptomycin
		222403180, -- eculizumab
		117038716, -- ertapenem
		37557233, -- immune globulin intravenous
		609787708, -- immune globulin intravenous and subcut
		37557342, -- isoproterenol
		37557425, -- levothyroxine
		2737463843, -- meropenem-vaborbactam
		37557675, -- niCARdipine
		37557776, -- pegfilgrastim
		1895018730 -- sugammadex
	)
	AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
    AND CLINICAL_EVENT.EVENT_CD = CV_EVENT.CODE_VALUE
	AND (
		CLINICAL_EVENT.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
		AND ENCOUNTER.ACTIVE_IND = 1
		AND ENCOUNTER.LOC_FACILITY_CD IN (
			3310, -- HH HERMANN
			3796, -- HC Childrens
			3821, -- HH Clinics
			3822, -- HH Trans Care
			3823, -- HH Rehab
			1099966301 -- HH Oncology TMC
		)
		AND ENCOUNTER.ENCNTR_TYPE_CD = CV_ENCOUTER_TYPE.CODE_VALUE
	)
	AND (
		CLINICAL_EVENT.ENCNTR_ID = ENCNTR_LOC_HIST.ENCNTR_ID
		AND ENCNTR_LOC_HIST.ACTIVE_IND = 1
		AND CLINICAL_EVENT.EVENT_END_DT_TM >= ENCNTR_LOC_HIST.TRANSACTION_DT_TM
		AND CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM AND ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM
		AND ENCNTR_LOC_HIST.TRANSACTION_DT_TM = (
			SELECT MAX(ELH.TRANSACTION_DT_TM)
			FROM ENCNTR_LOC_HIST ELH
			WHERE
				CLINICAL_EVENT.ENCNTR_ID = ELH.ENCNTR_ID
				AND ELH.ACTIVE_IND = 1
				AND CLINICAL_EVENT.EVENT_END_DT_TM >= ELH.TRANSACTION_DT_TM
		)
		AND ENCNTR_LOC_HIST.LOC_FACILITY_CD IN (
			3310, -- HH HERMANN
			3796, -- HC Childrens
			3821, -- HH Clinics
			3822, -- HH Trans Care
			3823, -- HH Rehab
			1099966301 -- HH Oncology TMC
		)
		AND ENCNTR_LOC_HIST.LOC_FACILITY_CD = CV_FACILITY.CODE_VALUE
	)
	AND (
		CLINICAL_EVENT.EVENT_ID = CE_MED_RESULT.EVENT_ID
		AND (
			CE_MED_RESULT.ADMIN_DOSAGE > 0 
			OR CE_MED_RESULT.IV_EVENT_CD = 688706 -- Begin Bag
		)
	)
	AND (
	    CLINICAL_EVENT.EVENT_CD NOT IN (37556009, 37557425) -- (acetaminophen, levothyroxine)
	    OR CE_MED_RESULT.ADMIN_ROUTE_CD IN (508984, 9022513, 9022647, 9022649) -- (IV, INJ, IVPB, IVP)
	)
	AND (
		CLINICAL_EVENT.EVENT_END_DT_TM + 0
			BETWEEN DECODE(
				@Prompt('Choose date range', 'A', {'Today', 'Yesterday', 'Week to Date', 'Last Week', 'Last Month', 'Month to Date', 'User-defined', 'N Days Prior'}, mono, free, , , User:79),
				'Today', pi_to_gmt(TRUNC(SYSDATE), 'CST'),
				'Yesterday', pi_to_gmt(TRUNC(SYSDATE) - 1, 'CST'),
				'Week to Date', pi_to_gmt(TRUNC(SYSDATE, 'DAY'), 'CST'),
				'Last Week', pi_to_gmt(TRUNC(SYSDATE - 7, 'DAY'), 'CST'),
				'Last Month', pi_to_gmt(TRUNC(ADD_MONTHS(SYSDATE, -1), 'MONTH'), 'CST'),
				'Month to Date', pi_to_gmt(TRUNC(SYSDATE - 1, 'MONTH'), 'CST'),
				'User-defined', pi_to_gmt(
					TO_DATE(
						@Prompt('Enter begin date (Leave as 01/01/1800 if using a Relative Date)', 'D', , mono, free, persistent, {'01/01/1800 00:00:00'}, User:80),
						pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
					),
					pi_time_zone(1, @Variable('BOUSER'))),
				'N Days Prior', pi_to_gmt(SYSDATE - @Prompt('Days Prior to Now', 'N', , mono, free, persistent, {'0'}, User:2080), 'CST')
			)
			AND DECODE(
				@Prompt('Choose date range', 'A', {'Today', 'Yesterday', 'Week to Date', 'Last Week', 'Last Month', 'Month to Date', 'User-defined', 'N Days Prior'}, mono, free, , , User:79),
				'Today', pi_to_gmt(TRUNC(SYSDATE) + (86399 / 86400), 'CST'),
				'Yesterday', pi_to_gmt(TRUNC(SYSDATE) - (1 / 86400), 'CST'),
				'Week to Date', pi_to_gmt(TRUNC(SYSDATE) - (1 / 86400), 'CST'),
				'Last Week', pi_to_gmt(TRUNC(SYSDATE, 'DAY') - (1 / 86400), 'CST'),
				'Last Month', pi_to_gmt(TRUNC(SYSDATE, 'MONTH') - (1 / 86400), 'CST'),
				'Month to Date', pi_to_gmt(TRUNC(SYSDATE) - (1 / 86400), 'CST'),
				'User-defined', pi_to_gmt(
					TO_DATE(
						@Prompt('Enter end date (Leave as 01/01/1800 if using a Relative Date)', 'D', , mono, free, persistent, {'01/01/1800 23:59:59'}, User:81),
						pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
					),
					pi_time_zone(1, @Variable('BOUSER'))),
				'N Days Prior', pi_to_gmt(SYSDATE, 'CST')
			)
		AND CLINICAL_EVENT.EVENT_END_DT_TM
			BETWEEN DECODE(
				@Prompt('Choose date range', 'A', {'Today', 'Yesterday', 'Week to Date', 'Last Week', 'Last Month', 'Month to Date', 'User-defined', 'N Days Prior'}, mono, free, , , User:79),
				'Today', TRUNC(SYSDATE),
				'Yesterday', TRUNC(SYSDATE) - 1,
				'Week to Date', TRUNC(SYSDATE, 'DAY'),
				'Last Week', TRUNC(SYSDATE - 7, 'DAY'),
				'Last Month', TRUNC(ADD_MONTHS(SYSDATE, -1), 'MONTH'),
				'Month to Date', TRUNC(SYSDATE - 1, 'MONTH'),
				'User-defined', DECODE(
					@Prompt('Enter begin date (Leave as 01/01/1800 if using a Relative Date)', 'D', , mono, free, persistent, {'01/01/1800 00:00:00'}, User:80),
					'01/01/1800 00:00:00',
					'',
					@Variable('Enter begin date (Leave as 01/01/1800 if using a Relative Date)')
				),
				'N Days Prior', SYSDATE - @Prompt('Days Prior to Now', 'N', , mono, free, persistent, {0}, User:2080)
			) - 1
			AND DECODE(
				@Prompt('Choose date range', 'A', {'Today', 'Yesterday', 'Week to Date', 'Last Week', 'Last Month', 'Month to Date', 'User-defined', 'N Days Prior'}, mono, free, , , User:79),
				'Today', TRUNC(SYSDATE) + (86399 / 86400),
				'Yesterday', TRUNC(SYSDATE) - (1 / 86400),
				'Week to Date', TRUNC(SYSDATE) - (1 / 86400),
				'Last Week', TRUNC(SYSDATE, 'DAY') - (1 / 86400),
				'Last Month', TRUNC(SYSDATE, 'MONTH') - (1 / 86400),
				'Month to Date', TRUNC(SYSDATE) - (1 / 86400),
				'User-defined', DECODE(
					@Prompt('Enter end date (Leave as 01/01/1800 if using a Relative Date)', 'D', , mono, free, persistent, {'01/01/1800 23:59:59'}, User:81),
					'01/01/1800 00:00:00',
					'',
					@Variable('Enter end date (Leave as 01/01/1800 if using a Relative Date)')
				),
				'N Days Prior', SYSDATE
			) + 1
	)
GROUP BY
    TRUNC(pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, 'CST'), 'MONTH'),
	CV_ENCOUTER_TYPE.DISPLAY,
	CV_FACILITY.DISPLAY,
	CV_EVENT.DISPLAY
