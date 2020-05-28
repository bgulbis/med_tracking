SELECT DISTINCT
	TO_CHAR(TRUNC(pi_from_gmt(ORDERS.ORIG_ORDER_DT_TM, 'America/Chicago'), 'MONTH'), 'YYYY-MM-DD') AS EVENT_DATE,
	CV_ENCOUTER_TYPE.DISPLAY AS ENCOUNTER_TYPE,
	CV_FACILITY.DISPLAY AS FACILITY,
	ORDERS.HNA_ORDER_MNEMONIC AS MEDICATION,
	COUNT(DISTINCT ORDERS.ORDER_ID) AS DOSES,
	COUNT(DISTINCT ORDERS.ENCNTR_ID) AS PATIENTS
FROM
	CODE_VALUE CV_ENCOUTER_TYPE,
	CODE_VALUE CV_FACILITY,
	ENCNTR_LOC_HIST,
	ENCOUNTER,
	ORDERS
WHERE
	ORDERS.ACTIVE_IND = 1
	AND ORDERS.CATALOG_CD = 750915114 -- bupivacaine liposome
	AND ORDERS.CATALOG_TYPE_CD = 1363 -- Pharmacy
	AND ORDERS.ORIG_ORD_AS_FLAG = 0
	AND	ORDERS.TEMPLATE_ORDER_FLAG IN (0, 1)
	AND ORDERS.TEMPLATE_ORDER_ID = 0
	AND (
		ORDERS.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
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
		ORDERS.ENCNTR_ID = ENCNTR_LOC_HIST.ENCNTR_ID
		AND ENCNTR_LOC_HIST.ACTIVE_IND = 1
		AND ENCNTR_LOC_HIST.TRANSACTION_DT_TM <= ORDERS.ORIG_ORDER_DT_TM
		AND (ORDERS.ORIG_ORDER_DT_TM BETWEEN ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM AND ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM)
		AND ENCNTR_LOC_HIST.TRANSACTION_DT_TM = (
			SELECT MAX(ELH.TRANSACTION_DT_TM)
			FROM ENCNTR_LOC_HIST ELH
			WHERE
				ELH.TRANSACTION_DT_TM <= ORDERS.ORIG_ORDER_DT_TM
				AND ORDERS.ENCNTR_ID = ELH.ENCNTR_ID
				AND ELH.ACTIVE_IND = 1
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
	AND	(
		ORDERS.ORIG_ORDER_DT_TM + 0
			BETWEEN DECODE(
				@Prompt('Choose date range', 'A', {'Today', 'Yesterday', 'Week to Date', 'Last Week', 'Last Month', 'Month to Date', 'User-defined', 'N Days Prior'}, mono, free, , , User:79),
				'Today', pi_to_gmt(TRUNC(SYSDATE), 'America/Chicago'),
				'Yesterday', pi_to_gmt(TRUNC(SYSDATE) - 1, 'America/Chicago'),
				'Week to Date', pi_to_gmt(TRUNC(SYSDATE, 'DAY'), 'America/Chicago'),
				'Last Week', pi_to_gmt(TRUNC(SYSDATE - 7, 'DAY'), 'America/Chicago'),
				'Last Month', pi_to_gmt(TRUNC(ADD_MONTHS(SYSDATE, -1), 'MONTH'), 'America/Chicago'),
				'Month to Date', pi_to_gmt(TRUNC(SYSDATE-1, 'MONTH'), 'America/Chicago'),
				'User-defined', pi_to_gmt(
					TO_DATE(
						@Prompt('Enter begin date (Leave as 01/01/1800 if using a Relative Date)', 'D', , mono, free, persistent, {'01/01/1800 00:00:00'}, User:80),
						pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
					),
					pi_time_zone(1, @Variable('BOUSER'))),
				'N Days Prior', pi_to_gmt(SYSDATE - @Prompt('Days Prior to Now', 'N', , mono, free, persistent, {'0'}, User:2080), 'America/Chicago')
			)
			AND DECODE(
				@Prompt('Choose date range', 'A', {'Today', 'Yesterday', 'Week to Date', 'Last Week', 'Last Month', 'Month to Date', 'User-defined', 'N Days Prior'}, mono, free, , , User:79),
				'Today', pi_to_gmt(TRUNC(SYSDATE) + (86399 / 86400), 'America/Chicago'),
				'Yesterday', pi_to_gmt(TRUNC(SYSDATE) - (1 / 86400), 'America/Chicago'),
				'Week to Date', pi_to_gmt(TRUNC(SYSDATE) - (1 / 86400), 'America/Chicago'),
				'Last Week', pi_to_gmt(TRUNC(SYSDATE, 'DAY') - (1 / 86400), 'America/Chicago'),
				'Last Month', pi_to_gmt(TRUNC(SYSDATE, 'MONTH') - (1 / 86400), 'America/Chicago'),
				'Month to Date', pi_to_gmt(TRUNC(SYSDATE) - (1 / 86400), 'America/Chicago'),
				'User-defined', pi_to_gmt(
					TO_DATE(
						@Prompt('Enter end date (Leave as 01/01/1800 if using a Relative Date)', 'D', , mono, free, persistent, {'01/01/1800 23:59:59'}, User:81),
						pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
					),
					pi_time_zone(1, @Variable('BOUSER'))),
				'N Days Prior', pi_to_gmt(SYSDATE, 'America/Chicago')
			)
		AND ORDERS.ORIG_ORDER_DT_TM
			BETWEEN DECODE(
				@Prompt('Choose date range', 'A', {'Today', 'Yesterday', 'Week to Date', 'Last Week', 'Last Month', 'Month to Date', 'User-defined', 'N Days Prior'}, mono, free, , , User:79),
				'Today', TRUNC(SYSDATE),
				'Yesterday', TRUNC(SYSDATE) - 1,
				'Week to Date', TRUNC(SYSDATE, 'DAY'),
				'Last Week', TRUNC(SYSDATE - 7, 'DAY'),
				'Last Month', TRUNC(ADD_MONTHS(SYSDATE, -1), 'MONTH'),
				'Month to Date', TRUNC(SYSDATE-1, 'MONTH'),
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
    TRUNC(pi_from_gmt(ORDERS.ORIG_ORDER_DT_TM, 'America/Chicago'), 'MONTH'),
	CV_ENCOUTER_TYPE.DISPLAY,
	CV_FACILITY.DISPLAY,
	ORDERS.HNA_ORDER_MNEMONIC
