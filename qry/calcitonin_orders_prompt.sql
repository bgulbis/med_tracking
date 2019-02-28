SELECT DISTINCT
	ENCOUNTER.ENCNTR_ID AS ENCOUNTER_ID,
	ORDERS.ORDER_ID AS ORDER_ID,
	TO_CHAR(pi_from_gmt(ORDERS.ORIG_ORDER_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))), 'YYYY-MM-DD"T"HH24:MI:SS') AS ORDER_DATETIME,
	ORDER_CATALOG.PRIMARY_MNEMONIC AS MED,
	CASE ORDERS.PRN_IND
		WHEN 1 THEN 'TRUE'
		ELSE 'FALSE'
	END AS PRN,
	CV_FACILITY.DISPLAY AS FACILITY,
	CV_NURSE_UNIT.DISPLAY AS NURSE_UNIT,
	CV_MED_SVC.DISPLAY AS MED_SERVICE,
	PRSNL_ORDER_ACTION.NAME_FULL_FORMATTED AS PROVIDER,
	CV_ORD_ACTION_POSITION.DISPLAY AS PROVIDER_POSITION,
	PRSNL_ORDER_ACTION_PERSONNEL.NAME_FULL_FORMATTED AS ENTERED_BY,
	CV_ORD_ACTION_PERSONNEL_POS.DISPLAY AS ENTERED_BY_POSITION,
	CV_ORD_ACTION_COMMUNICATION.DISPLAY AS ORDER_TYPE,
	PRSNL_MD.NAME_FULL_FORMATTED AS ATTENDING_ORDER,
	OD_ROUTE.OE_FIELD_DISPLAY_VALUE AS ROUTE,
	OD_FREQ.OE_FIELD_DISPLAY_VALUE AS FREQ,
	CASE ORDER_REVIEW.REVIEWED_STATUS_FLAG
		WHEN 1 THEN 'Accepted'
		WHEN 2 THEN 'Rejected'
		WHEN 5 THEN 'Reviewed'
	END AS VERIFIED_STATUS
FROM
	CODE_VALUE CV_FACILITY,
	CODE_VALUE CV_MED_SVC,
	CODE_VALUE CV_NURSE_UNIT,
	CODE_VALUE CV_ORD_ACTION_POSITION,
	CODE_VALUE CV_ORD_ACTION_PERSONNEL_POS,
	CODE_VALUE CV_ORD_ACTION_COMMUNICATION,
	ENCNTR_LOC_HIST,
	ENCNTR_PRSNL_RELTN,
	ENCOUNTER,
	ORDERS,
	ORDER_ACTION,
	ORDER_CATALOG,
	ORDER_DETAIL OD_FREQ,
	ORDER_DETAIL OD_ROUTE,
	ORDER_REVIEW,
	PRSNL PRSNL_MD,
	PRSNL PRSNL_ORDER_ACTION,
	PRSNL PRSNL_ORDER_ACTION_PERSONNEL
WHERE
	ORDERS.ACTIVE_IND = 1
	AND ORDERS.CATALOG_CD IN (9904976, 265337)
	AND ORDERS.CATALOG_TYPE_CD = 1363
	AND ORDERS.ORIG_ORD_AS_FLAG = 0
	AND	ORDERS.TEMPLATE_ORDER_FLAG IN (0, 1)
	AND ORDERS.CATALOG_CD = ORDER_CATALOG.CATALOG_CD
	AND (
		ORDERS.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
		AND ORDERS.PERSON_ID = ENCOUNTER.PERSON_ID
		AND ENCOUNTER.ACTIVE_IND = 1
		AND ENCOUNTER.LOC_FACILITY_CD IN (3310, 3796, 3821, 3822, 3823)
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
		AND ENCNTR_LOC_HIST.LOC_FACILITY_CD = CV_FACILITY.CODE_VALUE
		AND ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD = CV_NURSE_UNIT.CODE_VALUE
		AND ENCNTR_LOC_HIST.MED_SERVICE_CD = CV_MED_SVC.CODE_VALUE
	)
	AND (
		ORDERS.ORDER_ID = ORDER_ACTION.ORDER_ID
		AND ORDER_ACTION.ACTION_TYPE_CD = 1376
		AND ORDER_ACTION.ACTION_PERSONNEL_ID = PRSNL_ORDER_ACTION_PERSONNEL.PERSON_ID
		AND ORDER_ACTION.COMMUNICATION_TYPE_CD = CV_ORD_ACTION_COMMUNICATION.CODE_VALUE
		AND ORDER_ACTION.ORDER_PROVIDER_ID = PRSNL_ORDER_ACTION.PERSON_ID
		AND PRSNL_ORDER_ACTION_PERSONNEL.POSITION_CD = CV_ORD_ACTION_PERSONNEL_POS.CODE_VALUE
		AND PRSNL_ORDER_ACTION.POSITION_CD = CV_ORD_ACTION_POSITION.CODE_VALUE
	)
	AND (
		ORDERS.ENCNTR_ID = ENCNTR_PRSNL_RELTN.ENCNTR_ID
		AND ENCNTR_PRSNL_RELTN.ACTIVE_IND = 1
		AND ENCNTR_PRSNL_RELTN.ENCNTR_PRSNL_R_CD = 368029
		AND ENCNTR_PRSNL_RELTN.TRANSACTION_DT_TM <= ORDERS.ORIG_ORDER_DT_TM
		AND (ORDERS.ORIG_ORDER_DT_TM BETWEEN ENCNTR_PRSNL_RELTN.BEG_EFFECTIVE_DT_TM AND ENCNTR_PRSNL_RELTN.END_EFFECTIVE_DT_TM)
		AND ENCNTR_PRSNL_RELTN.TRANSACTION_DT_TM = (
				SELECT MAX(EPR.TRANSACTION_DT_TM)
				FROM ENCNTR_PRSNL_RELTN EPR
				WHERE EPR.TRANSACTION_DT_TM <= ORDERS.ORIG_ORDER_DT_TM
				AND ORDERS.ENCNTR_ID = EPR.ENCNTR_ID
				AND EPR.ENCNTR_PRSNL_R_CD = 368029
				AND EPR.ACTIVE_IND = 1
		)
		AND ENCNTR_PRSNL_RELTN.PRSNL_PERSON_ID = PRSNL_MD.PERSON_ID
	)
	AND (
		ORDERS.ORDER_ID = OD_ROUTE.ORDER_ID
		AND OD_ROUTE.ACTION_SEQUENCE = 1
		AND OD_ROUTE.OE_FIELD_MEANING_ID = 2050
	)
	AND (
		ORDERS.ORDER_ID = OD_FREQ.ORDER_ID(+)
		AND OD_FREQ.ACTION_SEQUENCE(+) = 1
		AND OD_FREQ.OE_FIELD_MEANING_ID(+) = 2011
	)
	AND (
		ORDERS.ORDER_ID = ORDER_REVIEW.ORDER_ID(+)
		AND ORDER_REVIEW.REVIEW_TYPE_FLAG(+) = 3
		AND ORDER_REVIEW.ACTION_SEQUENCE(+) = 1
		AND ORDER_REVIEW.REVIEWED_STATUS_FLAG(+) IN (1, 2, 5)
	)
	AND	(
		ORDERS.ORIG_ORDER_DT_TM + 0
			BETWEEN DECODE(
				@Prompt('Choose date range', 'A', {'Today', 'Yesterday', 'Week to Date', 'Last Week', 'Last Month', 'Month to Date', 'User-defined', 'N Days Prior'}, mono, free, , , User:79),
				'Today', pi_to_gmt(TRUNC(SYSDATE), pi_time_zone(2, @Variable('BOUSER'))),
				'Yesterday', pi_to_gmt(TRUNC(SYSDATE) - 1, pi_time_zone(2, @Variable('BOUSER'))),
				'Week to Date', pi_to_gmt(TRUNC(SYSDATE, 'DAY'), pi_time_zone(2, @Variable('BOUSER'))),
				'Last Week', pi_to_gmt(TRUNC(SYSDATE - 7, 'DAY'), pi_time_zone(2, @Variable('BOUSER'))),
				'Last Month', pi_to_gmt(TRUNC(ADD_MONTHS(SYSDATE, -1), 'MONTH'), pi_time_zone(2, @Variable('BOUSER'))),
				'Month to Date', pi_to_gmt(TRUNC(SYSDATE-1, 'MONTH'), pi_time_zone(2, @Variable('BOUSER'))),
				'User-defined', pi_to_gmt(
					TO_DATE(
						@Prompt('Enter begin date (Leave as 01/01/1800 if using a Relative Date)', 'D', , mono, free, persistent, {'01/01/1800 00:00:00'}, User:80),
						pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
					),
					pi_time_zone(1, @Variable('BOUSER'))),
				'N Days Prior', pi_to_gmt(SYSDATE - @Prompt('Days Prior to Now', 'N', , mono, free, persistent, {'0'}, User:2080), pi_time_zone(2, @Variable('BOUSER')))
			)
			AND DECODE(
				@Prompt('Choose date range', 'A', {'Today', 'Yesterday', 'Week to Date', 'Last Week', 'Last Month', 'Month to Date', 'User-defined', 'N Days Prior'}, mono, free, , , User:79),
				'Today', pi_to_gmt(TRUNC(SYSDATE) + (86399 / 86400), pi_time_zone(2, @Variable('BOUSER'))),
				'Yesterday', pi_to_gmt(TRUNC(SYSDATE) - (1 / 86400), pi_time_zone(2, @Variable('BOUSER'))),
				'Week to Date', pi_to_gmt(TRUNC(SYSDATE) - (1 / 86400), pi_time_zone(2, @Variable('BOUSER'))),
				'Last Week', pi_to_gmt(TRUNC(SYSDATE, 'DAY') - (1 / 86400), pi_time_zone(2, @Variable('BOUSER'))),
				'Last Month', pi_to_gmt(TRUNC(SYSDATE, 'MONTH') - (1 / 86400), pi_time_zone(2, @Variable('BOUSER'))),
				'Month to Date', pi_to_gmt(TRUNC(SYSDATE) - (1 / 86400), pi_time_zone(2, @Variable('BOUSER'))),
				'User-defined', pi_to_gmt(
					TO_DATE(
						@Prompt('Enter end date (Leave as 01/01/1800 if using a Relative Date)', 'D', , mono, free, persistent, {'01/01/1800 23:59:59'}, User:81),
						pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
					),
					pi_time_zone(1, @Variable('BOUSER'))),
				'N Days Prior', pi_to_gmt(SYSDATE, pi_time_zone(2, @Variable('BOUSER')))
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
