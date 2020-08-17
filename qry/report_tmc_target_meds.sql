WITH DOSES AS (
	SELECT DISTINCT
		CLINICAL_EVENT.ENCNTR_ID,
		CLINICAL_EVENT.PERSON_ID,
		CLINICAL_EVENT.EVENT_END_DT_TM,
		pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, 'America/Chicago') AS DOSE_DATETIME,
		TRUNC(pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, 'America/Chicago'), 'MONTH') AS DOSE_MONTH,
		CLINICAL_EVENT.EVENT_ID,
		CLINICAL_EVENT.EVENT_CD,
		LOWER(pi_get_cv_display(CLINICAL_EVENT.EVENT_CD)) AS MEDICATION,
		ENCNTR_LOC_HIST.LOC_FACILITY_CD,
		pi_get_cv_display(ENCNTR_LOC_HIST.LOC_FACILITY_CD) AS FACILITY,
		ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD,
		pi_get_cv_display(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD) AS NURSE_UNIT,
		ENCNTR_LOC_HIST.MED_SERVICE_CD,
		pi_get_cv_display(ENCNTR_LOC_HIST.MED_SERVICE_CD) AS MED_SERVICE,
		ENCNTR_LOC_HIST.ENCNTR_TYPE_CD,
		pi_get_cv_display(ENCNTR_LOC_HIST.ENCNTR_TYPE_CD) AS ENCNTR_TYPE
	FROM
		CE_MED_RESULT,
		CLINICAL_EVENT,
		ENCNTR_LOC_HIST
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
			37557761, -- pantoprazole
			37557776, -- pegfilgrastim
			1895018730 -- sugammadex
		)
		AND CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN
			DECODE(
				@Prompt('Choose date range', 'A', {'Last Month', 'User-defined'}, mono, free, , , User:0),
				'Last Month', pi_to_gmt(TRUNC(ADD_MONTHS(SYSDATE, -1), 'MONTH'), 'America/Chicago'),
				'User-defined', pi_to_gmt(
					TO_DATE(
						@Prompt('Enter begin date', 'D', , mono, free, persistent, {'01/01/2020 00:00:00'}, User:1),
						pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
					),
					pi_time_zone(1, 'America/Chicago')
				)
			)
			AND DECODE(
				@Prompt('Choose date range', 'A', {'Last Month', 'User-defined'}, mono, free, , , User:0),
				'Last Month', pi_to_gmt(TRUNC(SYSDATE, 'MONTH') - 1/86400, 'America/Chicago'),
				'User-defined', pi_to_gmt(
					TO_DATE(
						@Prompt('Enter end date', 'D', , mono, free, persistent, {'02/01/2020 00:00:00'}, User:2),
						pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
					) - 1/86400,
					pi_time_zone(1, 'America/Chicago')
				)
			)
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
		AND CLINICAL_EVENT.EVENT_ID = CE_MED_RESULT.EVENT_ID
		AND CE_MED_RESULT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
		AND (
			(CE_MED_RESULT.ADMIN_DOSAGE > 0 AND CE_MED_RESULT.IV_EVENT_CD = 0)
			OR CE_MED_RESULT.IV_EVENT_CD = 688706 -- Begin Bag
		)
		-- AND CE_MED_RESULT.IV_EVENT_CD IN (
			-- 0,
			-- 688706 -- Begin Bag
		-- )
		-- AND (CE_MED_RESULT.ADMIN_DOSAGE > 0 OR CE_MED_RESULT.INFUSION_RATE > 0)
		AND (
			CLINICAL_EVENT.EVENT_CD NOT IN (
				37556009, -- acetaminophen
				37557425, -- levothyroxine
				37557761 -- pantoprazole
			)
			OR CE_MED_RESULT.ADMIN_ROUTE_CD IN (
					508984, -- IV
					9022513, -- INJ
					9022647, -- IVPB
					9022649 -- IVP
			) 
		)
		AND CLINICAL_EVENT.ENCNTR_ID = ENCNTR_LOC_HIST.ENCNTR_ID
		AND ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM <= CLINICAL_EVENT.EVENT_END_DT_TM
		AND ENCNTR_LOC_HIST.TRANSACTION_DT_TM = (
			SELECT MAX(ELH.TRANSACTION_DT_TM)
			FROM ENCNTR_LOC_HIST ELH
			WHERE
				CLINICAL_EVENT.ENCNTR_ID = ELH.ENCNTR_ID
				AND ELH.TRANSACTION_DT_TM <= CLINICAL_EVENT.EVENT_END_DT_TM
		)
		AND ENCNTR_LOC_HIST.LOC_FACILITY_CD IN (
			3310, -- HH HERMANN
			3796, -- HC Childrens
			3821, -- HH Clinics
			3822, -- HH Trans Care
			3823, -- HH Rehab
			1099966301 -- HH Oncology TMC
		)
		AND ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM >= CLINICAL_EVENT.EVENT_END_DT_TM
), EXPAREL AS (
	SELECT DISTINCT
		ORDERS.ENCNTR_ID,
		ORDERS.PERSON_ID,
		ORDERS.ORIG_ORDER_DT_TM,
		pi_from_gmt(ORDERS.ORIG_ORDER_DT_TM, 'America/Chicago') AS ORDER_DATETIME,
		TRUNC(pi_from_gmt(ORDERS.ORIG_ORDER_DT_TM, 'America/Chicago'), 'MONTH') AS ORDER_MONTH,
		ORDERS.ORDER_ID,
		ORDERS.CATALOG_CD,
		LOWER(pi_get_cv_display(ORDERS.CATALOG_CD)) AS MEDICATION,
		ENCNTR_LOC_HIST.LOC_FACILITY_CD,
		pi_get_cv_display(ENCNTR_LOC_HIST.LOC_FACILITY_CD) AS FACILITY,
		ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD,
		pi_get_cv_display(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD) AS NURSE_UNIT,
		ENCNTR_LOC_HIST.MED_SERVICE_CD,
		pi_get_cv_display(ENCNTR_LOC_HIST.MED_SERVICE_CD) AS MED_SERVICE,
		ENCNTR_LOC_HIST.ENCNTR_TYPE_CD,
		pi_get_cv_display(ENCNTR_LOC_HIST.ENCNTR_TYPE_CD) AS ENCNTR_TYPE
	FROM
		ENCNTR_LOC_HIST,
		ORDERS
	WHERE
		ORDERS.CATALOG_CD = 750915114 -- bupivacaine liposome
		AND ORDERS.CATALOG_TYPE_CD = 1363 -- Pharmacy
		AND	ORDERS.TEMPLATE_ORDER_FLAG IN (0, 1)
		AND ORDERS.ORIG_ORDER_DT_TM BETWEEN
			DECODE(
				@Prompt('Choose date range', 'A', {'Last Month', 'User-defined'}, mono, free, , , User:0),
				'Last Month', pi_to_gmt(TRUNC(ADD_MONTHS(SYSDATE, -1), 'MONTH'), 'America/Chicago'),
				'User-defined', pi_to_gmt(
					TO_DATE(
						@Prompt('Enter begin date', 'D', , mono, free, persistent, {'01/01/2020 00:00:00'}, User:1),
						pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
					),
					pi_time_zone(1, 'America/Chicago')
				)
			)
			AND DECODE(
				@Prompt('Choose date range', 'A', {'Last Month', 'User-defined'}, mono, free, , , User:0),
				'Last Month', pi_to_gmt(TRUNC(SYSDATE, 'MONTH') - 1/86400, 'America/Chicago'),
				'User-defined', pi_to_gmt(
					TO_DATE(
						@Prompt('Enter end date', 'D', , mono, free, persistent, {'02/01/2020 00:00:00'}, User:2),
						pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
					) - 1/86400,
					pi_time_zone(1, 'America/Chicago')
				)
			)
		AND ORDERS.ENCNTR_ID = ENCNTR_LOC_HIST.ENCNTR_ID
		AND ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM <= ORDERS.ORIG_ORDER_DT_TM
		AND ENCNTR_LOC_HIST.TRANSACTION_DT_TM = (
			SELECT MAX(ELH.TRANSACTION_DT_TM)
			FROM ENCNTR_LOC_HIST ELH
			WHERE
				ORDERS.ENCNTR_ID = ELH.ENCNTR_ID
				AND ELH.TRANSACTION_DT_TM <= ORDERS.ORIG_ORDER_DT_TM
		)
		AND ENCNTR_LOC_HIST.LOC_FACILITY_CD IN (
			3310, -- HH HERMANN
			3796, -- HC Childrens
			3821, -- HH Clinics
			3822, -- HH Trans Care
			3823, -- HH Rehab
			1099966301 -- HH Oncology TMC
		)
		AND ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM >= ORDERS.ORIG_ORDER_DT_TM	
), ALL_DATA AS (
	SELECT
		ENCNTR_ID,
		DOSE_DATETIME,
		EVENT_ID,
		MEDICATION,
		FACILITY,
		NURSE_UNIT,
		MED_SERVICE,
		ENCNTR_TYPE
	FROM
		DOSES
		
	UNION
	
	SELECT
		ENCNTR_ID,
		ORDER_DATETIME AS DOSE_DATETIME,
		ORDER_ID AS EVENT_ID,
		MEDICATION,
		FACILITY,
		NURSE_UNIT,
		MED_SERVICE,
		ENCNTR_TYPE	
	FROM
		EXPAREL
)

SELECT * FROM ALL_DATA