WITH START_MONTH AS (
	SELECT DISTINCT
		DATE '2020-01-05' AS START_MONTH
	FROM
		DUAL
), ALL_MEDS AS (
	SELECT DISTINCT
		CODE_VALUE.CODE_VALUE AS EVENT_CD,
		LOWER(CODE_VALUE.DISPLAY) AS MEDICATION
	FROM
		CODE_VALUE
	WHERE
		CODE_VALUE.CODE_VALUE IN (
			37557367, -- ketamine
			37556709, -- dexmedetomidine
			37556956, -- FENTanyl
			37557204, -- HYDROmorphone
			37557620, -- morphine Sulfate
			37557455, -- LORAzepam
			37557589, -- midazolam
			37557925 -- propofol
		) 
), DOSES AS (
	SELECT DISTINCT
		CLINICAL_EVENT.ENCNTR_ID,
		CLINICAL_EVENT.PERSON_ID,
		CLINICAL_EVENT.EVENT_END_DT_TM,
		TRUNC(pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, 'America/Chicago'), 'DAY') AS DOSE_WEEK,
		CLINICAL_EVENT.EVENT_ID,
		CLINICAL_EVENT.EVENT_CD,
		LOWER(pi_get_cv_display(CLINICAL_EVENT.EVENT_CD)) AS MEDICATION,
		CLINICAL_EVENT.ORDER_ID,
		CE_MED_RESULT.IV_EVENT_CD,
		pi_get_cv_display(CE_MED_RESULT.IV_EVENT_CD) AS IV_EVENT,
		ENCNTR_LOC_HIST.LOC_FACILITY_CD,
		pi_get_cv_display(ENCNTR_LOC_HIST.LOC_FACILITY_CD) AS FACILITY,
		ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD,
		pi_get_cv_display(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD) AS NURSE_UNIT
	FROM
		ALL_MEDS,
		CE_MED_RESULT,
		CLINICAL_EVENT,
		ENCNTR_LOC_HIST,
		ENCOUNTER,
		START_MONTH
	WHERE
		CLINICAL_EVENT.EVENT_CD = ALL_MEDS.EVENT_CD
		AND CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN
			pi_to_gmt(START_MONTH.START_MONTH, 'America/Chicago')
			AND pi_to_gmt(TRUNC(SYSDATE, 'DAY') - 1/86400, 'America/Chicago')
		AND CLINICAL_EVENT.EVENT_ID = CE_MED_RESULT.EVENT_ID
		AND CE_MED_RESULT.IV_EVENT_CD = 688706 -- Begin Bag
		AND CLINICAL_EVENT.ENCNTR_ID = ENCNTR_LOC_HIST.ENCNTR_ID
		AND ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM <= CLINICAL_EVENT.EVENT_END_DT_TM
		AND ENCNTR_LOC_HIST.TRANSACTION_DT_TM = (
			SELECT MAX(ELH.TRANSACTION_DT_TM)
			FROM ENCNTR_LOC_HIST ELH
			WHERE
				CLINICAL_EVENT.ENCNTR_ID = ELH.ENCNTR_ID
				AND ELH.TRANSACTION_DT_TM <= CLINICAL_EVENT.EVENT_END_DT_TM
		)
		AND ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM >= CLINICAL_EVENT.EVENT_END_DT_TM
		AND ENCNTR_LOC_HIST.LOC_FACILITY_CD IN (
			3310, -- HH HERMANN
			-- 3796, -- HC Childrens
			-- 3821, -- HH Clinics
			3822, -- HH Trans Care
			3823 -- HH Rehab		
		)
		AND CLINICAL_EVENT.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
		AND ENCOUNTER.ENCNTR_TYPE_CLASS_CD IN (
			42631, -- Inpatient
			55851, -- Emergency
			688523 -- Observation
		)
), ACTIONS AS (
	SELECT
		DOSES.ENCNTR_ID,
		DOSES.EVENT_ID,
		DOSES.ORDER_ID,
		MAX(ORDER_ACTION.ACTION_SEQUENCE) AS ACTION_SEQUENCE
	FROM
		DOSES,
		ORDER_ACTION
	WHERE
		DOSES.ORDER_ID = ORDER_ACTION.ORDER_ID
		AND ORDER_ACTION.ACTION_DT_TM < DOSES.EVENT_END_DT_TM
	GROUP BY
		DOSES.ENCNTR_ID,
		DOSES.EVENT_ID,
		DOSES.ORDER_ID
), MED_PRODS AS (
	SELECT
		ACTIONS.ENCNTR_ID,
		ACTIONS.EVENT_ID,
		ACTIONS.ORDER_ID,
		ACTIONS.ACTION_SEQUENCE,
		ORDER_PRODUCT.ITEM_ID
	FROM
		ACTIONS,
		ORDER_PRODUCT
	WHERE
		ACTIONS.ORDER_ID = ORDER_PRODUCT.ORDER_ID
		AND ACTIONS.ACTION_SEQUENCE = ORDER_PRODUCT.ACTION_SEQUENCE
		AND ORDER_PRODUCT.ITEM_ID IN (
			275733495, -- ketAMINE 10 mg/mL in NS 50 mL syringe
			2931843, -- ketAMINE 500 mg/10 ml INJ VL
			304408345, -- ketAMINE 2000mg/NS 200ml (10mg/ml)
			10880895, -- fentaNYL 1000microgram/20ml drip (pyxis)
			117052454, -- dexmedetomidine 4 microgram/ml 100ml
			2950132, -- dexmedetomidine 200 microgram/2ml INJ
			2954763, -- propofol 1000 mg/100 ml INJ
			2902310, -- propofol 500 mg/50 ml INJ
			10881130, -- midazolam 50mg/ NS 50ml drip (premixed)
			107511585, -- HYDROmorphone HCL 50mg in NS 50ml PF
			10881363, -- MORPhine Sulfate 50mg/ NS 50ml drip (pyxis)
			72418593 -- LORazepam 50mg/NS 50ml Drip
		) 
), DOSES_PRODS AS (
	SELECT
		DOSES.*,
		MED_PRODS.ITEM_ID,
		MED_IDENTIFIER.VALUE AS MED_PRODUCT	
	FROM
		DOSES,
		MED_IDENTIFIER,
		MED_PRODS
	WHERE
		DOSES.EVENT_ID = MED_PRODS.EVENT_ID
		AND MED_PRODS.ITEM_ID = MED_IDENTIFIER.ITEM_ID
		AND MED_IDENTIFIER.MED_IDENTIFIER_TYPE_CD = 1564 -- Description
		AND MED_IDENTIFIER.MED_PRODUCT_ID = 0
), ALL_WEEKS AS (
	SELECT 
		START_DATE + ((LEVEL - 1) * 7) AS EVENT_WEEK
	FROM
		(
			SELECT 
				DATE '2020-01-05' START_DATE,
				TRUNC(SYSDATE, 'DAY') - 1/86400 END_DATE
			FROM
				DUAL
		)
	CONNECT BY LEVEL <= ((END_DATE - START_DATE) / 7) + 1
), ALL_VALUES AS (
	SELECT
		MEDICATION,
		EVENT_WEEK
	FROM
		ALL_MEDS,
		ALL_WEEKS
), DOSE_COUNTS AS (
	SELECT DISTINCT
		MEDICATION,
		DOSE_WEEK,
		-- MED_PRODUCT,
		-- FACILITY,
		NURSE_UNIT,
		COUNT(DISTINCT ENCNTR_ID) AS PATIENTS,
		COUNT(DISTINCT EVENT_ID) AS BAGS
	FROM
		DOSES_PRODS
	GROUP BY
		MEDICATION,
		DOSE_WEEK,
		-- MED_PRODUCT,
		-- FACILITY,
		NURSE_UNIT
), NUM_BAGS AS (
	SELECT
		ALL_VALUES.MEDICATION,
		ALL_VALUES.EVENT_WEEK,
		-- DOSE_COUNTS.MED_PRODUCT,
		DOSE_COUNTS.NURSE_UNIT,
		COALESCE(DOSE_COUNTS.PATIENTS, 0) AS PATIENTS,
		COALESCE(DOSE_COUNTS.BAGS, 0) AS BAGS
	FROM
		ALL_VALUES,
		DOSE_COUNTS
	WHERE
		ALL_VALUES.MEDICATION = DOSE_COUNTS.MEDICATION(+)
		AND ALL_VALUES.EVENT_WEEK = DOSE_COUNTS.DOSE_WEEK(+)
), ALL_UNITS AS (
	SELECT DISTINCT
		DOSES_PRODS.LOC_NURSE_UNIT_CD AS LOC_NURSE_UNIT_CD,
		DOSES_PRODS.NURSE_UNIT AS NURSE_UNIT
	FROM
		DOSES_PRODS
), UNIT_ENCNTR_LIST AS (
	SELECT DISTINCT
		ENCNTR_LOC_HIST.ENCNTR_ID
	FROM
		ALL_UNITS,
		ENCNTR_LOC_HIST,
		START_MONTH
	WHERE
		ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD = ALL_UNITS.LOC_NURSE_UNIT_CD
		AND ENCNTR_LOC_HIST.TRANSACTION_DT_TM BETWEEN
			pi_to_gmt(START_MONTH.START_MONTH, 'America/Chicago')
			AND pi_to_gmt(TRUNC(SYSDATE, 'DAY') - 1/86400, 'America/Chicago')
), UNIT_TRNSFRS AS (
	SELECT DISTINCT
		ENCNTR_LOC_HIST.ENCNTR_LOC_HIST_ID AS ELH_ID,
		ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD,
		CASE
			WHEN 
				LAG(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD, 1, 0) OVER (
					PARTITION BY ENCNTR_LOC_HIST.ENCNTR_ID 
					ORDER BY ENCNTR_LOC_HIST.ENCNTR_LOC_HIST_ID
				) <> ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD THEN 1
			WHEN
				LEAD(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD, 1, 0) OVER (
					PARTITION BY ENCNTR_LOC_HIST.ENCNTR_ID 
					ORDER BY ENCNTR_LOC_HIST.ENCNTR_LOC_HIST_ID
				) <> ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD
				AND LAG(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD, 1, 0) OVER (
					PARTITION BY ENCNTR_LOC_HIST.ENCNTR_ID 
					ORDER BY ENCNTR_LOC_HIST.ENCNTR_LOC_HIST_ID
				) = ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD THEN -1
			ELSE 0
		END AS UNIT_TRANSFER,
		CASE
			WHEN 
				LAG(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD, 1, 0) OVER (
					PARTITION BY ENCNTR_LOC_HIST.ENCNTR_ID 
					ORDER BY ENCNTR_LOC_HIST.ENCNTR_LOC_HIST_ID
				) <> ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD THEN TRUNC(pi_from_gmt(ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM, 'America/Chicago'))
			WHEN
				LEAD(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD, 1, 0) OVER (
					PARTITION BY ENCNTR_LOC_HIST.ENCNTR_ID 
					ORDER BY ENCNTR_LOC_HIST.ENCNTR_LOC_HIST_ID
				) <> ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD
				AND LAG(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD, 1, 0) OVER (
					PARTITION BY ENCNTR_LOC_HIST.ENCNTR_ID 
					ORDER BY ENCNTR_LOC_HIST.ENCNTR_LOC_HIST_ID
				) = ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD THEN TRUNC(pi_from_gmt(ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM, 'America/Chicago'))
		END AS TRANSFER_DATE
	FROM
		ENCNTR_LOC_HIST,
		UNIT_ENCNTR_LIST
	WHERE
		UNIT_ENCNTR_LIST.ENCNTR_ID = ENCNTR_LOC_HIST.ENCNTR_ID
), UNIT_TRNSFRS_DAILY AS (
	SELECT DISTINCT
		UNIT_TRNSFRS.TRANSFER_DATE,
		UNIT_TRNSFRS.LOC_NURSE_UNIT_CD,
		SUM(UNIT_TRNSFRS.UNIT_TRANSFER) AS NET_TRANSFERS
	FROM
		ALL_UNITS,
		START_MONTH,
		UNIT_TRNSFRS
	WHERE
		UNIT_TRNSFRS.UNIT_TRANSFER <> 0
		AND UNIT_TRNSFRS.LOC_NURSE_UNIT_CD = ALL_UNITS.LOC_NURSE_UNIT_CD
		AND UNIT_TRNSFRS.TRANSFER_DATE BETWEEN pi_to_gmt(START_MONTH.START_MONTH, 'America/Chicago') AND pi_to_gmt(TRUNC(SYSDATE, 'DAY') - 1/86400, 'America/Chicago')
	GROUP BY
		UNIT_TRNSFRS.TRANSFER_DATE,
		UNIT_TRNSFRS.LOC_NURSE_UNIT_CD
), CENSUS AS (
	SELECT DISTINCT
		TRUNC(pi_from_gmt(LH_CNT_LOC_UNIT_CENSUS.CENSUS_DT_TM, 'America/Chicago')) AS CENSUS_DATE,
		LH_CNT_LOC_UNIT_CENSUS.LOC_NURSE_UNIT_CD,
		LH_CNT_LOC_UNIT_CENSUS.CENSUS_PERSON_TOTAL AS CENSUS
	FROM
		ALL_UNITS,
		LH_CNT_LOC_UNIT_CENSUS,
		START_MONTH
	WHERE
		LH_CNT_LOC_UNIT_CENSUS.LOC_NURSE_UNIT_CD = ALL_UNITS.LOC_NURSE_UNIT_CD
		AND LH_CNT_LOC_UNIT_CENSUS.CENSUS_DT_TM BETWEEN
			pi_to_gmt(START_MONTH.START_MONTH, 'America/Chicago')
			AND pi_to_gmt(TRUNC(SYSDATE, 'DAY') - 1/86400, 'America/Chicago')
), PATIENT_DAYS AS (
	SELECT
		CENSUS.CENSUS_DATE,
		CENSUS.LOC_NURSE_UNIT_CD,
		CENSUS.CENSUS,
		NVL(UNIT_TRNSFRS_DAILY.NET_TRANSFERS, 0) AS NET_TRANSFERS,
		CENSUS.CENSUS + NVL(UNIT_TRNSFRS_DAILY.NET_TRANSFERS, 0) AS PATIENT_DAYS
	FROM
		CENSUS,
		UNIT_TRNSFRS_DAILY
	WHERE 
		CENSUS.CENSUS_DATE = UNIT_TRNSFRS_DAILY.TRANSFER_DATE(+)
		AND CENSUS.LOC_NURSE_UNIT_CD = UNIT_TRNSFRS_DAILY.LOC_NURSE_UNIT_CD(+)
), PT_DAY_TOTALS AS (
	SELECT
		TRUNC(PATIENT_DAYS.CENSUS_DATE, 'DAY') AS CENSUS_WEEK,
		pi_get_cv_display(PATIENT_DAYS.LOC_NURSE_UNIT_CD) AS NURSE_UNIT,
		-- 'ICU' AS NURSE_UNIT,
		SUM(PATIENT_DAYS.PATIENT_DAYS) AS PATIENT_DAYS
	FROM
		PATIENT_DAYS	
	WHERE
		pi_get_cv_display(PATIENT_DAYS.LOC_NURSE_UNIT_CD) <> 'HH PAHH'
	GROUP BY
		TRUNC(PATIENT_DAYS.CENSUS_DATE, 'DAY'),
		PATIENT_DAYS.LOC_NURSE_UNIT_CD
), BAGS_PT_DAYS AS (
	SELECT
		NUM_BAGS.*,
		PT_DAY_TOTALS.PATIENT_DAYS
	FROM
		NUM_BAGS,
		PT_DAY_TOTALS
	WHERE
		NUM_BAGS.NURSE_UNIT = PT_DAY_TOTALS.NURSE_UNIT(+)
		AND NUM_BAGS.EVENT_WEEK = PT_DAY_TOTALS.CENSUS_WEEK(+)
), ALL_CENSUS AS (
	SELECT 
		TRUNC(pi_from_gmt(LH_CNT_LOC_UNIT_CENSUS.CENSUS_DT_TM, 'America/Chicago')) AS CENSUS_DT_TM,
		LH_CNT_LOC_UNIT_CENSUS_ID,
		LOC_NURSE_UNIT_CD,
		pi_get_cv_display(LOC_NURSE_UNIT_CD) AS NURSE_UNIT,
		CENSUS_PERSON_TOTAL,
		LOC_FACILITY_CD,
		pi_get_cv_display(LOC_FACILITY_CD) AS FACILITY
	FROM 
		LH_CNT_LOC_UNIT_CENSUS,
		START_MONTH
	WHERE 
		LH_CNT_LOC_UNIT_CENSUS.CENSUS_DT_TM BETWEEN
			pi_to_gmt(START_MONTH.START_MONTH, 'America/Chicago')
			AND pi_to_gmt(TRUNC(SYSDATE, 'DAY') - 1/86400, 'America/Chicago')
), FACILITY_CENSUS AS (
	SELECT
		CENSUS_DT_TM,
		FACILITY,
		SUM(CENSUS_PERSON_TOTAL) AS CENSUS
	FROM 
		ALL_CENSUS
	WHERE
		FACILITY = 'HH HERMANN'
		AND NURSE_UNIT NOT IN ('HH PAHH', 'HH NNHH')
	GROUP BY
		CENSUS_DT_TM,
		FACILITY
), FACILITY_ADMITS AS (
	SELECT
		TRUNC(pi_from_gmt(REG_DT_TM, 'America/Chicago')) AS REG_DT_TM,
		COUNT(DISTINCT ENCNTR_ID) AS ADMITS
	FROM
		ENCOUNTER,
		START_MONTH
	WHERE
		ENCOUNTER.ORGANIZATION_ID = 1 -- Memorial Hermann Hospital
		AND ENCOUNTER.REG_DT_TM BETWEEN
			pi_to_gmt(START_MONTH.START_MONTH, 'America/Chicago')
			AND pi_to_gmt(TRUNC(SYSDATE, 'DAY') - 1/86400, 'America/Chicago')
		AND ENCOUNTER.ENCNTR_TYPE_CLASS_CD IN (
			42631, -- Inpatient
			-- 55851, -- Emergency
			688523 -- Observation
		)
		AND ENCOUNTER.LOC_FACILITY_CD = 3310 -- HH HERMANN
	GROUP BY
		TRUNC(pi_from_gmt(REG_DT_TM, 'America/Chicago'))
), FACILITY_DISCH AS (
	SELECT
		TRUNC(pi_from_gmt(DISCH_DT_TM, 'America/Chicago')) AS DISCH_DT_TM,
		COUNT(DISTINCT ENCNTR_ID) AS DISCH
	FROM
		ENCOUNTER,
		START_MONTH
	WHERE
		ENCOUNTER.DISCH_DT_TM BETWEEN
			pi_to_gmt(START_MONTH.START_MONTH, 'America/Chicago')
			AND pi_to_gmt(TRUNC(SYSDATE, 'DAY') - 1/86400, 'America/Chicago')
		AND ENCOUNTER.ORGANIZATION_ID = 1 -- Memorial Hermann Hospital
		AND ENCOUNTER.ENCNTR_TYPE_CLASS_CD IN (
			42631, -- Inpatient
			-- 55851, -- Emergency
			688523 -- Observation
		)
		AND ENCOUNTER.LOC_FACILITY_CD = 3310 -- HH HERMANN
		AND TRUNC(ENCOUNTER.REG_DT_TM) <> TRUNC(ENCOUNTER.DISCH_DT_TM)
	GROUP BY
		TRUNC(pi_from_gmt(DISCH_DT_TM, 'America/Chicago'))
), FACILITY_PT_DAYS AS (
	SELECT
		CENSUS_DT_TM,
		CENSUS,
		ADMITS,
		DISCH,
		CENSUS + ADMITS - DISCH AS PT_DAYS
	FROM
		FACILITY_ADMITS,
		FACILITY_CENSUS,
		FACILITY_DISCH
	WHERE
		FACILITY_CENSUS.CENSUS_DT_TM = FACILITY_ADMITS.REG_DT_TM
		AND FACILITY_CENSUS.CENSUS_DT_TM = FACILITY_DISCH.DISCH_DT_TM
), FACILITY_PT_DAYS_WEEKLY AS (
	SELECT
		TRUNC(CENSUS_DT_TM, 'DAY') AS EVENT_WEEK,
		SUM(PT_DAYS) AS PT_DAYS_FACILITY
	FROM
		FACILITY_PT_DAYS
	GROUP BY
		TRUNC(CENSUS_DT_TM, 'DAY')
)

SELECT
	BAGS_PT_DAYS.*,
	FACILITY_PT_DAYS_WEEKLY.PT_DAYS_FACILITY
FROM
	BAGS_PT_DAYS,
	FACILITY_PT_DAYS_WEEKLY
WHERE
	BAGS_PT_DAYS.EVENT_WEEK = FACILITY_PT_DAYS_WEEKLY.EVENT_WEEK