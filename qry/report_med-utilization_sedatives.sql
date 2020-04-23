WITH ALL_MEDS AS (
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
		TRUNC(pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))), 'DAY') AS DOSE_WEEK,
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
		ENCOUNTER
	WHERE
		CLINICAL_EVENT.EVENT_CD = ALL_MEDS.EVENT_CD
		AND CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN
			-- pi_to_gmt(TRUNC(SYSDATE - 7*6, 'DAY'), pi_time_zone(2, @Variable('BOUSER')))
			pi_to_gmt(DATE '2020-01-05', pi_time_zone(2, @Variable('BOUSER')))
			AND pi_to_gmt(TRUNC(SYSDATE, 'DAY') - 1/86400, pi_time_zone(2, @Variable('BOUSER')))
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
)

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