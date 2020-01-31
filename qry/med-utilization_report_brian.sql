WITH NURSE_UNIT_PTS AS (
	SELECT DISTINCT
		ENCNTR_LOC_HIST.ENCNTR_ID,
		ENCOUNTER.PERSON_ID,
		ENCNTR_LOC_HIST.ENCNTR_LOC_HIST_ID,
		ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM,
		ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM,
		ENCNTR_LOC_HIST.TRANSACTION_DT_TM,
		ENCNTR_LOC_HIST.LOC_FACILITY_CD,
		ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD,
		ENCOUNTER.REG_DT_TM,
		ENCOUNTER.DISCH_DT_TM
	FROM
		ENCNTR_LOC_HIST,
		ENCOUNTER
	WHERE
		ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM >= pi_to_gmt(ADD_MONTHS(TRUNC(ADD_MONTHS(SYSDATE, 6), 'YEAR'), -42), pi_time_zone(2, @Variable('BOUSER')))
		AND ENCNTR_LOC_HIST.LOC_FACILITY_CD = 3310 -- HH HERMANN
		AND ENCNTR_LOC_HIST.LOC_BUILDING_CD = 216053283 -- HH HVI
		AND ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD = 5541 -- HH CVICU
		AND ENCNTR_LOC_HIST.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
), TARGET_MEDS AS (
	SELECT DISTINCT
		NURSE_UNIT_PTS.ENCNTR_ID,
		NURSE_UNIT_PTS.PERSON_ID,
		CLINICAL_EVENT.EVENT_ID,
		CLINICAL_EVENT.EVENT_CD,
		CLINICAL_EVENT.EVENT_END_DT_TM
	FROM
		CE_MED_RESULT,
		CLINICAL_EVENT,
		NURSE_UNIT_PTS
	WHERE
		NURSE_UNIT_PTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
		AND CLINICAL_EVENT.EVENT_CLASS_CD = 158 -- MED
		--AND NURSE_UNIT_PTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
		AND CLINICAL_EVENT.EVENT_CD IN (
			37556009, -- acetaminophen
			37556051, -- albumin human
			37556077, -- alteplase
			627676649, -- ceftaroline
			37556551, -- cisatracurium
			37556587, -- coagulation factor VIIa
			37556681, -- daptomycin
			37556709, -- dexmedetomidine
			117038716, -- ertapenem
			37556889, -- esmolol
			37557425, -- levothyroxine
			37557675, -- niCARdipine
			926562948, -- prothrombin complex
			1895018730, -- sugammadex
			37558323 -- vasopressin
		)
		AND CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN
			pi_to_gmt(ADD_MONTHS(TRUNC(ADD_MONTHS(SYSDATE, 6), 'YEAR'), -42), pi_time_zone(2, @Variable('BOUSER')))
			AND pi_to_gmt(TRUNC(SYSDATE, 'MONTH') - (1 / 86400), pi_time_zone(2, @Variable('BOUSER')))
		--AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
		AND CLINICAL_EVENT.EVENT_ID = CE_MED_RESULT.EVENT_ID
		AND (
			CE_MED_RESULT.ADMIN_DOSAGE > 0 
			OR CE_MED_RESULT.IV_EVENT_CD = 688706 -- Begin Bag
		)
		AND (
			CLINICAL_EVENT.EVENT_CD NOT IN (
				37556009, -- acetaminophen
				37557425 -- levothyroxine
			) 
			OR CE_MED_RESULT.ADMIN_ROUTE_CD IN (
				508984, -- IV
				9022513, -- INJ
				9022647, -- IVPB
				9022649 -- IVP
			) 
		)
), DOSES AS (
	SELECT
		TARGET_MEDS.ENCNTR_ID AS ENCOUNTER_ID,
		TARGET_MEDS.EVENT_ID AS EVENT_ID,
		TRUNC(pi_from_gmt(TARGET_MEDS.EVENT_END_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))), 'MONTH') AS EVENT_DATE,
		pi_get_cv_display(TARGET_MEDS.EVENT_CD) AS MEDICATION,
		pi_get_cv_display(NURSE_UNIT_PTS.LOC_NURSE_UNIT_CD) AS NURSE_UNIT
	FROM 
		NURSE_UNIT_PTS,
		TARGET_MEDS
	WHERE
		NURSE_UNIT_PTS.ENCNTR_ID = TARGET_MEDS.ENCNTR_ID
		AND NURSE_UNIT_PTS.PERSON_ID = TARGET_MEDS.PERSON_ID
		AND NURSE_UNIT_PTS.ENCNTR_LOC_HIST_ID = (
			SELECT MAX(NUP.ENCNTR_LOC_HIST_ID)
			FROM NURSE_UNIT_PTS NUP
			WHERE
				TARGET_MEDS.ENCNTR_ID = NUP.ENCNTR_ID
				AND NUP.TRANSACTION_DT_TM <= TARGET_MEDS.EVENT_END_DT_TM
				AND NUP.LOC_FACILITY_CD = 3310 -- HH HERMANN
				AND NUP.LOC_NURSE_UNIT_CD = 5541 -- HH CVICU
				AND NUP.END_EFFECTIVE_DT_TM >= TARGET_MEDS.EVENT_END_DT_TM
		)
), DOSE_COUNTS (
	SELECT DISTINCT
		DOSES.NURSE_UNIT,
		DOSES.MEDICATION,
		DOSES.EVENT_DATE,
		COUNT(DISTINCT DOSES.EVENT_ID) AS NUM_DOSES
	FROM
		DOSES
	GROUP BY
		DOSES.NURSE_UNIT,
		DOSES.MEDICATION,
		DOSES.EVENT_DATE
), ALL_MONTHS (
	SELECT 
		ADD_MONTHS(START_DATE, LEVEL-1) AS EVENT_MONTH
	FROM
		(
			SELECT 
				ADD_MONTHS(TRUNC(ADD_MONTHS(SYSDATE, 6), 'YEAR'), -42) START_DATE,
				TRUNC(SYSDATE, 'MONTH') END_DATE
			FROM
				DUAL
		)
	CONNECT BY LEVEL <= MONTHS_BETWEEN(TRUNC(END_DATE, 'MM'), TRUNC(START_DATE, 'MM'))
)

SELECT DISTINCT
	DOSES.NURSE_UNIT,
	DOSES.MEDICATION,
	ALL_MONTHS.EVENT_MONTH,
	DOSES.NUM_DOSES
FROM
	ALL_MONTHS,
	DOSES
WHERE
	ALL_MONTHS.EVENT_MONTH = DOSES.EVENT_DATE(+)
	
 
