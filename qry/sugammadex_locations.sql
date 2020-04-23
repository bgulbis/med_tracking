WITH SUG_DOSES AS (
	SELECT DISTINCT
		CLINICAL_EVENT.ENCNTR_ID,
		CLINICAL_EVENT.EVENT_ID,
		CLINICAL_EVENT.EVENT_END_DT_TM,
		-- pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))) AS DOSE_DATETIME,
		pi_get_cv_display(CLINICAL_EVENT.EVENT_CD) AS MEDICATION,
		pi_get_cv_display(ENCNTR_LOC_HIST.LOC_FACILITY_CD) AS FACILITY,
		pi_get_cv_display(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD) AS NURSE_UNIT
	FROM
		CE_MED_RESULT,
		CLINICAL_EVENT,
		ENCNTR_LOC_HIST
	WHERE
		CLINICAL_EVENT.EVENT_CD = 1895018730 -- sugammadex
		AND CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN
			pi_to_gmt(TRUNC(ADD_MONTHS(SYSDATE, -1), 'MONTH'), pi_time_zone(2, @Variable('BOUSER')))
			AND pi_to_gmt(TRUNC(SYSDATE, 'MONTH') - 1/86400, pi_time_zone(2, @Variable('BOUSER')))
		AND CLINICAL_EVENT.EVENT_ID = CE_MED_RESULT.EVENT_ID
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
			3796, -- HC Childrens
			3821, -- HH Clinics
			3822, -- HH Trans Care
			3823 -- HH Rehab
		)
), SURG_TIMES AS (
	SELECT DISTINCT
		SUG_DOSES.ENCNTR_ID,
		SURGICAL_CASE.SURG_CASE_ID,
		SURGICAL_CASE.SURG_START_DT_TM,
		SURGICAL_CASE.SURG_STOP_DT_TM,
		CT_ANESTH_START.CASE_TIME_DT_TM AS ANESTH_START_DT_TM,
		CT_ANESTH_STOP.CASE_TIME_DT_TM AS ANESTH_STOP_DT_TM
	FROM
		CASE_TIMES CT_ANESTH_START,
		CASE_TIMES CT_ANESTH_STOP,
		SUG_DOSES,
		SURGICAL_CASE
	WHERE
		SUG_DOSES.ENCNTR_ID = SURGICAL_CASE.ENCNTR_ID
		AND SURGICAL_CASE.SURG_CASE_ID = CT_ANESTH_START.SURG_CASE_ID
		AND CT_ANESTH_START.TASK_ASSAY_CD = 10511932 -- SN - CTm - Anesthesia - Start Time
		AND SURGICAL_CASE.SURG_CASE_ID = CT_ANESTH_STOP.SURG_CASE_ID
		AND CT_ANESTH_STOP.TASK_ASSAY_CD = 10511933 -- SN - CTm - Anesthesia - Stop Time
), OR_DOSES AS (
	SELECT
		SUG_DOSES.ENCNTR_ID,
		SUG_DOSES.EVENT_ID
	FROM
		SUG_DOSES,
		SURG_TIMES
	WHERE
		SUG_DOSES.ENCNTR_ID = SURG_TIMES.ENCNTR_ID
		AND SUG_DOSES.EVENT_END_DT_TM BETWEEN ANESTH_START_DT_TM AND ANESTH_STOP_DT_TM
)

SELECT
	FACILITY,
	NURSE_UNIT,
	COUNT(SUG_DOSES.EVENT_ID) AS DOSES,
	COUNT(OR_DOSES.EVENT_ID) AS OR_DOSES
FROM
	OR_DOSES,
	SUG_DOSES
WHERE
	SUG_DOSES.ENCNTR_ID = OR_DOSES.ENCNTR_ID(+)
	AND SUG_DOSES.EVENT_ID = OR_DOSES.EVENT_ID(+)
GROUP BY
	FACILITY,
	NURSE_UNIT
