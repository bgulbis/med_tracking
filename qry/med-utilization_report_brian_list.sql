SELECT DISTINCT
	TRUNC(pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))), 'MONTH') AS EVENT_DATE,
	CLINICAL_EVENT.ENCNTR_ID AS ENCOUNTER_ID,
	CLINICAL_EVENT.EVENT_ID AS EVENT_ID,
	pi_get_cv_display(CLINICAL_EVENT.EVENT_CD) AS MEDICATION,
	pi_get_cv_display(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD) AS NURSE_UNIT
FROM
	CE_MED_RESULT,
	CLINICAL_EVENT,
	ENCNTR_LOC_HIST,
	ENCOUNTER
WHERE
	ENCOUNTER.LOC_FACILITY_CD = 3310 -- HH HERMANN
	AND ENCOUNTER.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
	AND ENCOUNTER.PERSON_ID = CLINICAL_EVENT.PERSON_ID
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
		pi_to_gmt(TRUNC(ADD_MONTHS(SYSDATE, -36), 'MONTH'), pi_time_zone(2, @Variable('BOUSER')))
		AND pi_to_gmt(TRUNC(SYSDATE, 'MONTH') - (1 / 86400), pi_time_zone(2, @Variable('BOUSER')))
	AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
	AND CLINICAL_EVENT.ENCNTR_ID = ENCNTR_LOC_HIST.ENCNTR_ID
	AND ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM <= CLINICAL_EVENT.EVENT_END_DT_TM
	AND ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD = 5541 -- HH CVICU
	AND ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM >= CLINICAL_EVENT.EVENT_END_DT_TM
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
