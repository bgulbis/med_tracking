SELECT DISTINCT
	TRUNC(pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, 'America/Chicago'), 'MONTH') AS MONTH,
	COUNT(DISTINCT CLINICAL_EVENT.ENCNTR_ID) AS PATIENTS,
	COUNT(CLINICAL_EVENT.EVENT_ID) AS DOSES
FROM
	CLINICAL_EVENT,
	ENCNTR_LOC_HIST
WHERE
	CLINICAL_EVENT.EVENT_CD = 37558118 -- succinylcholine
	AND CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN
		pi_to_gmt(
			TO_DATE(
				@Prompt('Enter begin date', 'D', , mono, free, persistent, {'03/01/2020 00:00:00'}, User:0), 
				pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
			), 
			'America/Chicago'
		)
		AND pi_to_gmt(
			TO_DATE(
				@Prompt('Enter end date', 'D', , mono, free, persistent, {'09/01/2020 00:00:00'}, User:1), 
				pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
			) - 1/86400,
			'America/Chicago'
		)
	AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
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
	AND ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD IN (
		3224989205, -- HH S VUHH
		3224989653, -- HH S EDHH
		3224990761, -- HH S EDTR
		3224990803, -- HH S EREV
		3224929611, -- HC S VUPD
		3224931695 -- HC S EDPD
	)
GROUP BY
	TRUNC(pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, 'America/Chicago'), 'MONTH') 