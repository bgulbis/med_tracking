SELECT DISTINCT
	TO_CHAR(TRUNC(pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))), 'MONTH'), 'YYYY-MM-DD') AS EVENT_DATE,
	CV_NURSE_UNIT.DISPLAY AS NURSE_UNIT,
	CV_EVENT.DISPLAY AS MEDICATION,
	COUNT(DISTINCT CLINICAL_EVENT.EVENT_ID) AS DOSES,
	COUNT(DISTINCT CLINICAL_EVENT.ENCNTR_ID) AS PATIENTS
FROM
	CE_MED_RESULT,
	CLINICAL_EVENT,
	CODE_VALUE CV_EVENT,
	CODE_VALUE CV_NURSE_UNIT,
	ENCNTR_LOC_HIST,
	ENCOUNTER
WHERE
	CLINICAL_EVENT.EVENT_CD IN (
		37556001, -- abciximab
		37556009, -- acetaminophen
		37556051, -- albumin human
		37556077, -- alteplase
		37556132, -- amphotericin B liposomal
		37556133, -- ampicillin
		37556134, -- ampicillin-sulbactam
		37556150, -- antihemophilic factor,
		37556449, -- cefazolin
		37556453, -- cefepime
		627676649, -- ceftaroline
		37556469, -- ceftriaxone
		37556587, -- coagulation factor VIIa
		37556681, -- daptomycin
		37556709, -- dexmedetomidine
		37556867, -- eptifibatide
		117038716, -- ertapenem
		37556889, -- esmolol
		37557233, -- immune globulin intravenous
		609787708, -- immune globulin intravenous and subcut
		37557425, -- levothyroxine
		37557527, -- meropenem
		37557578, -- METRONIDazole
		84261530, -- micafungin
		37557642, -- nafcillin
		37557675, -- niCARdipine
		37557681, -- nimodipine
		37557691, -- norepinephrine
		37557791, -- pentobarbital
		37557845, -- piperacillin-tazobactam
		37557925, -- propofol
		926562948, -- prothrombin complex
		37558315, -- vancomycin
		37558323 -- vasopressin
	)
	AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
    AND CLINICAL_EVENT.EVENT_CD = CV_EVENT.CODE_VALUE
	AND (
		CLINICAL_EVENT.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
		AND ENCOUNTER.ACTIVE_IND = 1
		AND ENCOUNTER.LOC_FACILITY_CD = 3310 -- HH HERMANN
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
		AND ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD IN (283905037, 193270) -- (HH 7J, HH STRK)
		AND ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD = CV_NURSE_UNIT.CODE_VALUE
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
				'Today', pi_to_gmt(TRUNC(SYSDATE), pi_time_zone(2, @Variable('BOUSER'))),
				'Yesterday', pi_to_gmt(TRUNC(SYSDATE) - 1, pi_time_zone(2, @Variable('BOUSER'))),
				'Week to Date', pi_to_gmt(TRUNC(SYSDATE, 'DAY'), pi_time_zone(2, @Variable('BOUSER'))),
				'Last Week', pi_to_gmt(TRUNC(SYSDATE - 7, 'DAY'), pi_time_zone(2, @Variable('BOUSER'))),
				'Last Month', pi_to_gmt(TRUNC(ADD_MONTHS(SYSDATE, -1), 'MONTH'), pi_time_zone(2, @Variable('BOUSER'))),
				'Month to Date', pi_to_gmt(TRUNC(SYSDATE - 1, 'MONTH'), pi_time_zone(2, @Variable('BOUSER'))),
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
    TRUNC(pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))), 'MONTH'),
	CV_NURSE_UNIT.DISPLAY,
	CV_EVENT.DISPLAY
