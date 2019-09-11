WITH DOSES AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID,
		CASE ORDERS.TEMPLATE_ORDER_ID
			WHEN 0 THEN CLINICAL_EVENT.ORDER_ID
			ELSE ORDERS.TEMPLATE_ORDER_ID
		END AS ORDER_ID
	FROM
		CE_MED_RESULT,
		CLINICAL_EVENT,
		ENCNTR_LOC_HIST,
		ENCOUNTER,
		ORDERS
	WHERE
		CLINICAL_EVENT.EVENT_CD IN (
			37557233, -- immune globulin intravenous
			609787708 -- immune globulin intravenous and subcut
		)
		AND CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN 
			pi_to_gmt(DATE '2014-01-01', pi_time_zone(2, @Variable('BOUSER')))
			AND pi_to_gmt(TRUNC(SYSDATE, 'MONTH') - (1 / 86400), pi_time_zone(2, @Variable('BOUSER')))
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
		AND (
			CLINICAL_EVENT.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
			AND ENCOUNTER.ACTIVE_IND = 1
			AND ENCOUNTER.ENCNTR_TYPE_CD IN (
				29532, -- Inpatient
				29540 -- Observation
			)
		)
		AND (
			CLINICAL_EVENT.ENCNTR_ID = ENCNTR_LOC_HIST.ENCNTR_ID
			AND ENCNTR_LOC_HIST.ACTIVE_IND = 1
			AND ENCNTR_LOC_HIST.LOC_FACILITY_CD IN (
				3310, -- HH HERMANN
				3821, -- HH Clinics
				3822, -- HH Trans Care
				3823 -- HH Rehab
			)
			AND ENCNTR_LOC_HIST.TRANSACTION_DT_TM <= CLINICAL_EVENT.EVENT_END_DT_TM
			AND CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM AND ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM
			AND ENCNTR_LOC_HIST.TRANSACTION_DT_TM = (
				SELECT MAX(ELH.TRANSACTION_DT_TM)
				FROM ENCNTR_LOC_HIST ELH
				WHERE
					ELH.TRANSACTION_DT_TM <= CLINICAL_EVENT.EVENT_END_DT_TM
					AND CLINICAL_EVENT.ENCNTR_ID = ELH.ENCNTR_ID
					AND ELH.ACTIVE_IND = 1
			)
		)
		AND (
			CLINICAL_EVENT.EVENT_ID = CE_MED_RESULT.EVENT_ID
			AND CE_MED_RESULT.ADMIN_DOSAGE > 0
		)
		AND CLINICAL_EVENT.ORDER_ID = ORDERS.ORDER_ID
)	
	

SELECT DISTINCT
	ENCNTR_ALIAS.ALIAS AS FIN,
	ORDERS.ORDER_ID AS ORDER_ID,
	ENCOUNTER.DISCH_DT_TM - ENCOUNTER.REG_DT_TM AS LOS,
	pi_from_gmt(ORDERS.ORIG_ORDER_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))) AS ORDER_DATETIME,
	pi_get_cv_display(ENCNTR_LOC_HIST.MED_SERVICE_CD) AS MED_SERVICE,
	pi_get_cv_display(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD) AS NURSE_UNIT
FROM
	DOSES,
	ENCNTR_ALIAS,
	ENCNTR_LOC_HIST,
	ENCOUNTER,
	ORDERS
WHERE
	DOSES.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
	AND DOSES.ORDER_ID = ORDERS.ORDER_ID
	AND (
		ORDERS.ENCNTR_ID = ENCNTR_LOC_HIST.ENCNTR_ID
		AND ENCNTR_LOC_HIST.ACTIVE_IND = 1
		AND ENCNTR_LOC_HIST.LOC_FACILITY_CD IN (
			3310, -- HH HERMANN
			3821, -- HH Clinics
			3822, -- HH Trans Care
			3823 -- HH Rehab
		)
		AND ENCNTR_LOC_HIST.TRANSACTION_DT_TM <= ORDERS.ORIG_ORDER_DT_TM
		AND ORDERS.ORIG_ORDER_DT_TM BETWEEN ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM AND ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM
		AND ENCNTR_LOC_HIST.TRANSACTION_DT_TM = (
			SELECT MAX(ELH.TRANSACTION_DT_TM)
			FROM ENCNTR_LOC_HIST ELH
			WHERE
				ELH.TRANSACTION_DT_TM <= ORDERS.ORIG_ORDER_DT_TM
				AND ORDERS.ENCNTR_ID = ELH.ENCNTR_ID
				AND ELH.ACTIVE_IND = 1
		)
	)
	AND (
		ORDERS.ENCNTR_ID = ENCNTR_ALIAS.ENCNTR_ID
		AND ENCNTR_ALIAS.ACTIVE_IND = 1
		AND ENCNTR_ALIAS.END_EFFECTIVE_DT_TM > SYSDATE
		AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
	)
