WITH DOSES AS (
	SELECT DISTINCT
		CLINICAL_EVENT.ENCNTR_ID,
		CLINICAL_EVENT.EVENT_ID,
		CLINICAL_EVENT.ORDER_ID,
		CASE 
			WHEN ORDERS.TEMPLATE_ORDER_ID = 0 THEN ORDERS.ORDER_ID
			ELSE ORDERS.TEMPLATE_ORDER_ID
		END AS ORIG_ORDER_ID		
	FROM
		CE_MED_RESULT,
		CLINICAL_EVENT,
		ENCNTR_LOC_HIST,
		ORDERS
	WHERE
		CLINICAL_EVENT.EVENT_CD = 53807885 -- riFAXimin
		AND CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN
			pi_to_gmt(DATE '2019-07-01', pi_time_zone(2, @Variable('BOUSER')))
			AND pi_to_gmt(DATE '2020-04-01' - 1/86400, pi_time_zone(2, @Variable('BOUSER')))
		AND CLINICAL_EVENT.EVENT_ID = CE_MED_RESULT.EVENT_ID
		AND CE_MED_RESULT.IV_EVENT_CD IN (
			0,
			688706 -- Begin Bag
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
			-- 3796, -- HC Childrens
			-- 3821, -- HH Clinics
			3822, -- HH Trans Care
			3823 -- HH Rehab
		)
		AND ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM >= CLINICAL_EVENT.EVENT_END_DT_TM
		AND CLINICAL_EVENT.ORDER_ID = ORDERS.ORDER_ID
), NUM_DOSES AS (
	SELECT 
		ORIG_ORDER_ID,
		COUNT(DISTINCT EVENT_ID) AS DOSES_GIVEN
	FROM
		DOSES
	GROUP BY
		ORIG_ORDER_ID
), MED_ORDERS AS (
	SELECT DISTINCT
		ORDERS.ENCNTR_ID,
		ORDERS.ORDER_ID,
		ORDERS.ORIG_ORDER_DT_TM,
		pi_from_gmt(ORDERS.ORIG_ORDER_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))) AS ORDER_DATETIME,
		ORDERS.CATALOG_CD,
		pi_get_cv_display(ORDERS.CATALOG_CD) AS MEDICATION,
		ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD,
		pi_get_cv_display(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD) AS NURSE_UNIT_ORDER,
		FREQUENCY_SCHEDULE.FREQUENCY_CD,
		pi_get_cv_display(FREQUENCY_SCHEDULE.FREQUENCY_CD) AS FREQ,
		PRSNL.PERSON_ID,
		PRSNL.NAME_FULL_FORMATTED AS ORDER_PROVIDER,
		PRSNL.POSITION_CD, 
		pi_get_cv_display(PRSNL.POSITION_CD) AS PROVIDER_POSITION,
		NUM_DOSES.DOSES_GIVEN
	FROM
		ENCNTR_LOC_HIST,
		FREQUENCY_SCHEDULE,
		NUM_DOSES,
		ORDER_ACTION,
		ORDERS,
		PRSNL
	WHERE
		NUM_DOSES.ORIG_ORDER_ID = ORDERS.ORDER_ID
		AND ORDERS.FREQUENCY_ID = FREQUENCY_SCHEDULE.FREQUENCY_ID
		AND ORDERS.ENCNTR_ID = ENCNTR_LOC_HIST.ENCNTR_ID
		AND ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM <= ORDERS.ORIG_ORDER_DT_TM
		AND ENCNTR_LOC_HIST.TRANSACTION_DT_TM = (
			SELECT MAX(ELH.TRANSACTION_DT_TM)
			FROM ENCNTR_LOC_HIST ELH
			WHERE
				ORDERS.ENCNTR_ID = ELH.ENCNTR_ID
				AND ELH.TRANSACTION_DT_TM <= ORDERS.ORIG_ORDER_DT_TM
		)
		AND ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM >= ORDERS.ORIG_ORDER_DT_TM
		AND ORDERS.ORDER_ID = ORDER_ACTION.ORDER_ID
		AND ORDER_ACTION.ACTION_TYPE_CD = 1376 -- Order
		AND ORDER_ACTION.ORDER_PROVIDER_ID = PRSNL.PERSON_ID
), ORD_DETAILS AS (
	SELECT DISTINCT
		ORDER_DETAIL.ORDER_ID,
		ORDER_DETAIL.OE_FIELD_MEANING,
		ORDER_DETAIL.OE_FIELD_DISPLAY_VALUE
	FROM
		MED_ORDERS,
		ORDER_DETAIL
	WHERE
		MED_ORDERS.ORDER_ID = ORDER_DETAIL.ORDER_ID
		AND ORDER_DETAIL.ACTION_SEQUENCE = 1
), ORD_DETAIL_PIVOT AS (
	SELECT * FROM ORD_DETAILS
	PIVOT(
		MIN(OE_FIELD_DISPLAY_VALUE) FOR OE_FIELD_MEANING IN (
			'DURATION' AS DURATION,
			'DURATIONUNIT' AS DURATION_UNIT,
			'STRENGTHDOSE' AS DOSE,
			'STRENGTHDOSEUNIT' AS DOSE_UNIT
		)
	)
)

SELECT
	ENCNTR_ALIAS.ALIAS AS FIN,
	MED_ORDERS.*,
	ORD_DETAIL_PIVOT.DURATION,
	ORD_DETAIL_PIVOT.DURATION_UNIT,
	ORD_DETAIL_PIVOT.DOSE,
	ORD_DETAIL_PIVOT.DOSE_UNIT
FROM 
	MED_ORDERS,
	ENCNTR_ALIAS,
	ORD_DETAIL_PIVOT
WHERE
	MED_ORDERS.ORDER_ID = ORD_DETAIL_PIVOT.ORDER_ID(+)
	AND MED_ORDERS.ENCNTR_ID = ENCNTR_ALIAS.ENCNTR_ID
	AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
