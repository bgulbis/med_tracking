WITH DOSES AS (
	SELECT DISTINCT
		CLINICAL_EVENT.ENCNTR_ID,
		-- CLINICAL_EVENT.EVENT_ID,
		-- CLINICAL_EVENT.ORDER_ID,
		CASE 
			WHEN ORDERS.TEMPLATE_ORDER_ID = 0 THEN ORDERS.ORDER_ID
			ELSE ORDERS.TEMPLATE_ORDER_ID
		END AS ORIG_ORDER_ID,
		pi_get_cv_display(CLINICAL_EVENT.EVENT_CD) AS MEDICATION,
		ORDERS.ORDERED_AS_MNEMONIC
		-- pi_get_cv_display(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD) AS NURSE_UNIT
	FROM
		CE_MED_RESULT,
		CLINICAL_EVENT,
		ENCNTR_LOC_HIST,
		ORDERS
	WHERE
		CLINICAL_EVENT.EVENT_CD IN (
			37556051, -- albumin human
			37557675, -- niCARdipine
			53807885 -- riFAXimin
		)
		AND CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN
			pi_to_gmt(DATE '2020-03-01', pi_time_zone(2, @Variable('BOUSER')))
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
), PROD_LOOKUP AS (
	SELECT
		DOSES.*,
		ORDER_PRODUCT.ITEM_ID,
		-- MED_IDENTIFIER.VALUE AS NDC,
		PROD_DISPENSE_HX.MED_PRODUCT_ID,
		PHA_PROD_DISP_OBS_ST.MANF_NDC_STR,
		MLTM_NDC_CORE_DESCRIPTION.NDC_CODE,
		MLTM_NDC_CORE_DESCRIPTION.INNER_PACKAGE_SIZE,
		MLTM_NDC_CORE_DESCRIPTION.INNER_PACKAGE_DESC_CODE,
		MLTM_NDC_CORE_DESCRIPTION.OUTER_PACKAGE_SIZE,
		MLTM_NDC_COST.COST,
		MLTM_NDC_COST.INVENTORY_TYPE
	FROM
		DISPENSE_HX,
		DOSES,
		-- MED_IDENTIFIER,
		MLTM_NDC_CORE_DESCRIPTION,
		MLTM_NDC_COST,
		ORDER_PRODUCT,
		PHA_PROD_DISP_OBS_ST,
		PROD_DISPENSE_HX
	WHERE
		DOSES.ORIG_ORDER_ID = ORDER_PRODUCT.ORDER_ID(+)
		-- AND ORDER_PRODUCT.ITEM_ID = MED_IDENTIFIER.ITEM_ID(+)
		-- AND MED_IDENTIFIER.MED_IDENTIFIER_TYPE_CD(+) = 1573 -- NDC
		AND DOSES.ORIG_ORDER_ID = PHA_PROD_DISP_OBS_ST.ORDER_ID(+)
		AND PHA_PROD_DISP_OBS_ST.MANF_NDC_STR = MLTM_NDC_CORE_DESCRIPTION.NDC_FORMATTED(+)
		AND MLTM_NDC_CORE_DESCRIPTION.NDC_CODE = MLTM_NDC_COST.NDC_CODE(+)
		AND DOSES.ORIG_ORDER_ID = DISPENSE_HX.ORDER_ID(+)
		AND DISPENSE_HX.DISPENSE_HX_ID = PROD_DISPENSE_HX.DISPENSE_HX_ID(+)
), MISSING_PROD AS (
	SELECT
		PROD_LOOKUP.ENCNTR_ID,
		PROD_LOOKUP.ORIG_ORDER_ID,
		PROD_LOOKUP.MEDICATION,
		PROD_LOOKUP.ITEM_ID,
		MED_IDENTIFIER.VALUE AS NDC,
		MLTM_NDC_CORE_DESCRIPTION.NDC_CODE,
		MLTM_NDC_CORE_DESCRIPTION.INNER_PACKAGE_SIZE,
		MLTM_NDC_CORE_DESCRIPTION.OUTER_PACKAGE_SIZE,
		MLTM_NDC_COST.COST,
		MLTM_NDC_COST.INVENTORY_TYPE
	FROM
		MED_IDENTIFIER,
		MLTM_NDC_CORE_DESCRIPTION,
		MLTM_NDC_COST,
		PROD_LOOKUP
	WHERE
		PROD_LOOKUP.MED_PRODUCT_ID IS NULL
		AND PROD_LOOKUP.ITEM_ID = MED_IDENTIFIER.ITEM_ID
		AND MED_IDENTIFIER.MED_IDENTIFIER_TYPE_CD = 1573 -- NDC
		AND REGEXP_REPLACE(MED_IDENTIFIER.VALUE, 'z', '') = MLTM_NDC_CORE_DESCRIPTION.NDC_FORMATTED(+)
		AND MLTM_NDC_CORE_DESCRIPTION.NDC_CODE = MLTM_NDC_COST.NDC_CODE(+)
)

SELECT * FROM PROD_LOOKUP
		
/* ), DOSE_PRODS AS (
	SELECT DISTINCT
		DOSES.*,
		ORDER_PRODUCT.ITEM_ID,
		DISPENSE_HX.DISPENSE_HX_ID,
		PROD_DISPENSE_HX.ITEM_ID AS PDH_ITEM_ID,
		PROD_DISPENSE_HX.MANF_ITEM_ID,
		PROD_DISPENSE_HX.MED_PRODUCT_ID,
		MED_COST_HX.COST
		-- CE_MED_ADMIN_IDENT.MED_PRODUCT_ID
		MED_IDENTIFIER.MED_PRODUCT_ID,
		MED_IDENTIFIER.VALUE AS MED_PRODUCT,
		MED_COST_HX.COST
	FROM
		-- CE_MED_ADMIN_IDENT,
		DISPENSE_HX,
		DOSES,
		MED_COST_HX,
		-- MED_IDENTIFIER,
		ORDER_PRODUCT,
		PROD_DISPENSE_HX
	WHERE
		DOSES.ORIG_ORDER_ID = ORDER_PRODUCT.ORDER_ID(+)
		AND DOSES.ORIG_ORDER_ID = DISPENSE_HX.ORDER_ID(+)
		AND DISPENSE_HX.DISPENSE_HX_ID = PROD_DISPENSE_HX.DISPENSE_HX_ID(+)
		AND PROD_DISPENSE_HX.MED_PRODUCT_ID = MED_COST_HX.MED_PRODUCT_ID(+)
		-- AND ORDER_PRODUCT.ITEM_ID = CE_MED_ADMIN_IDENT.ITEM_ID(+)
		AND ORDER_PRODUCT.ITEM_ID = MED_IDENTIFIER.ITEM_ID(+)
		AND MED_IDENTIFIER.MED_IDENTIFIER_TYPE_CD(+) = 1564 -- Description
		AND MED_IDENTIFIER.MED_PRODUCT_ID(+) > 0
		AND MED_IDENTIFIER.MED_PRODUCT_ID = MED_COST_HX.MED_PRODUCT_ID(+)
)

SELECT * FROM DOSE_PRODS */