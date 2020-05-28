WITH START_MONTH AS (
	SELECT DISTINCT
		ADD_MONTHS(TRUNC(ADD_MONTHS(SYSDATE, 6), 'YEAR'), -@Prompt('Months Back', 'N', , mono, free, persistent, {'30'}, User:0)) AS START_MONTH
	FROM
		DUAL
), ALL_UNITS AS (
	SELECT DISTINCT
		CODE_VALUE.CODE_VALUE AS LOC_NURSE_UNIT_CD,
		CODE_VALUE.DISPLAY AS NURSE_UNIT
	FROM
		CODE_VALUE
	WHERE
		CODE_VALUE.CODE_VALUE = 193270 -- HH STRK
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
			AND pi_to_gmt(TRUNC(SYSDATE, 'MONTH') - 1/86400, 'America/Chicago')
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
				) <> ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD THEN TRUNC(pi_from_gmt(ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))))
			WHEN
				LEAD(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD, 1, 0) OVER (
					PARTITION BY ENCNTR_LOC_HIST.ENCNTR_ID 
					ORDER BY ENCNTR_LOC_HIST.ENCNTR_LOC_HIST_ID
				) <> ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD
				AND LAG(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD, 1, 0) OVER (
					PARTITION BY ENCNTR_LOC_HIST.ENCNTR_ID 
					ORDER BY ENCNTR_LOC_HIST.ENCNTR_LOC_HIST_ID
				) = ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD THEN TRUNC(pi_from_gmt(ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))))
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
		AND UNIT_TRNSFRS.TRANSFER_DATE BETWEEN pi_to_gmt(START_MONTH.START_MONTH, 'America/Chicago') AND TRUNC(SYSDATE, 'MONTH') - 1/86400
	GROUP BY
		UNIT_TRNSFRS.TRANSFER_DATE,
		UNIT_TRNSFRS.LOC_NURSE_UNIT_CD
), CENSUS AS (
	SELECT DISTINCT
		TRUNC(pi_from_gmt(LH_CNT_LOC_UNIT_CENSUS.CENSUS_DT_TM, (pi_time_zone(1, @Variable('BOUSER'))))) AS CENSUS_DATE,
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
			AND pi_to_gmt(TRUNC(SYSDATE, 'MONTH') - 1/86400, 'America/Chicago')
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
)

SELECT
	TRUNC(PATIENT_DAYS.CENSUS_DATE, 'MONTH') AS CENSUS_MONTH,
	pi_get_cv_display(PATIENT_DAYS.LOC_NURSE_UNIT_CD) AS NURSE_UNIT,
	SUM(PATIENT_DAYS.PATIENT_DAYS) AS PATIENT_DAYS
FROM
	PATIENT_DAYS	
GROUP BY
	TRUNC(PATIENT_DAYS.CENSUS_DATE, 'MONTH'),
	PATIENT_DAYS.LOC_NURSE_UNIT_CD
