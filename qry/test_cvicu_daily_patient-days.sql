WITH UNIT_ENCNTR_LIST AS (
	SELECT DISTINCT
		ENCNTR_LOC_HIST.ENCNTR_ID
	FROM
		ENCNTR_LOC_HIST
	WHERE
		ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD = 5541 -- HH CVICU
		AND ENCNTR_LOC_HIST.TRANSACTION_DT_TM 
			BETWEEN pi_to_gmt(TRUNC(SYSDATE - 7, 'DAY'), pi_time_zone(2, @Variable('BOUSER')))
			AND pi_to_gmt(TRUNC(SYSDATE, 'DAY') - (1 / 86400), pi_time_zone(2, @Variable('BOUSER')))
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
		SUM(UNIT_TRNSFRS.UNIT_TRANSFER) AS NET_TRANSFERS
	FROM
		UNIT_TRNSFRS
	WHERE
		UNIT_TRNSFRS.UNIT_TRANSFER <> 0
		AND UNIT_TRNSFRS.LOC_NURSE_UNIT_CD = 5541 -- HH CVICU
		AND UNIT_TRNSFRS.TRANSFER_DATE BETWEEN TRUNC(SYSDATE - 7, 'DAY') AND TRUNC(SYSDATE, 'DAY') - (1 / 86400)
	GROUP BY
		UNIT_TRNSFRS.TRANSFER_DATE
), CENSUS AS (
	SELECT DISTINCT
		TRUNC(pi_from_gmt(LH_CNT_LOC_UNIT_CENSUS.CENSUS_DT_TM, (pi_time_zone(1, @Variable('BOUSER'))))) AS CENSUS_DATE,
		LH_CNT_LOC_UNIT_CENSUS.CENSUS_PERSON_TOTAL AS CENSUS
	FROM
		LH_CNT_LOC_UNIT_CENSUS
	WHERE
		LH_CNT_LOC_UNIT_CENSUS.LOC_NURSE_UNIT_CD = 5541 -- HH CVICU
		AND LH_CNT_LOC_UNIT_CENSUS.CENSUS_DT_TM
			BETWEEN pi_to_gmt(TRUNC(SYSDATE - 7, 'DAY'), pi_time_zone(2, @Variable('BOUSER')))
			AND pi_to_gmt(TRUNC(SYSDATE, 'DAY') - (1 / 86400), pi_time_zone(2, @Variable('BOUSER')))
)

SELECT
	CENSUS.CENSUS_DATE,
	CENSUS.CENSUS,
	NVL(UNIT_TRNSFRS_DAILY.NET_TRANSFERS, 0) AS NET_TRANSFERS,
	CENSUS.CENSUS + NVL(UNIT_TRNSFRS_DAILY.NET_TRANSFERS, 0) AS PATIENT_DAYS
FROM
	CENSUS,
	UNIT_TRNSFRS_DAILY
WHERE 
	CENSUS.CENSUS_DATE = UNIT_TRNSFRS_DAILY.TRANSFER_DATE(+)
