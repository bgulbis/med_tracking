SELECT DISTINCT
	ELH_TRANSFER.TRANSFER_DATE,
	SUM(UNIT_TRANSFER) AS NET_TRANSFERS,
	SYSDATE AS SYS_DATETIME
FROM
	(
		SELECT DISTINCT
			ENCNTR_LOC_HIST.ENCNTR_LOC_HIST_ID AS ELH_ID,
			CV_NURSE_UNIT.DISPLAY AS NURSE_UNIT,
			CASE
				WHEN LAG(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD, 1, 0) OVER (PARTITION BY ENCNTR_LOC_HIST.ENCNTR_ID ORDER BY ENCNTR_LOC_HIST.ENCNTR_LOC_HIST_ID) <> ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD THEN 1
				WHEN
					LEAD(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD, 1, 0) OVER (PARTITION BY ENCNTR_LOC_HIST.ENCNTR_ID ORDER BY ENCNTR_LOC_HIST.ENCNTR_LOC_HIST_ID) <> ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD
					AND LAG(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD, 1, 0) OVER (PARTITION BY ENCNTR_LOC_HIST.ENCNTR_ID ORDER BY ENCNTR_LOC_HIST.ENCNTR_LOC_HIST_ID) = ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD THEN -1
				ELSE 0
			END AS UNIT_TRANSFER,
			CASE
				WHEN LAG(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD, 1, 0) OVER (PARTITION BY ENCNTR_LOC_HIST.ENCNTR_ID ORDER BY ENCNTR_LOC_HIST.ENCNTR_LOC_HIST_ID) <> ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD THEN TRUNC(pi_from_gmt(ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))))
				WHEN
					LEAD(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD, 1, 0) OVER (PARTITION BY ENCNTR_LOC_HIST.ENCNTR_ID ORDER BY ENCNTR_LOC_HIST.ENCNTR_LOC_HIST_ID) <> ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD
					AND LAG(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD, 1, 0) OVER (PARTITION BY ENCNTR_LOC_HIST.ENCNTR_ID ORDER BY ENCNTR_LOC_HIST.ENCNTR_LOC_HIST_ID) = ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD THEN TRUNC(pi_from_gmt(ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))))
			END AS TRANSFER_DATE
		FROM
			CODE_VALUE CV_NURSE_UNIT,
			ENCNTR_LOC_HIST,
			(
				SELECT DISTINCT
					ENCNTR_LOC_HIST.ENCNTR_ID
				FROM
					ENCNTR_LOC_HIST
				WHERE
					ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD = 5541 -- HH CVICU
					AND (
						ENCNTR_LOC_HIST.TRANSACTION_DT_TM + 0
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
						AND ENCNTR_LOC_HIST.TRANSACTION_DT_TM
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
			) ENCNTR_LIST
		WHERE
			ENCNTR_LIST.ENCNTR_ID = ENCNTR_LOC_HIST.ENCNTR_ID
			AND ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD = CV_NURSE_UNIT.CODE_VALUE
	) ELH_TRANSFER
WHERE
	ELH_TRANSFER.UNIT_TRANSFER <> 0
	AND ELH_TRANSFER.NURSE_UNIT = 'HH CVICU'
	AND (
		ELH_TRANSFER.TRANSFER_DATE + 0
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
		AND ELH_TRANSFER.TRANSFER_DATE
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
	ELH_TRANSFER.TRANSFER_DATE
