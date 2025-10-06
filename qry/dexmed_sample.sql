SELECT DISTINCT
    ENCNTR_ALIAS.ALIAS AS FIN
FROM
    CE_MED_RESULT,
    CLINICAL_EVENT,
    ENCNTR_ALIAS,
    ENCNTR_LOC_HIST
WHERE
    CLINICAL_EVENT.EVENT_CD = 37556709 -- dexmedetomidine
    AND CLINICAL_EVENT.EVENT_CLASS_CD = 158 -- MED
    AND CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN
        pi_to_gmt(
            TO_DATE(
                @Prompt('Enter begin date', 'D', , mono, free, persistent, {'12/02/2021 00:00:00'}, User:0), 
                pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
            ), 
            'America/Chicago'
        )
        AND pi_to_gmt(
            TO_DATE(
                @Prompt('Enter end date', 'D', , mono, free, persistent, {'12/03/2021 00:00:00'}, User:1), 
                pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
            ) - 1/86400, 
            'America/Chicago'
        )
    AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
    AND CLINICAL_EVENT.EVENT_ID = CE_MED_RESULT.EVENT_ID
    AND CE_MED_RESULT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
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
        3796, -- HC Childrens
        3821, -- HH Clinics
        3822, -- HH Trans Care
        3823 -- HH Rehab
        -- 1099966301 -- HH Oncology TMC
    )
    AND ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM >= CLINICAL_EVENT.EVENT_END_DT_TM
    AND CLINICAL_EVENT.ENCNTR_ID = ENCNTR_ALIAS.ENCNTR_ID
    AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
    AND ENCNTR_ALIAS.ACTIVE_IND = 1
