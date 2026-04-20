--IPCAP_OR_CASE_DATA
CREATE TABLE IPCAP_OR_CASE_DATA as
    WITH ip AS (
       SELECT *
       FROM MSX_IP_OUTPUT
       WHERE (DSCH_DT_SRC BETWEEN DATE '2025-01-01' AND DATE '2025-12-31' OR
              ADMIT_DT_SRC BETWEEN DATE '2025-01-01' AND DATE '2025-12-31') AND
              FACILITY_MSX <> 'MSSN'),
    cases as (
       SELECT *
       FROM MS_INSIGHT.OR_QUALITY_DASHBOARD_CASE_DETAILS),
    or_room as (
        SELECT *
        FROM MS_INSIGHT.OR_QUALITY_DASHBOARD_ROOM_MASTER)
    SELECT
        ip.FACILITY_MSX,
        ip.ENCOUNTER_NO,
        ip.MSMRN,
        ip.AGE_ENC,
        ip.ADMIT_DT_SRC,
        ip.DSCH_DT_SRC,
        ip.FISCAL_YR_MSX,
        ip.FISCAL_MON_MSX,
        ip.LOS_NO_SRC,
        ip.PRINCIPAL_SURGEON_CD_SRC,
        ip.PRINCIPAL_SURGEON_NAME_MSX,
        ip.MSDRG_CD_SRC,
        ip.MSDRG_DESC_MSX,
        ip.ADMIT_TYPE_CD_SRC,
        ip.ADMIT_TYPE_DESC_SRC,
        ip.VERITY_DEPT_CD_SRC,
        ip.VERITY_DEPT_DESC_SRC,
        ip.VERITY_DIV_CD_SRC,
        ip.VERITY_DIV_DESC_SRC,
        or_room.LOCATION_NM,
        or_room.ROOM_ID,
        or_room.CLUSTER_NAME,
        cases.CLINIC_DESC_SRC,
        cases.CLINIC_GROUP_DESC_MSX,
        cases.OR_CASE_ID,
        cases.SURGERY_DATE,
        cases.PAT_CLASS_NAME,
        cases.SURGEON_NPI,
        cases.PRIMARY_SURGEON,
        cases.SURGEON_SPECIALTY,
        cases.PRIMARY_PROC_CODE,
        cases.PRIMARY_PROCEDURE,
        cases.SCHED_IN_ROOM_DTTM,
        cases.SCHED_START_TIME,
        cases.PATIENT_IN_ROOM_DTTM,
        cases.PATIENT_OUT_ROOM_DTTM,
        cases.MINUTES_IN_ROOM_TO_OUT_ROOM,
        cases.TURNOVER_FROM_PRIOR_CASE,
        cases.TIME_SCHEDULED,
        cases.CASE_STATUS,
        cases.TIME_FROM_ARRIVAL_TO_OR_IN,
        cases.TIME_FROM_HOLDING_TO_OR_IN,
        cases.TIME_FROM_ARRIVAL_TO_HOLDING,
        cases.TIME_FROM_HP_TO_OR_IN,
        cases.TIME_FROM_NURSING_READY_TO_OR_IN,
        cases.TIME_FROM_OR_IN_TO_ANES_READY,
        cases.READINESS_BOARD_HP,
        cases.READINESS_BOARD_ANES,
        cases.READINESS_BOARD_PREPROC,
        cases.READINESS_BOARD_ROOM,
        cases.ROBOTIC_SURGERY_DAVINCI_YN
    FROM ip
    LEFT JOIN cases on
        ip.MSMRN = cases.PAT_MRN_ID and
        cases.SURGERY_DATE between ip.ADMIT_DT_SRC and ip.DSCH_DT_SRC
    LEFT JOIN or_room on
        cases.OR_ID = or_room.ROOM_ID;