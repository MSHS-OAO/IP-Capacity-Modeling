CREATE OR REPLACE PROCEDURE refresh_ipcap_procedure_data AS
BEGIN
    -- Disable parallel DML to avoid ORA-12839
   EXECUTE IMMEDIATE 'ALTER SESSION DISABLE PARALLEL DML';

   -- Step 1: delete the existing table
   DELETE FROM IPCAP_PROCEDURE_DATA WHERE 1 = 1;

   -- Step 2: Insert new data
   INSERT INTO IPCAP_PROCEDURE_DATA
       WITH ip AS (
       SELECT *
       FROM MSX_IP_OUTPUT
       WHERE (DSCH_DT_SRC BETWEEN DATE '2024-06-01' AND DATE '2025-09-30' OR
              ADMIT_DT_SRC BETWEEN DATE '2024-06-01' AND DATE '2025-09-30') AND
              FACILITY_MSX <> 'MSSN'
       ), proc AS (
           SELECT *
           FROM MSX_IP_PROC
           WHERE ENCOUNTER_NO in (select ENCOUNTER_NO from ip)
       ), procedures AS (
           SELECT *
           FROM IPCAP_PROCEDURE_MAPPING
           WHERE ICD_CODE in (select SEC_PROC_CD from proc)
       ), surgeon as (
           SELECT *
           FROM MSX_PROVIDER_V
       ), final AS (
           SELECT ip.ENCOUNTER_NO,
                  ip.MSMRN,
                  ip.FACILITY_MSX,
                  ip.DSCH_DT_SRC,
                  ip.DSCH_TIME_SRC,
                  ip.ADMIT_DT_SRC,
                  ip.ADMIT_TIME_SRC,
                  ip.LOS_NO_SRC,
                  ip.ATTENDING_MD_CD_SRC,
                  ip.ATTENDING_MD_NAME_MSX,
                  ip.MSDRG_CD_SRC,
                  ip.MSDRG_DESC_MSX,
                  ip.ADMIT_TYPE_CD_SRC,
                  ip.ADMIT_TYPE_DESC_SRC,
                  ip.P_AVG_LOS_MSDRG,
                  ip.VERITY_DEPT_CD_SRC,
                  ip.VERITY_DEPT_DESC_SRC,
                  ip.VERITY_DIV_CD_SRC,
                  ip.VERITY_DIV_DESC_SRC,
                  proc.SEC_PROC_CD,
                  procedures.SHORT_DESCRIPTION,
                  proc.SEC_PROC_NUM,
                  proc.PROC_DT,
                  proc.PROC_TYPE,
                  ip.PRINCIPAL_SURGEON_CD_SRC,
                  ip.PRINCIPAL_SURGEON_NAME_MSX,
                  proc.PROC_SURGEON,
                  surgeon.LAST_NAME || ', '|| surgeon.FIRST_NAME  as PROC_SURGEON_NAME,
                  surgeon.VERITY_DEPT_1_DESC_SRC as SURGEON_VERITY_DEPT_DESC,
                  surgeon.VERITY_DIV_DESC_SRC as SURGEON_VERITY_DIV_DESC
           FROM ip
           LEFT JOIN proc on ip.ENCOUNTER_NO = proc.ENCOUNTER_NO
           LEFT JOIN surgeon on proc.PROC_NPI = surgeon.NPI
           LEFT JOIN procedures on proc.SEC_PROC_CD = procedures.ICD_CODE
       )
       select * from final;
END;
/

BEGIN
  DBMS_SCHEDULER.CREATE_JOB (
    job_name        => 'REFRESH_IPCAP_PROCEDURES_WEEKLY',
    job_type        => 'PLSQL_BLOCK',
    job_action      => 'BEGIN refresh_ipcap_procedure_data; END;',
    start_date      => SYSTIMESTAMP,
    repeat_interval => 'FREQ=WEEKLY; BYDAY=WED; BYHOUR=3; BYMINUTE=0; BYSECOND=0',
    enabled         => FALSE,
    comments        => 'Refreshes IPCAP_PROCEDURE_DATA every Wednesday at 3:00 AM'
  );
END;