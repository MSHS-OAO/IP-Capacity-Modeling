CREATE OR REPLACE PROCEDURE refresh_ipcap_providers AS
BEGIN
    -- Disable parallel DML to avoid ORA-12839
   EXECUTE IMMEDIATE 'ALTER SESSION DISABLE PARALLEL DML';

   -- Step 1: delete the existing table
   DELETE FROM IPCAP_PROVIDERS WHERE 1 = 1;

   -- Step 2: Insert new data
   INSERT INTO IPCAP_PROVIDERS
   with encounter as (
    select distinct
           ENCOUNTER_NO,
           PRINCIPAL_SURGEON_CD_SRC,
           PRINCIPAL_SURGEON_NAME_MSX
    from MSX_IP_OUTPUT
    where (DSCH_DT_SRC BETWEEN DATE '2024-06-01' AND DATE '2025-06-30' OR
           ADMIT_DT_SRC BETWEEN DATE '2024-06-01' AND DATE '2025-06-30') AND
        FACILITY_MSX <> 'MSSN'
    ), proc as (
        select ENCOUNTER_NO,
               PROC_SURGEON
        from MSX_IP_PROC
        where PROC_TYPE = 'P'
    ), surgeon_codes as (
        select distinct proc.PROC_SURGEON,
                        encounter.PRINCIPAL_SURGEON_CD_SRC
        from encounter
        left join proc on encounter.ENCOUNTER_NO = proc.ENCOUNTER_NO
    )
    select surgeon_codes.PROC_SURGEON,
           surgeon_codes.PRINCIPAL_SURGEON_CD_SRC,
           MSX_PROVIDER_V.*
    from surgeon_codes
    left join MSX_PROVIDER_V on surgeon_codes.PRINCIPAL_SURGEON_CD_SRC = MSX_PROVIDER_V.MSH_PROV_CD;
END;
/

BEGIN
  DBMS_SCHEDULER.CREATE_JOB (
    job_name        => 'REFRESH_IPCAP_PROVIDERS_WEEKLY',
    job_type        => 'PLSQL_BLOCK',
    job_action      => 'BEGIN refresh_ipcap_providers; END;',
    start_date      => SYSTIMESTAMP,
    repeat_interval => 'FREQ=WEEKLY; BYDAY=WED; BYHOUR=2; BYMINUTE=0; BYSECOND=0',
    enabled         => TRUE,
    comments        => 'Refreshes IPCAP_PROVIDERS every Wednesday at 2:00 AM'
  );
END;