CREATE OR REPLACE PROCEDURE refresh_ipcap_care_team AS
BEGIN
    -- Disable parallel DML to avoid ORA-12839
   EXECUTE IMMEDIATE 'ALTER SESSION DISABLE PARALLEL DML';

   -- Step 1: delete the existing table
   DELETE FROM IPCAP_CARE_TEAM WHERE 1 = 1;

   -- Step 2: Insert new data
   INSERT INTO IPCAP_CARE_TEAM
   with ip as (
       select distinct
           ENCOUNTER_NO,
           ADMIT_DT_SRC,
           SERVICE_DESC_MSX,
           LOS_NO_SRC,
           VIZ_EX_LOS,
           MSDRG_CD_SRC,
           MSDRG_DESC_MSX
       from IPCAP_BEDCHARGES
   ), care_team as (
       select *
       from MS_INSIGHT.v_temp_22_ra
       where ENCOUNTER_NO in (select distinct ENCOUNTER_NO from ip)
   ), providers as (
       select *
       from MSX_PROVIDER_V
       where NPI in (select distinct NPI from care_team)
   ), final as (
       select c.*,
              p.PROV_TYPE,
              p.VERITY_DEPT_1_CD_SRC,
              p.VERITY_DEPT_1_DESC_SRC,
              p.VERITY_DIV_CD_SRC,
              p.VERITY_DIV_DESC_SRC,
              ip.ADMIT_DT_SRC,
              ip.SERVICE_DESC_MSX,
              ip.LOS_NO_SRC,
              ip.VIZ_EX_LOS,
              ip.MSDRG_CD_SRC,
              ip.MSDRG_DESC_MSX
       from ip
       left join care_team c on ip.ENCOUNTER_NO = c.ENCOUNTER_NO
       left join providers p on c.NPI = p.NPI
   )
   select *
   from final;
END;
/

BEGIN
  DBMS_SCHEDULER.CREATE_JOB (
    job_name        => 'REFRESH_IPCAP_CARE_TEAM_WEEKLY',
    job_type        => 'PLSQL_BLOCK',
    job_action      => 'BEGIN refresh_ipcap_care_team; END;',
    start_date      => SYSTIMESTAMP,
    repeat_interval => 'FREQ=WEEKLY; BYDAY=WED; BYHOUR=2; BYMINUTE=0; BYSECOND=0',
    enabled         => FALSE,
    comments        => 'Refreshes IPCAP_CARE_TEAM every Wednesday at 2:00 AM'
  );
END;