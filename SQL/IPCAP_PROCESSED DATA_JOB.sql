CREATE OR REPLACE PROCEDURE refresh_ipcap_processed_data AS
BEGIN
    -- Disable parallel DML to avoid ORA-12839
   EXECUTE IMMEDIATE 'ALTER SESSION DISABLE PARALLEL DML';

   -- Step 1: delete the existing table
   DELETE FROM IPCAP_PROCESSED_DATA WHERE 1 = 1;

   -- Step 2: Insert new data
   INSERT INTO IPCAP_PROCESSED_DATA
   WITH ip AS (
       SELECT *
       FROM MSX_IP_OUTPUT
       WHERE (DSCH_DT_SRC BETWEEN DATE '2024-06-01' AND DATE '2025-09-30' OR
              ADMIT_DT_SRC BETWEEN DATE '2024-06-01' AND DATE '2025-09-30') AND
              FACILITY_MSX <> 'MSSN'
   ),
   charge AS (
       SELECT *
       FROM OE_CHARGE_DETAIL
       WHERE HSP_ACCOUNT_ID IN (SELECT DISTINCT ENCOUNTER_NO FROM ip) AND
             SERVICE_DATE BETWEEN 20240601 AND 20250930
   ),
   service_group AS (
       select LOC_NAME, EPIC_DEPT_ID, EXTERNAL_NAME, SERVICE_GROUP, VALID_FROM,
              NVL(VALID_TO, SYSDATE) AS VALID_TO
       from IPCAP_SERVICE_GROUPS
   ),
   cpt AS (
       SELECT CPT, CPT_COUNT, LAB_COUNT, DESCRIPTION_SHORT
       FROM IPCAP_CPT_MAPPING
       WHERE CPT IN (SELECT DISTINCT CPT_HCPCS_C FROM charge)
   ),
   billing_cat AS (
       SELECT *
       FROM IPCAP_BILLING_CAT_DESC
   ),
   principal_surgeon AS (
       SELECT *
       FROM MSX_PROVIDER_V
   ),
   final AS (
       SELECT ip.ENCOUNTER_NO,
              ip.MSMRN,
              ip.FACILITY_MSX,
              ip.DSCH_DT_SRC,
              ip.DSCH_TIME_SRC,
              ip.ADMIT_DT_SRC,
              ip.ADMIT_TIME_SRC,
              ip.LOS_NO_SRC,
              ip.UNIT_DESC_MSX,
              ip.ATTENDING_MD_CD_SRC,
              ip.ATTENDING_MD_NAME_MSX,
              ip.VERITY_DEPT_CD_SRC AS ATTENDING_VERITY_DEPT_CD,
              ip.VERITY_DEPT_DESC_SRC AS ATTENDING_VERITY_DEPT_DESC,
              ip.VERITY_DIV_CD_SRC AS ATTENDING_VERITY_DIV_CD,
              ip.VERITY_DIV_DESC_SRC AS ATTENDING_VERITY_DIV_DESC,
              ip.VERITY_REPORT_SERVICE_MSX AS ATTENDING_VERITY_REPORT_SERVICE,
              ip.PRINCIPAL_SURGEON_CD_SRC,
              ip.PRINCIPAL_SURGEON_NAME_MSX,
              principal_surgeon.VERITY_DEPT_1_DESC_SRC as PRINCIPAL_SURGEON_VERITY_DEPT_DESC,
              principal_surgeon.VERITY_DIV_DESC_SRC as PRINCIPAL_SURGEON_VERITY_DIV_DESC,
              ip.MSDRG_CD_SRC,
              ip.MSDRG_DESC_MSX,
              ip.ADMIT_TYPE_CD_SRC,
              ip.ADMIT_TYPE_DESC_SRC,
              ip.VIZ_EX_LOS,
              ip.SERVICE_DESC_MSX,
              charge.FACILITY_ABBR,
              charge.COST_CENTER_C,
              charge.CHARGE_C,
              charge.EPIC_DEPT_ID,
              charge.EPIC_DEPT_NAME,
              charge.QUANTITY,
              charge.SERVICE_DATE,
              charge.CPT_HCPCS_C,
              cpt.CPT_COUNT,
              cpt.LAB_COUNT,
              cpt.DESCRIPTION_SHORT,
              charge.BILLING_CAT_C,
              charge.BILLING_CAT_DESC,
              billing_cat.CATEGORY,
              charge.RPT_GRP_NINETEEN_C,
              charge.RPT_GRP_NINETEEN_DESC,
              charge.NEW_COST_CENTER_C,
              charge.NEW_GL_COMPONENT,
              service_group.EXTERNAL_NAME,
              service_group.SERVICE_GROUP,
              service_group.LOC_NAME
       FROM ip
       LEFT JOIN charge
         ON ip.ENCOUNTER_NO = charge.HSP_ACCOUNT_ID
       LEFT JOIN service_group
         ON charge.EPIC_DEPT_ID = service_group.EPIC_DEPT_ID AND
            TO_DATE(charge.SERVICE_DATE, 'YYYYMMDD') BETWEEN service_group.VALID_FROM AND service_group.VALID_TO
       LEFT JOIN cpt
         ON charge.CPT_HCPCS_C = cpt.CPT
       LEFT JOIN principal_surgeon
         ON ip.PRINCIPAL_SURGEON_CD_SRC = principal_surgeon.MSH_PROV_CD
       LEFT JOIN billing_cat
         ON charge.BILLING_CAT_DESC = billing_cat.BILLING_CAT_DESC
   )
   SELECT * FROM final;
END;
/

BEGIN
  DBMS_SCHEDULER.CREATE_JOB (
    job_name        => 'REFRESH_IPCAP_WEEKLY',
    job_type        => 'PLSQL_BLOCK',
    job_action      => 'BEGIN refresh_ipcap_processed_data; END;',
    start_date      => SYSTIMESTAMP,
    repeat_interval => 'FREQ=WEEKLY; BYDAY=WED; BYHOUR=1; BYMINUTE=0; BYSECOND=0',
    enabled         => FALSE,
    comments        => 'Refreshes IPCAP_PROCESSED_DATA every Wednesday at 1:00 AM'
  );
END;