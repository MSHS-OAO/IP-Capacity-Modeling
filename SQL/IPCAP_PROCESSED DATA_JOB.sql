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
       WHERE (DSCH_DT_SRC BETWEEN DATE '2024-06-01' AND DATE '2024-12-31' OR
              ADMIT_DT_SRC BETWEEN DATE '2024-06-01' AND DATE '2024-12-31') AND
              FACILITY_MSX <> 'MSSN'
   ),
   charge AS (
       SELECT *
       FROM OE_CHARGE_DETAIL
       WHERE PRIM_ENC_CSN_ID IN (SELECT DISTINCT EPIC_CSN FROM ip) AND
             SERVICE_DATE BETWEEN 20240601 AND 20241231
   ),
   service_group AS (
       SELECT *
       FROM DASHBD_USER.CLARITY_DEP_REF
       WHERE EXTERNAL_NAME not in ('MSW MAIN 11NU', 'MSW Main 12A (L&D)')
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
   final AS (
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
              ip.PRINCIPAL_SURGEON_CD_SRC,
              ip.PRINCIPAL_SURGEON_NAME_MSX,
              ip.MSDRG_CD_SRC,
              ip.MSDRG_DESC_MSX,
              ip.ADMIT_TYPE_CD_SRC,
              ip.ADMIT_TYPE_DESC_SRC,
              ip.P_AVG_LOS_MSDRG,
              ip.VERITY_DEPT_CD_SRC,
              ip.VERITY_DEPT_DESC_SRC,
              ip.VERITY_DIV_CD_SRC,
              ip.VERITY_DIV_DESC_SRC,
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
              service_group.RPT_GRP_TWENTYTHREE
       FROM ip
       LEFT JOIN charge
         ON ip.EPIC_CSN = charge.PRIM_ENC_CSN_ID
       LEFT JOIN service_group
         ON charge.EPIC_DEPT_ID = service_group.DEPARTMENT_ID
       LEFT JOIN cpt
         ON charge.CPT_HCPCS_C = cpt.CPT
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
    enabled         => TRUE,
    comments        => 'Refreshes IPCAP_PROCESSED_DATA every Wednesday at 1:00 AM'
  );
END;