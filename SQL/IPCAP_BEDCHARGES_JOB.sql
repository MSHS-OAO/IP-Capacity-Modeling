CREATE OR REPLACE PROCEDURE refresh_ipcap_bedcharges AS
BEGIN
    -- Disable parallel DML to avoid ORA-12839
   EXECUTE IMMEDIATE 'ALTER SESSION DISABLE PARALLEL DML';

   -- Step 1: delete the existing table
   DELETE FROM IPCAP_BEDCHARGES WHERE 1 = 1;

   -- Step 2: Insert new data
   INSERT INTO IPCAP_BEDCHARGES
   SELECT *
   FROM IPCAP_PROCESSED_DATA
   WHERE CATEGORY = 'IP';
END;
/

BEGIN
  DBMS_SCHEDULER.CREATE_JOB (
    job_name        => 'REFRESH_IPCAP_BEDCHARGES_WEEKLY',
    job_type        => 'PLSQL_BLOCK',
    job_action      => 'BEGIN refresh_ipcap_bedcharges; END;',
    start_date      => SYSTIMESTAMP,
    repeat_interval => 'FREQ=WEEKLY; BYDAY=WED; BYHOUR=4; BYMINUTE=0; BYSECOND=0',
    enabled         => TRUE,
    comments        => 'Refreshes IPCAP_BEDCHARGES every Wednesday at 4:00 AM'
  );
END;