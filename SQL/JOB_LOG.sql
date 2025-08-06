SELECT
  job_name,
  status,
  log_date,
  run_duration,
  additional_info
FROM
  user_scheduler_job_run_details
WHERE
  job_name IN ('REFRESH_IPCAP_WEEKLY',
               'REFRESH_IPCAP_BEDCHARGES')
ORDER BY
  log_date DESC;