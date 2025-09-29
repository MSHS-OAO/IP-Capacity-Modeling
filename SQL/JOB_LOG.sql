SELECT
  job_name,
  enabled,
  state,
  repeat_interval,
  last_start_date,
  next_run_date
FROM
  user_scheduler_jobs
WHERE
  job_name in ('REFRESH_IPCAP_WEEKLY',
               'REFRESH_IPCAP_BEDCHARGES_WEEKLY',
               'REFRESH_IPCAP_PROVIDERS_WEEKLY',
               'REFRESH_IPCAP_PROCEDURES_WEEKLY');

SELECT
  job_name,
  status,
  log_date,
  run_duration,
  additional_info
FROM
  user_scheduler_job_run_details
WHERE
  job_name in ('REFRESH_IPCAP_WEEKLY',
               'REFRESH_IPCAP_BEDCHARGES_WEEKLY',
               'REFRESH_IPCAP_PROVIDERS_WEEKLY',
               'REFRESH_IPCAP_PROCEDURES_WEEKLY')
ORDER BY log_date desc;