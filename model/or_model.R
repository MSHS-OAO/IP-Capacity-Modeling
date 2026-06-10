source('functions/or_constants_functions.R')


or_cases <- get_or_data(sched_start_date = '2025-01-01', sched_end_date = '2025-12-31',status = 'Completed')
summary_metrics_baseline <- summary_metrics(or_cases)