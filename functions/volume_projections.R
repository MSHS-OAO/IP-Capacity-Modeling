volume_projections <- function(df, vol_projections_file) {
  
  #create UNIQUE_ID column
  df <- df %>% mutate(UNIQUE_ID = paste0(LOC_NAME, ATTENDING_VERITY_REPORT_SERVICE))
  

  df <- df %>%
    group_by(ENCOUNTER_NO) %>%
    arrange(SERVICE_DATE, LOC_NAME, ATTENDING_VERITY_REPORT_SERVICE, .by_group = TRUE) %>%
    mutate(OG_SEQ = row_number()) %>%
    ungroup()

  
  #reading volme projections file
  volume_percentages <- read_csv(paste0(cap_dir, "Mapping Info/volume projections/", vol_projections_file),
                                 show_col_types = FALSE)
  
  #get unique # of encounters for each UNIQUE_ID in df
  unique_id_count <- unique(df %>% select(ENCOUNTER_NO,UNIQUE_ID)) %>% 
    add_count(UNIQUE_ID, name = "count") %>%
    select(-ENCOUNTER_NO) %>%
    unique()
  # add the # of encounters
  volume_percentages <- volume_percentages %>%
    left_join(unique_id_count, by = "UNIQUE_ID")
  #calculate sample size for each UNIQUE_ID, remove percent = 0 
  volume_percentages <- volume_percentages %>% 
    filter(!is.na(count)) %>%
    filter(PERCENT != 0) %>%
    mutate(sample_size = round(PERCENT*count)) %>%
    mutate(total = round(count + (PERCENT*count)))
  
  
  #get unique ENCOUNTER_NO values for UNIQUE_ID with a valid percent val in volume_percentages (needed population)
  unique_encounters <- df %>%
    semi_join(volume_percentages, by = "UNIQUE_ID") %>%
    select(ENCOUNTER_NO, UNIQUE_ID) %>%
    distinct()
  
  #get date range in dataset
  service_date_range <- range(df$SERVICE_DATE, na.rm = TRUE)
  count = 0
  
  for (i in 1:nrow(volume_percentages)) {
    
    #get first UNIQUE_id and its sample size
    current_unique_id = volume_percentages$UNIQUE_ID[i]
    current_sample_size =  volume_percentages$sample_size[i]
    
    #filter this UNIQUE_ID encounters from unique_encounter_no
    current_encounters <- unique_encounters %>% filter(UNIQUE_ID == current_unique_id)
    
    #sample encounters, sample with replacement when percent > 100 
    sampled_encounters <- sample(
      current_encounters$ENCOUNTER_NO,
      size    = abs(current_sample_size),
      replace = abs(current_sample_size) > length(current_encounters$ENCOUNTER_NO)
    )
    sampled_encounters_df <- tibble(
      SAMPLE_INSTANCE = seq_along(sampled_encounters),
      ENCOUNTER_NO    = sampled_encounters
    )
    
    if(current_sample_size < 0){
      count = count - nrow(df %>% filter(ENCOUNTER_NO %in% sampled_encounters_df$ENCOUNTER_NO))
      df <- df %>% filter(!(ENCOUNTER_NO %in% sampled_encounters_df$ENCOUNTER_NO))          
    }
    else{
      #retrieve full rows with sampled encounter_no values from df
      full_sampled_encounters <- df %>%
        inner_join(sampled_encounters_df, by = "ENCOUNTER_NO", relationship = "many-to-many")
      count = count + nrow(full_sampled_encounters)
      #modify service_dates. mainting LOS
      #add sequential randomized service_dates
      full_sampled_encounters_randomized <- full_sampled_encounters %>%
        group_by(ENCOUNTER_NO, SAMPLE_INSTANCE) %>%
        arrange(OG_SEQ, .by_group = TRUE) %>%
        group_modify(~ {
          d <- .x$SERVICE_DATE
          d_ok <- d[!is.na(d)]
          
          if (length(d_ok) == 0) {
            return(.x)
          }
          
          min_date <- min(d_ok)
          day_offset <- as.integer(d - min_date)
          
          los_days <- max(day_offset, na.rm = TRUE) + 1
          if (!is.finite(los_days) || los_days <= 0) {
            return(.x)
          }
          
          max_start <- service_date_range[2] - (los_days - 1)
          if (!is.finite(max_start) || max_start < service_date_range[1]) {
            return(.x)
          }
          
          start_date <- sample(seq(service_date_range[1], max_start, by = "day"), 1)
          .x$SERVICE_DATE <- start_date + day_offset
          
          .x
        }) %>%
        ungroup() %>%
        mutate(
          SERVICE_MONTH = floor_date(SERVICE_DATE, unit = "month"),
          ENCOUNTER_NO  = paste0(ENCOUNTER_NO, "_", SAMPLE_INSTANCE)
        ) %>%
        select(-SAMPLE_INSTANCE)      
      
      #add sampled/randomized rows back to df
      df <- bind_rows(
        df,
        full_sampled_encounters_randomized
      )
    }
  }
  
  df <- df %>% select(-UNIQUE_ID, OG_SEQ)
  return(df)
}
