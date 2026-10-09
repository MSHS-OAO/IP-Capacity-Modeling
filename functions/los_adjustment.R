los_reduction_sim <- function(encounter_days_df) {

  # Read LOS targets
  los_projections <- read_csv(
    paste0(
      cap_dir,
      "Mapping Info/los adjustments/",
      los_projections_file
    ),
    show_col_types = FALSE
  ) %>%
    filter(!is.na(TARGET_LOS)) %>%
    mutate(
      UNIQUE_ID = paste0(Hospital, VERITY_REPORT_SERVICE_MSX)
    ) %>%
    select(UNIQUE_ID, TARGET_LOS) %>%
    distinct()

  # Add service-group identifier
  encounter_daily <- encounter_days_df %>%
    mutate(
      UNIQUE_ID = paste0(
        LOC_NAME,
        ATTENDING_VERITY_REPORT_SERVICE
      )
    )

  # Calculate current service-group ALOS after volume projections
  baseline_los <- encounter_daily %>%
    filter(
      !is.na(LOC_NAME),
      !is.na(ATTENDING_VERITY_REPORT_SERVICE),
      !is.na(ENCOUNTER_NO)
    ) %>%
    group_by(
      LOC_NAME,
      ATTENDING_VERITY_REPORT_SERVICE,
      UNIQUE_ID
    ) %>%
    summarise(
      ENCOUNTERS = n_distinct(ENCOUNTER_NO),
      TOTAL_DAYS = sum(BED_CHARGES, na.rm = TRUE),
      ALOS = TOTAL_DAYS / ENCOUNTERS,
      .groups = "drop"
    )

  # Calculate the number of bed days to remove from each service group
  baseline_projections <- baseline_los %>%
    left_join(los_projections, by = "UNIQUE_ID") %>%
    mutate(
      TARGET_TOTAL_DAYS = floor(TARGET_LOS * ENCOUNTERS),

      DAYS_TO_REMOVE = case_when(
        is.na(TARGET_LOS) ~ 0,
        ALOS <= TARGET_LOS ~ 0,
        TRUE ~ pmax(0, TOTAL_DAYS - TARGET_TOTAL_DAYS)
      )
    )

  # Identify days that could be removed from each encounter
  encounter_daily <- encounter_daily %>%
    left_join(
      baseline_projections %>%
        select(
          UNIQUE_ID,
          TARGET_LOS,
          ALOS,
          ENCOUNTERS,
          TOTAL_DAYS,
          TARGET_TOTAL_DAYS,
          DAYS_TO_REMOVE
        ),
      by = "UNIQUE_ID"
    ) %>%
    arrange(
      LOC_NAME,
      ATTENDING_VERITY_REPORT_SERVICE,
      ENCOUNTER_NO,
      SERVICE_DATE
    ) %>%
    group_by(
      LOC_NAME,
      ATTENDING_VERITY_REPORT_SERVICE,
      ENCOUNTER_NO
    ) %>%
    mutate(
      LOS = sum(BED_CHARGES, na.rm = TRUE),

      DAY_NUMBER = row_number(),

      # Latest positive bed day = 1; next latest = 2
      TRIM_STEP = rev(cumsum(rev(BED_CHARGES > 0))),

      # LOS immediately before this day would be removed
      LOS_BEFORE_REMOVAL = LOS - (TRIM_STEP - 1),

      REMOVABLE_DAY =
        BED_CHARGES > 0 &
        DAY_NUMBER > 1 &
        !is.na(TARGET_LOS) &
        LOS_BEFORE_REMOVAL > TARGET_LOS
    ) %>%
    ungroup()

  # Select days within each service group
  encounter_daily <- encounter_daily %>%
    group_by(UNIQUE_ID) %>%
    group_modify(~ {
      df <- .x
      df$REMOVE_DAY <- FALSE

      quota <- df$DAYS_TO_REMOVE[1]

      if (is.na(quota) || quota <= 0) {
        return(df)
      }

      eligible_rows <- which(df$REMOVABLE_DAY %in% TRUE)

      if (length(eligible_rows) == 0) {
        return(df)
      }

      # Store each encounter's eligible days, latest first
      eligible <- split(
        eligible_rows,
        as.character(df$ENCOUNTER_NO[eligible_rows])
      )

      eligible <- lapply(
        eligible,
        function(i) {
          i[order(df$SERVICE_DATE[i], decreasing = TRUE)]
        }
      )

      remaining_los <- vapply(
        eligible,
        function(i) df$LOS[i[1]],
        numeric(1)
      )

      target <- df$TARGET_LOS[1]
      max_removals <- min(as.integer(quota), sum(lengths(eligible)))

      for (step in seq_len(max_removals)) {

        available <- names(eligible)[lengths(eligible) > 0]

        if (length(available) == 0) {
          break
        }

        # An encounter remains eligible only while above target
        excess <- remaining_los[available] - target
        available <- available[excess > 0]

        if (length(available) == 0) {
          break
        }

        # Longer encounters are more likely to be selected,
        # but every eligible encounter has a chance
        weights <- sqrt(remaining_los[available] - target)

        chosen <- sample(
          available,
          size = 1,
          prob = weights
        )

        # Remove one latest eligible day, then draw again
        row_to_remove <- eligible[[chosen]][1]
        df$REMOVE_DAY[row_to_remove] <- TRUE

        eligible[[chosen]] <- eligible[[chosen]][-1]
        remaining_los[chosen] <- remaining_los[chosen] - 1
      }

      df
    }) %>%
    ungroup()

  # Remove selected days and recalculate encounter dates
  encounter_days_adjusted <- encounter_daily %>%
    filter(!REMOVE_DAY) %>%
    arrange(
      LOC_NAME,
      ENCOUNTER_NO,
      SERVICE_DATE
    ) %>%
    group_by(
      LOC_NAME,
      ENCOUNTER_NO
    ) %>%
    mutate(
      NEW_ADMIT_DT_SRC = min(SERVICE_DATE, na.rm = TRUE),
      NEW_DSCH_DT_SRC = max(SERVICE_DATE, na.rm = TRUE) + days(1)
    ) %>%
    ungroup() %>%
    select(
      -ENCOUNTERS,
      -TOTAL_DAYS,
      -TARGET_TOTAL_DAYS,
      -DAYS_TO_REMOVE,
      -DAY_NUMBER,
      -TRIM_STEP,
      -LOS_BEFORE_REMOVAL,
      -REMOVABLE_DAY,
      -REMOVE_DAY
    )

  return(encounter_days_adjusted)
}

