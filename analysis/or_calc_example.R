compute_gaps <- function(case_starts, case_ends, pt_start, pt_end) {
  
  # 1. Trim every case to the prime-time window
  busy_start <- pmax(case_starts, pt_start)
  busy_end   <- pmin(case_ends,   pt_end)
  
  # 2. Throw away cases that ended up inverted (fully outside the window)
  keep <- busy_end > busy_start
  bs <- busy_start[keep]
  be <- busy_end[keep]
  
  # 3. No cases? The whole window is one gap.
  if (length(bs) == 0) {
    return(as.numeric(difftime(pt_end, pt_start, units = "mins")))
  }
  
  # 4. Otherwise: gap before first case, gaps between cases, gap after last
  opening <- as.numeric(difftime(bs[1], pt_start, units = "mins"))
  middles <- if (length(bs) > 1)
    as.numeric(difftime(bs[-1], be[-length(be)], units = "mins"))
  closing <- as.numeric(difftime(pt_end, be[length(be)], units = "mins"))
  
  pmax(c(opening, middles, closing), 0)
}


ps <- as.POSIXct("2025-01-02 07:30", tz = "America/New_York")
pe <- as.POSIXct("2025-01-02 17:30", tz = "America/New_York")

starts <- as.POSIXct(c("2025-01-02 06:45", "2025-01-02 10:10", "2025-01-02 14:00"),
                     tz = "America/New_York")
ends   <- as.POSIXct(c("2025-01-02 08:40", "2025-01-02 12:30", "2025-01-02 19:00"),
                     tz = "America/New_York")

compute_gaps(starts, ends, ps, pe)


compute_gaps(starts[1], ends[1], ps, pe)
# one case: 0 530  → opening gap 0, no middles, huge closing gap

compute_gaps(as.POSIXct("2025-01-02 19:00", tz="America/New_York"),
             as.POSIXct("2025-01-02 21:00", tz="America/New_York"), ps, pe)
# evening-only case: 600 → clamp inverted it, keep dropped it, empty branch fired

compute_gaps(starts[c(1,2)], ends[c(2,1)], ps, pe)  # deliberately scrambled order
# garbage! → this is why arrange() before summarise is mandatory