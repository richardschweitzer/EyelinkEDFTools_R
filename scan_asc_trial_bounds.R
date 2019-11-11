# this function scans a vector of strings (asc_file) for trial_start and trial_end codes
# by Richard, 04/2018

scan_asc_trial_bounds <- function(asc_file, 
                                  trial_start_code = "TRIAL_Start",
                                  trialCntr_addon = 0,
                                  trial_end_code = "TRIAL_End", 
                                  assert_consistency = TRUE) {
  require(assertthat)
  
  #### scan for patterns
  # start codes
  trial_i_starts <- grep(x = asc_file, 
                         pattern = paste0(trial_start_code, " ") )
  trial_i_starts_msg <- asc_file[trial_i_starts]
  n_starts <- length(trial_i_starts)
  # end codes
  trial_i_ends <- grep(x = asc_file, 
                       pattern = paste0(trial_end_code, " ") )
  trial_i_ends_msg <- asc_file[trial_i_ends]
  n_ends <- length(trial_i_ends)
  
  # make sure they have the same length
  assertthat::are_equal(n_starts, n_ends)
  
  # now make a comprehensive data.frame
  trial_bound_df <- data.frame(trial_i_starts, trial_i_starts_msg, 
                               trial_i_ends, trial_i_ends_msg)
  # ... and check its consistency
  assertthat::assert_that(all(trial_bound_df$trial_i_starts<trial_bound_df$trial_i_ends))
  # here we check for the right order:
  trial_bound_df$trialCntr <- NA
  trial_bound_df$trial_consistent <- NA
  for (r in 1:nrow(trial_bound_df)) {
    trial_bound_df$trialCntr[r] <- r + trialCntr_addon
    trial_bound_df$trial_consistent[r] <- grepl(x = trial_bound_df$trial_i_starts_msg[r], 
                                                pattern = paste0("\\b", trial_start_code, " ", trial_bound_df$trialCntr[r], "\\b") ) & 
      grepl(x = trial_bound_df$trial_i_ends_msg[r], 
            pattern = paste0("\\b", trial_end_code, " ", trial_bound_df$trialCntr[r], "\\b") )
  }
  if (assert_consistency) {
    assertthat::assert_that(all(trial_bound_df$trial_consistent))
  }
  head(trial_bound_df)
  
  return(trial_bound_df)
}