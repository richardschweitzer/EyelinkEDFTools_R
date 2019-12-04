##### function including SEARCH ######
extract_trial_from_edf <- function(trial_i, 
                                   dom_eye,
                                   asc_file, 
                                   col_names = c("t", "x", "y", "pupil"),
                                   msg_of_interest = c("EVENT_cueOn", "EVENT_timeSaccade", 
                                                       "EVENT_timeMoveStarted", "EVENT_timeMoveFinished", 
                                                       "EVENT_boundaryCross"), 
                                   trial_start_code = "TRIAL_Start",
                                   trial_end_code = "TRIAL_End",
                                   msg_separator = "\t",
                                   normal_msg_length = 8,
                                   used_mode_1440 = TRUE,
                                   use_search = "stringi", 
                                   use_end_of_string = FALSE) {
  # aux functions
  strsplits <- function(x, splits, ...) {
    # http://stackoverflow.com/questions/10738729/r-strsplit-with-multiple-unordered-split-arguments
    for (split in splits)
    {
      x <- unlist(strsplit(x, split, ...))
    }
    return(x[!x == ""]) # Remove empty values (it actually returns nonempty values - dose not remove anything)
  }
  
  check_if_message <- function(string_list, critical_element_at = 1, print_line = FALSE) {
    is_message <- TRUE
    # check whether the first argument is a timestamp (i.e., no MSG or EFIX or ESAC)
    first_element_ok = suppressWarnings(!is.na(as.numeric(string_list[[1]][critical_element_at])))
    if (first_element_ok) {
      is_message <- FALSE
    }
    if (print_line) {
      print(unlist(string_list))
    }
    return(is_message)
  }
  
  len_asc_file <- length(asc_file)
  # find respective trial
  # see: https://stackoverflow.com/questions/26813667/how-to-use-grep-to-find-exact-match
  if (use_search=="stringi") { # fast search using package stringi
    require(stringi)
    (trial_i_start <- which(stri_detect_regex(str = asc_file, 
                                              pattern = paste0("\\b", trial_start_code, " ", trial_i, "\\b"))) )
    (trial_i_temp <- which(stri_detect_regex(str = asc_file[trial_i_start:len_asc_file], 
                                            pattern = paste0("\\b", trial_end_code, " ", trial_i, "\\b"))) )
    (trial_i_end <- trial_i_start+trial_i_temp-1)
  } else if (use_search=="grepl") { # slower search using grepl
    (trial_i_start <- grep(x = asc_file, 
                           pattern = paste0("\\b", trial_start_code, " ", trial_i, "\\b") ))
    (trial_i_temp <- grep(x = asc_file[trial_i_start:len_asc_file], 
                         pattern = paste0("\\b", trial_end_code, " ", trial_i, "\\b") ))
    (trial_i_end <- trial_i_start+trial_i_temp-1)
  } 
  assertthat::assert_that(trial_i_end>trial_i_start)
  trial_bounds <- c(trial_i_start, trial_i_end)
  names(trial_bounds) <- c("trial_i_start", "trial_i_end")
  
  # extract timestamps of interest
  asc_file_trial_i <- asc_file[trial_i_start:trial_i_end]
  c_row <- NULL
  for (msg in msg_of_interest) {
    # see how we want to deal with the EVENT_sac and EVENT_sacend case...
    if (use_end_of_string) {
      msg <- paste0(msg,'$')
    }
    # look for specific message within trial data
    msg_line <- asc_file_trial_i[grep(x = asc_file_trial_i, pattern = msg)]
    if (length(msg_line)==1) { # msg_line found
      msg_line_split <- strsplit(x = msg_line, split = msg_separator)[[1]]
      msg_time <- as.numeric(msg_line_split[2])
    } else if (length(msg_line)==0) { # no msg_line found
      msg_time <- NA
      print_this <- paste("No match found for", msg, "in trial:", trial_i)
      #print(print_this)
      warning(print_this)
    } else { # more than 1 instance found, which shouldn't happen
      msg_time <- NA
      print_this <- paste("More than 1 matches found for:", msg, "- lines:", msg_line)
      #print(print_this)
      warning(print_this)
    }
    # add to row
    c_row <- c(c_row, msg_time)
  }
  # add names
  names(c_row) <- msg_of_interest
  
  ### split all lines and choose only those that contain only data and no messages
  # split line to single elements
  asc_file_trial_i_list <- lapply(asc_file_trial_i, strsplits, splits = msg_separator)
  # remove those lines that include MSG et al.
  is_not_msg <- unlist(lapply(asc_file_trial_i_list, length))==normal_msg_length &
    unlist(lapply(asc_file_trial_i_list, check_if_message))==FALSE
  asc_file_trial_i_list <- asc_file_trial_i_list[is_not_msg]
  # convert to numeric
  asc_file_trial_i_list <- suppressWarnings(lapply(asc_file_trial_i_list, as.numeric))
  # convert to data.frame
  asc_file_trial_i_df <- suppressWarnings(data.frame(Reduce(rbind, asc_file_trial_i_list)))
  asc_file_trial_i_df <- asc_file_trial_i_df[ ,-length(colnames(asc_file_trial_i_df))]
  # extract dominant eye: has to be collected from const.eye_used (0: left, 1: right)
  if (nrow(asc_file_trial_i_df)!=0) {
    if (dom_eye==0) {
      asc_file_trial_i_df <- asc_file_trial_i_df[ ,c(1,2:4)]
    } else if (dom_eye==1) {
      asc_file_trial_i_df <- asc_file_trial_i_df[ ,c(1,5:7)]
    }
    # was 1440 Hz mode used? if so, correct gaze position data
    if (nrow(asc_file_trial_i_df)!=0 & used_mode_1440) {
      asc_file_trial_i_df[ ,c(2,3)] <- asc_file_trial_i_df[ ,c(2,3)] / 2
    }
  } else {
    asc_file_trial_i_df <- data.frame(t = NA, x = NA, y = NA, pupil = NA)
  }
  # colnames
  colnames(asc_file_trial_i_df) <- col_names
  
  # return timestamps and data
  return(list(msg = c_row, 
              eye_raw = asc_file_trial_i_df,
              trial_bounds = trial_bounds))
}

##### the function WITHOUT SEARCH #####
extract_trial_from_edf_nosearch <- function(trial_i_start, trial_i_end, 
                                            dom_eye,
                                            asc_file, 
                                            col_names = c("t", "x", "y", "pupil"),
                                            msg_of_interest = c("EVENT_cueOn", "EVENT_timeSaccade", 
                                                                "EVENT_timeMoveStarted", "EVENT_timeMoveFinished", 
                                                                "EVENT_boundaryCross"), 
                                            msg_separator = "\t",
                                            normal_msg_length = 8,
                                            used_mode_1440 = TRUE, 
                                            use_end_of_string = FALSE) {
  # aux functions
  strsplits <- function(x, splits, ...) {
    # http://stackoverflow.com/questions/10738729/r-strsplit-with-multiple-unordered-split-arguments
    for (split in splits)
    {
      x <- unlist(strsplit(x, split, ...))
    }
    return(x[!x == ""]) # Remove empty values (it actually returns nonempty values - dose not remove anything)
  }
  
  check_if_message <- function(string_list, critical_element_at = 1, print_line = FALSE) {
    is_message <- TRUE
    # check whether the first argument is a timestamp (i.e., no MSG or EFIX or ESAC)
    first_element_ok = suppressWarnings(!is.na(as.numeric(string_list[[1]][critical_element_at])))
    if (first_element_ok) {
      is_message <- FALSE
    }
    if (print_line) {
      print(paste0("is_message=", is_message))
      print(unlist(string_list))
    }
    return(is_message)
  }
  
  # assert some things
  assertthat::assert_that(trial_i_end>trial_i_start)
  trial_bounds <- c(trial_i_start, trial_i_end)
  names(trial_bounds) <- c("trial_i_start", "trial_i_end")
  
  # extract timestamps of interest
  asc_file_trial_i <- asc_file[trial_i_start:trial_i_end]
  c_row <- NULL
  for (msg in msg_of_interest) {
    # see how we want to deal with the EVENT_sac and EVENT_sacend case...
    if (use_end_of_string) {
      msg <- paste0(msg,'$')
    }
    # look for specific message within trial data
    msg_line <- asc_file_trial_i[grep(x = asc_file_trial_i, pattern = msg)]
    if (length(msg_line)==1) { # msg_line found
      msg_line_split <- strsplit(x = msg_line, split = msg_separator)[[1]] # strsplit
      msg_time <- as.numeric(msg_line_split[2])
    } else if (length(msg_line)==0) { # no msg_line found
      msg_time <- NA
      print_this <- paste("No match found for", msg, 
                          "within trial indices:", trial_i_start, "-", trial_i_end)
      #print(print_this)
      warning(print_this)
    } else { # more than 1 instance found, which shouldn't happen
      msg_time <- NA
      print_this <- paste("More than 1 matches found for:", msg, 
                          "- lines:", msg_line)
      #print(print_this)
      warning(print_this)
    }
    # add to row
    c_row <- c(c_row, msg_time)
  }
  # add names
  names(c_row) <- msg_of_interest
  
  ### split all lines and choose only those that contain only data and no messages
  # split line to single elements
  asc_file_trial_i_list <- lapply(asc_file_trial_i, strsplits, splits = msg_separator)
  # remove those lines that include MSG et al.
  is_not_msg <- unlist(lapply(asc_file_trial_i_list, length))==normal_msg_length &
    unlist(lapply(asc_file_trial_i_list, check_if_message))==FALSE
  asc_file_trial_i_list <- asc_file_trial_i_list[is_not_msg]
  # convert to numeric
  asc_file_trial_i_list <- suppressWarnings(lapply(asc_file_trial_i_list, as.numeric))
  # convert to data.frame
  asc_file_trial_i_df <- suppressWarnings(data.frame(Reduce(rbind, asc_file_trial_i_list)))
  asc_file_trial_i_df <- asc_file_trial_i_df[ ,-length(colnames(asc_file_trial_i_df))]
  # extract dominant eye: has to be collected from const.eye_used (0: left, 1: right)
  if (nrow(asc_file_trial_i_df)!=0) {
    if (dom_eye==0) {
      asc_file_trial_i_df <- asc_file_trial_i_df[ ,c(1,2:4)]
    } else if (dom_eye==1) {
      asc_file_trial_i_df <- asc_file_trial_i_df[ ,c(1,5:7)]
    }
    # was 1440 Hz mode used? If so, then correct data
    if (nrow(asc_file_trial_i_df)!=0 & used_mode_1440) { 
      asc_file_trial_i_df[ ,c(2,3)] <- asc_file_trial_i_df[ ,c(2,3)] / 2 # cols: time, x, y, pupil
    }
  } else {
    asc_file_trial_i_df <- data.frame(t = NA, x = NA, y = NA, pupil = NA)
  }
  
  # colnames
  colnames(asc_file_trial_i_df) <- col_names
  
  # return timestamps and data
  return(list(msg = c_row, 
              eye_raw = asc_file_trial_i_df,
              trial_bounds = trial_bounds))
}


#### TEST SECTION ####
test_extract_trial_from_edf <- FALSE

if (test_extract_trial_from_edf) {
  
  library(tictoc)
  
  # load EDF-asc converter
  source("~/Dropbox/PROMOTION WORKING FOLDER/General/EDF2ASC/convert_edf_to_asc.R")
  
  # codes and stuff
  trial_start_code <- "TRIAL_Start"
  trial_end_code <- "TRIAL_End"
  msg_of_interest <- c("EVENT_cueOn", "EVENT_timeSaccade", 
                       "EVENT_timeMoveStarted", "EVENT_timeMoveFinished", 
                       "EVENT_boundaryCross")
  data_columns_needed <- c("eye_time", "eyel_x", "eyel_y", "eyel_p", "eyer_x", "eyer_y", "eyer_p")
  msg_separator <- "\t"
  used_mode_1440 <- TRUE # was 1440 Hz mode used?
  
  # read file
  asc_name <- convert_edf_to_asc(file_path = "~/DATA/STRK/Edf", edf_file = "STRKAK01.edf")
  asc_file <- readLines(con=asc_name)
  str(asc_file)
  
  ##### scan file for trial boundaries #####
  trial_i_starts <- grep(x = asc_file, 
                         pattern = paste0(trial_start_code, " ") )
  trial_i_starts_msg <- asc_file[trial_i_starts]
  n_starts <- length(trial_i_starts)
  trial_i_ends <- grep(x = asc_file, 
                        pattern = paste0(trial_end_code, " ") )
  trial_i_ends_msg <- asc_file[trial_i_ends]
  n_ends <- length(trial_i_ends)
  assertthat::are_equal(n_starts, n_ends)
  # make a comprehensive frame
  trial_bound_df <- data.frame(trial_i_starts, trial_i_starts_msg, 
                               trial_i_ends, trial_i_ends_msg)
  # check consistency
  assertthat::assert_that(all(trial_bound_df$trial_i_starts<trial_bound_df$trial_i_ends))
  trial_bound_df$trialCntr <- NA
  trial_bound_df$trial_consistent <- NA
  for (r in 1:nrow(trial_bound_df)) {
    trial_bound_df$trialCntr[r] <- r
    trial_bound_df$trial_consistent[r] <- grepl(x = trial_bound_df$trial_i_starts_msg[r], 
                                                pattern = paste0("\\b", trial_start_code, " ", r, "\\b") ) & 
                                          grepl(x = trial_bound_df$trial_i_ends_msg[r], 
                                                pattern = paste0("\\b", trial_end_code, " ", r, "\\b") )
  }
  assertthat::assert_that(all(trial_bound_df$trial_consistent))
  head(trial_bound_df)
  
  ##### scan for trial boundaries with function #####
  source('~/Dropbox/PROMOTION WORKING FOLDER/General/ASC parser/scan_asc_trial_bounds.R')
  trial_bound_df2 <- scan_asc_trial_bounds(asc_file)
  assertthat::assert_that(all(trial_bound_df==trial_bound_df2))
  
  ### test with search
  trial_i = 10 # this is trialCntr
  tic()
  res <- extract_trial_from_edf(trial_i = trial_i, asc_file = asc_file, dom_eye = 0)
  toc()
  plot(res$eye_raw$x[res$eye_raw$t>=res$msg[3] &
                       res$eye_raw$t<=res$msg[4]], 
       res$eye_raw$y[res$eye_raw$t>=res$msg[3] &
                       res$eye_raw$t<=res$msg[4]])
  
  ### test without search
  tic()
  res <- extract_trial_from_edf_nosearch(trial_i_start = trial_bound_df2$trial_i_starts[trial_i],
                                         trial_i_end = trial_bound_df2$trial_i_ends[trial_i],
                                         asc_file = asc_file, dom_eye = 0)
  toc()
  plot(res$eye_raw$x[res$eye_raw$t>=res$msg[3] &
                       res$eye_raw$t<=res$msg[4]], 
       res$eye_raw$y[res$eye_raw$t>=res$msg[3] &
                       res$eye_raw$t<=res$msg[4]])
  
}
