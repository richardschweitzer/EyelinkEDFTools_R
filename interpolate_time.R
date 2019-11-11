interpolate_time <- function(eyelink_time, pc_time, use_robust = FALSE) {
  # this function tries to find a linear relationship between the clocks of the eyelink and the experimental computer.
  # it does that with a linear regression and returns the object of the model, so that it can later be used to correct timestamps.
  require(assertthat)
  assertthat::are_equal(length(eyelink_time), length(pc_time))
  assertthat::assert_that(length(eyelink_time)>1)

  ##### prepare the timestamps #####
  # set beginning to zero to be able to ignore intercept
  eyelink_start_time = eyelink_time[1]
  eyelink_time = eyelink_time - eyelink_start_time
  pc_start_time = pc_time[1]
  pc_time = pc_time - pc_start_time
  
  ##### fit the model #####
  if (use_robust) {
    require(robust)
    robust_control <- lmRob.control(mxr = 500, mxf = 500, mxs = 500)
    time_model_rob <- lmRob(formula = pc_time ~ eyelink_time, control = robust_control)
    # print(time_model_rob$coefficients)
    time_model <- time_model_rob
  } else {
    time_model_lm <- lm(pc_time ~ eyelink_time)
    # print(time_model_lm$coefficients)
    time_model <- time_model_lm
  }
  # we 
  intercept = time_model$coefficients[1] 
  slope = time_model$coefficients[2]
  
  ##### return the model #####
  return_this <- data.frame(eyelink_start_time, pc_start_time, intercept, slope)
  colnames(return_this) <- c("eyelink_start_time", "pc_start_time", "time_intercept", "time_slope")
  rownames(return_this) <- "value"
  return(return_this)
}

get_pc_time <- function(eyelink_pc_time, eyelink_timestamps, eyelink_delay = 0) {
  # compute this here:
  pc_timestamps = eyelink_pc_time$pc_start_time + eyelink_pc_time$time_intercept + 
    eyelink_pc_time$time_slope * ((eyelink_timestamps-eyelink_delay) - eyelink_pc_time$eyelink_start_time)
  # return this here:
  return(pc_timestamps)
}

interpolate_pc_from_eyelink <- function(eyelink_time, pc_time, 
                                        eyelink_timestamps, 
                                        eyelink_delay = 0,
                                        use_robust = TRUE, 
                                        use_names = TRUE) {
  names_eyelink_timestamps <- names(eyelink_timestamps)
  if (length(eyelink_time)>1) {
    # first, interpolate
    interpolation_results <- interpolate_time(eyelink_time = eyelink_time, pc_time = pc_time, 
                                              use_robust = use_robust)
    # second, apply the interpolation function
    pc_timestamps <- get_pc_time(eyelink_pc_time = interpolation_results, 
                                 eyelink_timestamps = eyelink_timestamps, 
                                 eyelink_delay = eyelink_delay)  
  } else {
    interpolation_results <- data.frame(eyelink_start_time = NA, pc_start_time = NA, 
                                        time_intercept = NA, time_slope = NA)
    colnames(interpolation_results) <- c("eyelink_start_time", "pc_start_time", "time_intercept", "time_slope")
    pc_timestamps <- rep(x = NA, times = length(eyelink_timestamps))
  }
  if (use_names) {
    names(pc_timestamps) <- paste(names_eyelink_timestamps, "pc", sep = "_")
  }
  # third, return the interesting values
  return(list(interpolation_results = interpolation_results, 
              pc_timestamps = pc_timestamps))
}


##### test these functions #####
test_these_guys <- FALSE

if (test_these_guys) {
  # timestamps
  (pc_timestamps_real = c(data_row$t_cueOn, data_row$t_saccading, data_row$t_boundary_cross))
  (eyelink_timestamps = c(data_row$EVENT_cueOn, data_row$EVENT_timeSaccade, data_row$EVENT_boundaryCross))
  raw_timestamps = data.frame(pc_timestamps_real, eyelink_timestamps)
  # data points
  eyelink_time = as.numeric(data[ , ,data_row$trial]$t.eye)
  pc_time = as.numeric(data[ , ,data_row$trial]$t)
  raw_times <- data.frame(eyelink_time, pc_time)
  head(raw_times)
  t_pc_start <- raw_times$pc_time[1]
  # differences between data points
  diff_time = pc_time - eyelink_time
  diff_time = diff_time - diff_time[1]
  scatter.smooth(diff_time)
  scatter.smooth(eyelink_time[1:20], pc_time[1:20])
  # plot
  ggplot(data = raw_times, aes(x = eyelink_time, y = pc_time-t_pc_start, color = "blue")) + geom_point() +
    geom_point(data = raw_timestamps, aes(x = eyelink_timestamps, y = pc_timestamps_real-t_pc_start, color = "red")) +
    xlim(min(raw_timestamps$eyelink_timestamps-10), min(raw_timestamps$eyelink_timestamps)+50) +
    geom_smooth(method = "lm", se = FALSE)

  # run the interpolation
  (interpolation_results <- interpolate_time(eyelink_time = eyelink_time, pc_time = pc_time))
  
  # test the interpolated values
  pc_timestamps <- get_pc_time(eyelink_pc_time = interpolation_results, eyelink_timestamps = eyelink_timestamps)  
  pc_timestamps - pc_timestamps_real
  
}



