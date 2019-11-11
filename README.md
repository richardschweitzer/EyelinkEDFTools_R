# EyelinkEDFTools_R

This repository holds a few R functions used for the pre-processing of SR Research Eyelink EDF files. 

## Source some functions
In order to use those functions, just source them.
```R
source("~/your_folder/convert_edf_to_asc.R")
source("~/your_folder/extract_trial_from_edf.R")
source("~/your_folder/scan_asc_trial_bounds.R")
source("~/your_folder/interpolate_time.R")
```

## Conversion from EDF to ASC
Some edf2asc function must be installed on your system. The function in this repository calls *wine* to run the *edf2asc.exe* to convert a specified EDF file to ASC.
```R
saccade_edf_file <- convert_edf_to_asc(file_path = path_to_edf, edf_file = saccade_edf_filename, 
                                       options = '-y -t -failsafe')
```

## Scanning for trial boundaries
You should now have a ASC files that you can read in as a text file.
```R
saccade_asc_file <- readLines(con=saccade_edf_file)
```
Provided that you have sent messages to your Eyelink, such as "Trial_Start_1" and "Trial_End_1", to demarcate trials, you can now parse the text file for those lines that contain trial data. The function can assert consistency (*assert_consistency=TRUE*), assuming that the trial counter starts at 1. If that is not the case, you may specify a *trialCntr_addon*, i.e., the counter of the first trial in the EDF file.
```R
saccade_trial_bounds <- scan_asc_trial_bounds(saccade_asc_file, 
                                              trialCntr_addon = 0, 
                                              assert_consistency = TRUE)
head(saccade_trial_bounds)
paste("Found", nrow(saccade_trial_bounds), "consistent trial boundaries in EDF.")
```

## Extract data of one given trial
You can now extract properly formatted data of a specified trial, let's say trial 1. To do that, you specify the boundaries of the trial from the previous step and supply the matching text file. 
If you tracked monocularly, always specify *dom_eye = 0* and *normal_msg_length = 5*. If you tracked binocularly, you may choose *dom_eye = 0* or *1*, but specify *normal_msg_length = 8*, so that the eyelink message and data rows can be properly recognized. Also be sure to have a matching *msg_separator* (default: *"\t"*), so that messages can be parsed. If you have sent custom messages during your trials, you may specify them in *msg_of_interest* and their timestamps will be returned if they were found (if not, you'll receive a warning). Finally, *col_names* are the column names of the returned data.frame.
```R
trial_edf <- extract_trial_from_edf_nosearch(trial_i_start = saccade_trial_bounds$trial_i_starts[1], 
                                             trial_i_end = saccade_trial_bounds$trial_i_ends[1],
                                             asc_file = saccade_asc_file, 
                                             dom_eye = 0, 
                                             normal_msg_length = 5,
                                             used_mode_1440 = FALSE, 
                                             msg_of_interest = c("EVENT_cueOn", "EVENT_timeSaccade", 
                                                                 "EVENT_timeMoveStarted", "EVENT_timeMoveFinished", 
                                                                 "EVENT_boundaryCross"),
                                             col_names = c("sac_t_raw", "sac_x_raw", "sac_y_raw", "sac_pupil") )
```

## Time interpolation (Eyelink -> Experiment PC time)
Finally, if you have sampled online data during your experiment and thus have both the retrieval timestamps of the eyelink and the experiment PC (below the vectors *t.eye* and *t*, respectively), then you can remap eyelink timestamps (such as those in *trial_edf$msg* or *trial_edf$eye_raw$sac_t_raw* below) to the time on your experiment PC. 
```R
interpolated_pc_timestamps <- interpolate_pc_from_eyelink(eyelink_time = t.eye, 
                                                          pc_time = t, 
                                                          eyelink_timestamps = trial_edf$msg )
interpolated_pc_time <- interpolate_pc_from_eyelink(eyelink_time = t.eye, 
                                                    pc_time = t, 
                                                    eyelink_timestamps = trial_edf$eye_raw$sac_t_raw, 
                                                    use_names = FALSE )
```

