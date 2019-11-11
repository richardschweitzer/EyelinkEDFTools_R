# Convert an edf file (file) in a folder (file_path) to ASCII 
# using edf2asc on Ubuntu
# needs wine!
# by Richard, 08/2015

# EDF2ASC: EyeLink EDF file -> ASCII (text) file translator
# EDF2ASC version 3.1 Win32 Feb 16 2010 
# (c)1995-2009 by SR Research, last modified Feb 16 2010
# 
# USAGE: edf2asc.exe  [options] <input .edf file>
#   OPTIONS: -p <path> writes output with same name to <path> directory
# -p *.<ext> writes output of same name with new extension
# -d <filename> creates log data file
# -t use only tabs as delimiters
# -c check consistency
# -z disable check consistency and fix the errors
# -v verbose - reports warning messages. 
# -y overwrite asc file if exists.
# If no output file name, will match wildcards on input file name,
# and will write output files to new path or will overwrite old files.
# DATA OPTIONS: -sp  outputs sample raw pupil position if present
# -sh  outputs sample HREF angle data if present
# -sg  outputs sample GAZE data if present (default)
# -l or -nr   outputs left-eye data only if binocular data file
# -r or -nl   outputs right-eye data only if binocular data file
# -res         outputs resolution data if present
# -vel (-fvel) outputs sample velocity (-fvel matches EDFVIEW numbers)
# -s or -ne   outputs sample data only
# -e or -ns   outputs event data only
# -miss <value>     replaces missing (x,y) in samples with <value>
#   -setres <xr> <yr> uses a fixed <xr>,<yr> resolution always
# -defres <xr> <yr> uses a default <xr>,<yr> resolution if none in file
# -nv         hide viewer commands
# -nst        blocks output of start events
# -nmsg       blocks message event output
# -neye       outputs only non-eye events (for sample-only files)
# Use  -neye     to get samples labeled with non-eye events only
# Use  -neye -ns to get non-eye events only
# -nflags to disable flags data for EyeLink II or EyeLink1000 data files.
# -hpos  output head marker positions
# -avg  output average data 
# -ftime output float time 
# -input output input values in samples.
# -failsafe runs in failsafe mode and recover partial edf file                                                                                            
# -ntarget to disable target data for EyeLink1000 Remote data files.                                                                                      
# 
# 
# Scene camera parameters                                                                                                                                 
# -gazemap  outputs gaze data in avi coordinates. -g  can also be used instead
# -insertframe  inserts frame number. -i can also be used instead
# -scenecam  same as using -gazemap -insertframe together

convert_edf_to_asc <- function(file_path, edf_file, 
                               exe_path = '~/Dropbox/PROMOTION WORKING FOLDER/General/EDF2ASC/edf2asc.exe', 
                               options = '-c -y -t -failsafe') {
  
  # aux function: only for the linux command line
  space_convert <- function(s) { 
    # convert a space to \space (for linux terminal). R represents "\" using "\\"
    gsub(pattern=" ",replacement="\\\\ ", x=s)
  }
  
  # create file path
  edf_name <- file.path(file_path, edf_file)
  
  # make command
  command <- paste('wine', space_convert(exe_path), # initiate wine emulator and find edf2asc.exe
                   options,    # default: '-c -y -t -failsafe'
                   space_convert(edf_name) ) # where is the edf file?
  print(command)
  system(command)
  # return asc name
  asc_name <- gsub(pattern = ".edf", replacement = ".asc", 
                   x = edf_name)
  return(asc_name)
}