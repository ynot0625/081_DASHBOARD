
read.files <- function(hours = 96, hy.path) {
  ## find tdump files
  files <- Sys.glob(paste0(hy.path,"working/tdump*"))
  output <- file(paste0(hy.path,'working/Rcombined.txt'), 'w')
  ## read through them all, ignoring 1st 7 lines
  for (i in files){
    input <- readLines(i)
    input <- input[-c(1:7)] # delete header
    writeLines(input, output)
  }
  close(output)
  ## read the combined txt file
  #hy.path <- "C:/RWORK/020_HYSPLIT/hysplit4/"
  traj <- read.table(paste0(hy.path, "working/Rcombined.txt"), header = FALSE)
  traj <- subset(traj, select = -c(V2, V7, V8))
  traj <- reshape::rename(traj, c(V1 = "receptor", V3 = "year", V4 = "month", V5 = "day",
                         V6 = "hour", V9 = "hour.inc", V10 = "lat", V11 = "lon",
                         V12 = "height", V13 = "pressure"))
  ## hysplit uses 2-digit years ...
  year <- traj$year[1]
  if (year < 50) traj$year <- traj$year + 2000 else traj$year <- traj$year + 1900
  traj$date2 <- with(traj, ISOdatetime(year, month, day, hour, min = 0, sec = 0,
                                       tz = "GMT"))
  ## arrival time
  traj$date <- traj$date2 - 3600 * traj$hour.inc
  traj
}

add.met <- function(month, Year, met, bat.file) {
  ## if month is one, need previous year and month = 12
  if (month == 0) {
    month <- 12
    Year <- as.numeric(Year) - 1
  }
  if (month < 10) month <- paste("0", month, sep = "")
  ## add first line
  write.table(paste("echo", met, " >>CONTROL"),
              bat.file, col.names = FALSE,
              row.names = FALSE, quote = FALSE, append = TRUE)
  x <- paste("echo RP", Year, month, ".gbl >>CONTROL", sep = "")
  write.table(x, bat.file, col.names = FALSE,
              row.names = FALSE, quote = FALSE, append = TRUE)
}

procTraj <- function(lat = 51.5, lon = -0.1, year = 2010, name = "london",
                     met = "D:/RWORK/DATA/NCEP/", out = "D:/RWORK/HYSPLIT/data/hysplit_out/",
                     hours = 96, height = 10, hy.path = "C:/hysplit4/") {
  ## hours is the back trajectory time e.g. 96 = 4-day back trajectory
  ## height is start height (m)
  lapply(c("openair", "plyr", "reshape2"), require, character.only = TRUE)
  ## function to run 12 months of trajectories
  ## assumes 96 hour back trajectories, 1 receptor
  setwd(paste0(hy.path, "working/"))
  ## remove existing "tdump" files
  path.files <- paste0(hy.path, "working/")
  bat.file <- paste0(hy.path, "working/test.bat") ## name of BAT file to add to/run
  files <- list.files(path = path.files, pattern = "tdump")
  lapply(files, function(x) file.remove(x))
  start <- paste(year, "-01-01", sep = "")
  end <- paste(year, "-12-31 18:00", sep = "")
  dates <- seq(as.POSIXct(start, "GMT"), as.POSIXct(end, "GMT"), by = "3 hour")
  for (i in 1:length(dates)) {
    year <- format(dates[i], "%y")
    Year <- format(dates[i], "%Y") # long format
    month <- format(dates[i], "%m")
    day <- format(dates[i], "%d")
    hour <- format(dates[i], "%H")
    x <- paste("echo", year, month, day, hour, " >CONTROL")
    write.table(x, bat.file, col.names = FALSE,
                row.names = FALSE, quote = FALSE)
    x <- "echo 1 >>CONTROL"
    write.table(x, bat.file, col.names = FALSE,
                row.names = FALSE, quote = FALSE, append = TRUE)
    x <- paste("echo", lat, lon, height, " >>CONTROL")
    write.table(x, bat.file, col.names = FALSE,
                row.names = FALSE, quote = FALSE, append = TRUE)
    x <- paste("echo ", "-", hours, " >>CONTROL", sep = "")
    write.table(x, bat.file, col.names = FALSE,
                row.names = FALSE, quote = FALSE, append = TRUE)
    x <- "echo 0 >>CONTROL
    echo 10000.0 >>CONTROL
    echo 3 >>CONTROL"
    write.table(x, bat.file, col.names = FALSE,
                row.names = FALSE, quote = FALSE, append = TRUE)
    ## processing always assumes 3 months of met for consistent tdump files
    months <- as.numeric(unique(format(dates[i], "%m")))
    #months <- c(months, months + 1:2)  
    months <- c(months, months + 0:1)
    months <- months - 1 ## to make sure we get the start of the previous year
    months <- months[months <= 12]
    if (length(months) == 2) months <- c(min(months) - 1, months)
    for (i in 1:3)
      add.met(months[i], Year, met, bat.file)
    x <- "echo ./ >>CONTROL"
    write.table(x, bat.file, col.names = FALSE,
                row.names = FALSE, quote = FALSE, append = TRUE)
    x <- paste("echo tdump", year, month, day, hour, " >>CONTROL", sep = "")
    write.table(x, bat.file, col.names = FALSE,
                row.names = FALSE, quote = FALSE, append = TRUE)
    #x <- "C:/RWORK/020_HYSPLIT/hysplit4/exec/hyts_std"
    x <- "C:/hysplit4/exec/hyts_std"
    write.table(x, bat.file, col.names = FALSE,
                row.names = FALSE, quote = FALSE, append = TRUE)
    # run the file
    system(paste0(hy.path, 'working/test.bat'))
  }
  ## combine files and make data frame
  traj <- read.files(hours, hy.path)
  ## write R object to file
  file.name <- paste(out, name, Year, ".RData", sep = "")
  save(traj, file = file.name)
}


run_hysplit <- function (latitude = 51.5, 
          longitude = -0.1, 
          start = NA, 
          end = NA, 
          interval = "3 hour", 
          runtime = -96, 
          start_height = 10, 
          model_height = 10000,
          hysplit_exec = "exec/", 
          hysplit_input, 
          hysplit_output = "hysplit_output/",
          delete = FALSE,
          drop = TRUE,
          site = NA, 
          source = NA, 
          verbose = FALSE) {
  
  require(stringr)
  require(lubridate)
  
  # Parse arguments
  # Expand paths
  hysplit_exec <- path.expand(hysplit_exec)
  hysplit_input <- path.expand(hysplit_input)
  hysplit_output <- path.expand(hysplit_output)
  
  # check these places exist
  if (file.access(hysplit_exec, mode = 0) != 0) 
    stop(paste("File path", hysplit_exec, "does not exist"))
  
  if (file.access(hysplit_input, mode = 0) != 0) 
    stop(paste("File path", hysplit_input, "does not exist"))
  
  if (file.access(hysplit_output, mode = 0) != 0) 
    stop(paste("File path", hysplit_output, "does not exist"))
  
  if (file.access(hysplit_exec, mode = 2) != 0) 
    stop(paste("File path", hysplit_exec, "is not writable"))
  
  # Add final path separator
  hysplit_exec <- str_c(hysplit_exec, .Platform$file.sep)
  hysplit_input <- str_c(hysplit_input, .Platform$file.sep)
  hysplit_output <- str_c(hysplit_output, .Platform$file.sep)
  
  # Selection after testing if directory is empty
  files_old <- list.files(hysplit_output, full.names = TRUE, 
                          pattern = "hysplit_output.txt")
  
  # Delete old files
  if (delete) {
    
    # Delete
    message("Deleting old files...")
    file.remove(files_old)
    
  } else {
    
    # Selection control
    while (length(files_old) > 1) {
      
      # Get input
      input <- readline("There are hysplit output files in the output directory. \nShould these be deleted (y/n)? \n")
      
      # Parse
      input <- str_to_upper(input)
      input <- ifelse(
        input %in% c("YES", "Y", "T", "TRUE", "TR", "TRU"), TRUE, FALSE)
      
      if (input) {
        
        message("Deleting old files...")
        file.remove(files_old)
        break
        
      } else {
        
        break
        
      }
      
    }
    
  }
  
  # Dates
  # Start and end dates
  start <- parse_date_arguments(start, "start")
  end <- parse_date_arguments(end, "end")
  
  # Receptor location and starting height
  coordinates <- str_c(latitude, longitude, start_height, sep = " ")
  
  # Set up where the control file is to be written
  control_file <- file.path(hysplit_exec, "CONTROL")
  
  # Store working directory because this will be changed
  wd <- getwd()
  
  # Create date sequence
  date_sequence <- seq(start, end, interval)
  
  # Apply function which runs the model multiple times
  message(str_c("Running ", length(date_sequence), " HYSPLIT trajectories..."))
  
  # For plyr's progress bar
  progress <- ifelse(verbose, "none", "time")
  
  # Run model
  plyr::l_ply(date_sequence, run_trajectory, 
              hysplit_exec = hysplit_exec, 
              hysplit_input = hysplit_input, 
              hysplit_output = hysplit_output, 
              control_file = control_file, 
              coordinates = coordinates, 
              runtime = runtime, 
              model_height = model_height, 
              verbose = verbose,
              .progress = progress)
  
  # Change working directory back to original after system calls
  setwd(wd)
  
  # Bind output files
  message("Binding HYSPLIT files...")
  
  # Get file list
  file_list <- list.files(hysplit_output, "hysplit_output.txt", full.name = TRUE)
  
  # Load files as in openair, but drop some things usually
  df <- plyr::ldply(file_list, read_hysplit_file, drop = drop)
  
  # Add variables which are not in openair's files
  df$start_height <- start_height
  if (!is.na(site)) df$site <- site
  if (!is.na(source)) df$source <- source
  
  # Delete files
  if (delete) file.remove(file_list)
  
  # Return
  df
  
}

