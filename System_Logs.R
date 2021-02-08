# Custom System Command to capture: Exit Status, Output & Errors
System <- function(Command, Log_name, Append){
  # Create temporary files to hold command outputs
  stderrFile <- tempfile(pattern="R_robust.system_stderr", fileext=as.character(Sys.getpid()))
  stdoutFile <- tempfile(pattern="R_robust.system_stdout", fileext=as.character(Sys.getpid()))
  # Create a list to hold output
  retval <- list()
  # run command capturing error messages
  retval$StartSysTime <- Sys.time()
  retval$exitStatus = system(paste0(Command, " 2> ", shQuote(stderrFile), " > ", shQuote(stdoutFile)))
  retval$EndSysTime <- Sys.time()
  retval$stdout = readLines(stdoutFile)
  retval$stderr = readLines(stderrFile)
  # remove temporary files
  unlink(c(stdoutFile, stderrFile))
  
  # Write to Log File
  write(c(format(retval$StartSysTime, "%a %b %d %X %Y"), "Command:"), file = Log_name, sep = "\n\n", append = Append)
  write(Command, file = Log_name, sep = "\n\n", append = TRUE)
  write(c("\n", "Run_Time:"), file = Log_name, sep = "\n\n", append = TRUE)
  write(paste0(difftime(retval$EndSysTime, retval$StartSysTime, units = "secs")[[1]], ' Secs'), file = Log_name, sep = "\n\n", append = TRUE)
  write(c("\n", "Exit_Status:"), file = Log_name, sep = "\n\n", append = TRUE)
  write(retval$exitStatus, file = Log_name, sep = "\n\n", append = TRUE)
  write(c("\n", "Std_Out:"), file = Log_name, sep = "\n\n", append = TRUE)
  write(retval$stdout, file = Log_name, sep = "\n\n", append = TRUE)
  write(c("\n", "Std_Err:"), file = Log_name, sep = "\n\n",append = TRUE)
  write(retval$stderr, file = Log_name, sep = "\n\n", append = TRUE)
  return(retval)
}


# Custom Function to retrieve Log File Information
Log_data <- function(Sample_regex, Log_name){
  Command_Stats <- cbind(data.frame(gsub(Sample_regex, "\\1", 
                                         system(paste0("awk 'f{print;f=0} /Command:/{f=1}' ", Log_name), intern = TRUE))),
                         data.frame(system(paste0("awk 'f{print;f=0} /Exit_Status/{f=1}' ", Log_name), intern = TRUE)),
                         data.frame(system(paste0("awk 'f{print;f=0} /Run_Time/{f=1}' ", Log_name, " | sed 's/ Secs//g'"), intern = TRUE)))
  
  colnames(Command_Stats) <- c("Sample", "Exit.Status", "Run.Time")
  Command_Stats$Exit.Status <- as.numeric(as.character(Command_Stats$Exit.Status))
  Command_Stats$Run.Time <- as.numeric(as.character(Command_Stats$Run.Time))
  return(Command_Stats)
}