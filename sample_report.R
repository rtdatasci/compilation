#' sample_report
#' 
#' maps SampleIDs .xlsx files and text files within studypath folder,
#'     usually located within  folders.  It will also catch non-text
#'     files with SampleID in the file name.
#'     
#' @param studypath full filepath for the study, for example:
#'     studypath <- "~/studypath"
#'
#' @export
sample_report <- function(studypath){
  report <- NULL
  report <- as_tibble(report)

  # full list of files in / with full paths
  raw <- paste(studypath, "/rawdata", sep="")
  files <- raw %>% dir(full.names=T, recursive=T)
  

  # map all .txt files within fmi_obd folders
  report_txt <- txt_grab_function(files)  
  # map all .xlsx files within fmi_obd folders
  report_xlsx <- xlsx_grab_function(files) 
  
  report <- rbind2(report_txt, report_xlsx) %>% distinct()
  
  study <- basename(studypath)
  
  report_name <- paste0(getwd(), "/", study, "_sample_to_file.csv")
  
  if (!file.exists(report_name)){
    write_csv(report, report_name, append=T, col_names=T)
  } else {
    write_csv(report, report_name, append=T, col_names=F)
  }
  
  print("Report on files from ~/studypath folders was generated in your working directory")
}
###############################



