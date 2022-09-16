#' file_cutter
#' Adds functionality to scan files and break up files that are too large. Only files that haven't
#' already been added to .gitignore will be added
#' 
#' file_cutter breaks large csv files (> 100MB) into smaller chunks (50 MB) and 
#'    writes each chunk as csv files with the number of chunk as "_chunk" 
#'    added to their filename.
#' 
#'@param filepath for example
#' filepath <- "~/file.csv"
#'@export
file_cutter_auto <- function(study){
  gitignore_file <- readLines("~/.gitignore")
  all_files <- list.files(paste0("~/", tolower(study)), full.names = TRUE)

  for(filepath in all_files){
    size_of_file <- file.size(filepath)
    if(size_of_file > 100000000){
      tryCatch({file <- read_csv(filepath)},
               error=function(e) {cat("Bad file pattern:\n",
                                      file, "\n")}
      )
      
      name_of_file <- filepath %>% basename() %>% str_trim()
      name_of_file_no_ext <- name_of_file %>% str_remove(".csv")

      # size_of_file <- file.size(filepath) # file size in bytes
      no_of_chunks <- ceiling(size_of_file / 50000000)
      chunks <- ceiling(1:nrow(file) / nrow(file) * no_of_chunks)
      file_chunks <- split(file, chunks)
      map2_dfr(file_chunks, paste0(name_of_file_no_ext, "_", names(file_chunks),
                                   ".csv"), write_csv)
      if(all(!str_detect(gitignore_file, name_of_file))){
        cat(paste0("\n", "*", str_trim(name_of_file)), file = "~/.gitignore",
            append = TRUE)
      }
    }
  }
}
  