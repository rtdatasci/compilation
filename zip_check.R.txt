#' zip_check
#' zip_check function checks if the contents of zip folders are unpacked 
#' 
#' 
#'@param studypath for example
#' studypath <- "~/zip"
#'@export
#'


library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(readxl)


zip_check <- function(studypath){
  
 
  ### check if all the zip contents are mapped
  
  ## breakdown into study assay rawdata for crawl efficiency: 
  study_dirs <- list.dirs(studypath, full.names = TRUE, recursive = F)
  study_dirs <- as.data.frame(study_dirs)
  study_dirs <- lapply(study_dirs, function(x){paste0(x, "/rawdata")})
  study_dirs <- study_dirs$study_dirs[!grepl("linical-unofficial|linical_unofficial|Docs|docs|doc|",study_dirs$study_dirs)]  # character
  
  
  # List all files as total_files and zipped folders
  total_files <- NULL
  total_files <- as_tibble(total_files)
  for (i in study_dirs){
    total_file <- list.files(file.path(i), recursive=TRUE, full.names=TRUE, ignore.case = TRUE)
    total_files <- append(total_files, total_file)
  }
  
  
  total_files <- unlist(total_files)
  zip_folders <- total_files[grepl("zip$|ZIP$|Zip$",total_files)]
  zip_folders <- zip_folders[!grepl("error.zip",zip_folders)]       # to exclude error
  total_files <- basename(total_files)                               # goto file level
  
  if(length(zip_folders)!=0){
    
    # all zipped folders and their contents
    all_unzipped_files <- NULL
    all_unzipped_files <- as_tibble(all_unzipped_files)
    
    for(i in seq_along(zip_folders)){ 
      tryCatch({unzipped_files <- zip_folders[i] %>% unzip(list=T) %>% select(Name) },
               error=function(e) {
                 file.name <- paste("zip_unable_to_open", ".txt", sep="")
                 sink(file.name, append=T, split=T)
                 cat(zip_folders[i], "\n")
                 while (sink.number()>0) sink()
               }
      )
      if(exists("unzipped_files")){
        unzipped_files_df <- cbind(zip_folders[i],unzipped_files)
        all_unzipped_files <- bind_rows(all_unzipped_files, unzipped_files_df) %>% distinct()
      }
    }
    
    
    # Cleanup
    all_unzipped_files <- all_unzipped_files[!grepl("\\__MACOSX", all_unzipped_files$Name),]  # don't list hidden folder "__MACOSX" because this is not detected in total_files
    all_unzipped_files <- all_unzipped_files[!grepl("DS_Store", all_unzipped_files$Name),]    # .DS_Store is attribute files that is not detected in total_files
    all_unzipped_files <- all_unzipped_files[!grepl("\\/$", all_unzipped_files$Name),]        # remove path that ends at folder level
    
    filenames <-  basename(all_unzipped_files$Name)
    all_unzipped_files <- cbind(all_unzipped_files, filenames)
    
    
   
    all_unzipped_files   <- all_unzipped_files %>% mutate_all(tolower)
    total_files          <- tolower(total_files)
    
    
    
   
    all_unzipped_files$filenames   <- gsub("(.*)\\..*$","\\1",all_unzipped_files$filenames)
    total_files                    <- gsub("(.*)\\..*$","\\1",total_files)
    
    
  }
  
  ## check if all the zipped contents are unpacked
  if(length(zip_folders) ==0){
    output <- cbind(NA, "No zip folders found")
    colnames(output) <- c("Folder_path","Zip_status")
  }else{
    if(length(all_unzipped_files)==0){
      output <- cbind(NA, "Zip folder unable to open -- see file zip_unable_to_open")
      colnames(output) <- c("Folder_path","Zip_status")
    }else{
      if(all(all_unzipped_files$filenames %in% total_files)){
        unpacked_zipfolders <- all_unzipped_files$`zip_folders[i]`
        uniq_unpacked_zipfolders <- unique(unpacked_zipfolders)
        
        output <- cbind(uniq_unpacked_zipfolders, "All zip folders are unpacked")
      }else{
        not_unpacked <- setdiff(all_unzipped_files$filenames, total_files)
        
          missed_zip_folders <- all_unzipped_files$`zip_folders[i]`[all_unzipped_files$filenames %in% not_unpacked]
          uniq_missed_zip_folders <- unique(missed_zip_folders)
          
          output <- cbind(uniq_missed_zip_folders, "not_unpacked")
          colnames(output) <- c("Folder_path","Zip_status")
        }
      }
    }
  }
  
  output_df <- as.data.frame(output)
  colnames(output_df) <- c("File_Path", "Zip_status")
  as_tibble(output_df)
  #print(output_df)
  
}
  
  ###########################################
  ## test
  # studypath <- "~/studypath"
  # zip_check(studypath)
