# git commit QC passed tables to /output 

push_to_github <- function(study) {
  
  # setup
  updateDate <- Sys.Date()
  study <- tolower(study)
  
  # QC passed files
  qc_passed1 <- paste0("~/",study,"file1.csv")
  qc_passed2 <- paste0("~/",study,"/",study,"file2.csv")
  
  # Destination /output
  qc_passed_output <- "~/output"

  
  # copy QC passed files to /output
  file.copy(from = file.path(qc_passed1), to = file.path(qc_passed_output), overwrite = TRUE)
  file.copy(from = file.path(qc_passed2), to = file.path(qc_passed_output), overwrite = TRUE)
  
  commit_msg <- paste0("updated_",study,"_", updateDate)
  
  
  #command line push to github
  system("cd ~/output &&
        git add -u &&                    # add all modified files, alternatively git add --all
        git commit -m commit_msg  
        git push origin master
        ")
  
}


## test
# push_to_github(study="abc123")

