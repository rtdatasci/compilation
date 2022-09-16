# check studies that needed modification


library(tidyverse)
library(googlesheets4)
googlesheets4::gs4_auth(email = "your_email.com", use_oob = TRUE)



tracker_path <- "https://docs.google.com/spreadsheets/d/example"
tracker<-read_sheet(tracker_path,sheet="Tables", col_types = 'c') 
tracker_high_priority <- tracker %>% 
  select(Study_ID, JIRA_Ticket, Priority)%>% 
  filter(Priority %in% c("High","high","HIGH"))


## table exists (yes/no)
for (i in tracker_high_priority$Study_ID) {
  study = toupper(i)
  
  if(length(path_all)==0){
    cat(study, " : S2F table not found" )
    cat("\n")
    
    ## for the record
    status <- "~/status.txt"
    lines = paste0(study, " : table not found")
    write_lines(lines, status, append = T)
    
    # skip to next study
    next
  }

 
  if(length(dedup_file)==0){
    cat(study, " : Dedup table not found" )
    cat("\n")
    
    ## for the record 
    s2f_status <- "~/s2f_status.txt"
    lines = paste0(study, " : Deduplicated table not found")
    write_lines(lines, s2f_status, append = T)

    # skip to next study
    next
  }
}




