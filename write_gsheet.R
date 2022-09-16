
## packages
library(dplyr)
library(stringr)
library(googlesheets4)



## setup
working_dir <- "~/"
setwd(working_dir) 
run_date <- Sys.Date()

## tracker file
tracker_path <- "https://docs.google.com/spreadsheets/d/test"
tracker_studies <- read_sheet(tracker_path, 'Issues') %>% 
  select(Study, Curator,Sample_Num,duplicated_SAMPLE_ID, Data_Status, Priority) 



## s2f files from /reports
s2f_reports_dir <- "~/reports"
s2f_filepath <- list.dirs(s2f_reports_dir) 

# cleanup unnecessary folders
s2f_df <- as.data.frame(s2f_filepath) 
s2f_df <- s2f_df %>% 
  filter(!grepl("old|archive|temp",s2f_filepath)) 



## Summarize studies:
report <- as.data.frame(NULL)
for (i in 1:nrow(s2f_df)){
  
  # check for deduplicated files
  test_case <- any(grepl("deduplicated", list.files(s2f_df$s2f_filepath[i]) )==TRUE)
    
  Study = s2f_df$Study[i] %>% tolower()
  status = ifelse(test_case==TRUE, "deduplicated","not done")
  combine <- cbind(Study, status)
  report <- rbind2(report, combine) %>% arrange(status,Study)
}




# join
report_df <- report %>% left_join(tracker_studies, by=c("Study"))%>% 
  mutate(deduplication_needed=ifelse(is.na(deduplication_needed),"NO",deduplication_needed)) %>% 
  mutate(Priority=ifelse(Priority=="high","High",Priority)) %>% 
  select("Study","Curator","Sample_Num","duplicated_SAMPLE_ID","Data_Status","Priority","deduplication_needed","status") %>% 
  arrange(Priority,deduplication_needed)


# add count summary (to display count summary in the dashboard) 
report_df <- report_df %>% 
  mutate(completed=ifelse(status=="deduplicated",1,0),
         crawl_date=run_date)   # add crawl date


## save output
output_path <- paste0(getwd(), "/", "s2f_deduplication_status_", run_date,".csv")
# write_csv(report_df, output_path)




## updatetracker sheet=deduplication:
tracker_address <- "https://docs.google.com/spreadsheets/d/test"
#tracker <- read_sheet(tracker_address, sheet = "deduplication")  # can comment out to check the table but not needed

tracker_updated <- report_df 

sheet_write(tracker_updated,
            ss = tracker_address,
            sheet = "new")


# 
## save to gsheet for datastudio dashboard (https://datastudio.google.com/reporting/test)
# ss <- gs4_create("deduplication_status",sheets = "status") # create empty table; only need to run the first time, or it will create additional new gsheets
#sheet_write(report_df, ss = ss,sheet = "status")           # write data in the table


#############################################################

