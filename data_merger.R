
run_date <- Sys.Date()

library(tidyverse)
library(googlesheets4)
library(readxl)
library(reshape2)

# data from local and gsheet
data_path <- " "
data<- read_excel(paste(data_path, "/excelname.xlsx", sep=""), col_names = TRUE, col_types = "text")

variables_path <- "https://docs.google.com/spreadsheets/0"
variables <- read_sheetvariables_path, sheet = "sheetname", col_types = 'c')
                                      

# Relocate a col
data <- data %>% 
  relocate(SmokingHistory, .after = `Cigarette Status`)



# Assign
list_df <- as_tibble(list) %>% 
  mutate(enumerate = 1:31) %>% 
  mutate_all(as.character) %>% 
  mutate(Primary_Diag = case_when(enumerate %in% c("1") ~ "IPF",
                                  TRUE ~ value)) 
                              
# check
if(length(uniquelist_df $Subject_ID)) == nrow(data)){
  cat("one subject xxxxxxxxxxx")
}


# lookups
lookup <- data %>%
  replace(is.na(.), "FALSE") %>% # Replace NA by FALSE
  group_by(Subjects) %>% 
  summarise_all(funs(min(as.character(.))))



# convert to wide format so that one subject per line table can be created -- multiple values for certain variables noted
data<- data%>%
  pivot_wider(
    names_from = RxRx, 
    values_from = c(Dose, 
                    start_time, 
                    stop_time)) 

#########################
# merge rows (sparse matrix with NULL to be coalesced)

# maximum number of rows per subject 
data %>% count(Subject_ID) %>% slice(which.max(n))

coalesce_by_column <- function(df) {
  return(coalesce(df[1],df[2],df[3],df[4],df[5],df[6]))
}

data <- data %>%
  mutate_all(na_if,"NULL") %>% 
  mutate_all(na_if, "NA") %>% 
  group_by(Subject_ID) %>%
  summarise_all(coalesce_by_column) 

# sanity check
data %>% count(Subject_ID) %>% slice(which.max(n))


# list concomittant drugs in single cell
new_frame <- data%>% 
  rowwise() %>% 
  mutate_if(is.list, ~paste(unlist(.), collapse = '|')) 


# remove columns with "_Dose" and "ongoing" 
data <- data%>% 
  select(-contains(c("Dose","Ongoing")))




## Deciding enrollment days as a window of -30 to 30 days  (60 days) and selecting the one closest to 0
data <- data %>% 
  select("Subject_ID",
         "Testing_Date",                    # to be able to select bmi at enrollment date
         "bmi") %>% 
  filter(!is.na(bmi)) %>% 
  filter(`Testing_Date` <= 30) %>% 
  filter(`Testing_Date` >= -30) %>% 
  group_by(Subject_ID) %>% 
  slice(which.min(abs(Testing_Date)))   # value closest to 0 = enrollment date









# max values of duplicate assessments (or multiples (more than duplicates))
slice_max <- data %>% 
  mutate(new = paste0(Subject_ID,"/",Test_Name,"/",Testing_Date)) %>% 
  filter(new %in% check_multiples_dup$new) %>% 
  group_by(new) %>% 
  slice(which.max(Result)) %>% 
  mutate(max_result = paste0(Subject_ID,"/",Test_Name,"/",Testing_Date,"/",Result))

slice_min <- data %>% 
  mutate(new = paste0(Subject_ID,"/",Test_Name,"/",Testing_Date)) %>% 
  filter(new %in% check_multiples_dup$new) %>% 
  mutate(min_result = paste0(Subject_ID,"/",Test_Name,"/",Testing_Date,"/",Result)) %>% 
  filter(!min_result %in% slice_max$max_result)



# Assign flag "FLAG" to single result values and max values if multiples are present
data_flag <- data %>% 
    mutate(combo = paste0(Subject_ID,"/",Test_Name,"/",Testing_Date,"/",Result)) %>%
    mutate(FLAG = ifelse(combo %in% slice_min$min_result, NA, "Y")) %>%
    select(-combo)  # remove helper column
  





## Additional flag:
# First time when % predicted YYY has an absolute decline by 5% within a one year period; Flag: One_year_5percent
# First time when % predicted YYY has an absolute decline by 10% within a two year period; Flag: Two_year_10percent

df <- as.data.frame(NA)
for (i in data$subjects){
  subject = i 
  df <- data %>% filter(Subject_ID %in% subject)%>% filter(Test_Name=="testname")
  # add baseline time
  df_mod <- df %>% filter(ABFL=="Y") %>% select(Subject_ID, Testing_Date) %>% distinct()
  df <- df %>% left_join(df_mod, by=c("Subject_ID")) %>%
    dplyr::rename(Testing_Date=Testing_Date.x, BASE_Date=Testing_Date.y)

  # compare decrease by 5% if there are multiple values and if a baseline date is available
  if(nrow(df)>1 & "Y" %in% df$FLAG){
      df_temp <- df %>% 
        mutate(percent_change = ((Result -  BASE)/BASE)) %>%
        mutate(days_change = Testing_Date - BASE_Date ) %>% 
        
        # filter out results that fall out of baseline/enrollment period (-30 to 30 days)
        filter(! Testing_Date <= -30) %>% 
        
        # UPDATED to reflect decline (previously opposite was calculated):
        # mutate(Flag_oneyear_5percent = ifelse(percent_change >= 0.05 & days_change <=365, "Y", NA))%>% 
        # mutate(Flag_twoyear_10percent = ifelse(percent_change >= 0.10 & days_change <=730, "Y", NA))
        mutate(Flag_oneyear_5percent = ifelse(percent_change <= -0.05 & days_change <=365, "Y", NA))%>% 
        mutate(Flag_twoyear_10percent = ifelse(percent_change <= -0.10 & days_change <=730, "Y", NA))
        
      
      #cleanup flag for dates older than baseline date
      df_temp <- df_temp %>% 
        mutate(Flag_oneyear_5percent = ifelse(Testing_Date < BASE_Date,NA,Flag_oneyear_5percent))%>% 
        mutate(Flag_twoyear_10percent = ifelse(Testing_Date < BASE_Date,NA,Flag_twoyear_10percent))
      
      #cleanup flag if multiples; keep the first flag to reach 5% or 10% change
      df1 <- df_temp %>%
        arrange(Testing_Date) %>%
        select(-Flag_twoyear_10percent) %>%
        filter(!is.na(Flag_oneyear_5percent)) %>%
        slice(1)  # keep the first occurrence of flag: Y
        
      df2 <- df_temp %>%
        arrange(Testing_Date) %>%
        select(-Flag_oneyear_5percent) %>%
        filter(!is.na(Flag_twoyear_10percent)) %>%
        slice(1)   # keep the first occurrence of flag: Y
        
      
      df1df2 <- df1 %>% bind_rows(df2) %>% 
        mutate(Flag_oneyear_5percent=as.character(Flag_oneyear_5percent)) %>%
        mutate(Flag_twoyear_10percent=as.character(Flag_twoyear_10percent))

      # combine
      df_all <- bind_rows(df, df1df2) %>% 
        select("Subject_ID","Test_Name" ,"Result","Testing_Date","FLAG","percent_change" ,"days_change" , "Flag_oneyear_5percent" ,"Flag_twoyear_10percent") %>% 
        filter(!is.na(Subject_ID)) %>% 
        distinct() 
      
     }else{
    print("does not contain multiple values to compare or does not have baseline")
  }
}


############################################################################
## ADD YYY slopes to subject level data


data <- data %>% 
  filter(Test_Name=="YYY") %>% 
  #filter(!is.na(BASE)) %>%  # commented out to take all samples with or without BASE
  select(Subject_ID, Result, Testing_Date, BASE, CHG) %>% 
  
  ## Parse Year1 and Year2 FVC values: values with minimum difference from 365 and 730 
  # YR1: -30 to 395;  YR2: -30: 760 (similar to baseline a -30:+30 window used) assigned a temporary flag "T" for true
  mutate(YR1 =  ifelse((Testing_Date %in% -30:395), "T", NA)) %>% 
  mutate(YR2 =  ifelse((Testing_Date %in% -30:760), "T", NA)) %>% 
  
  # remove redundant cols
  select(-BASE, -CHG)
  


## Testing slope calculations:
## Calculating slope using lm in r. Example with two points -- can be extended to multiple points:
# x <- c(-7,192)
# y <- c(4.33,4.55)  
# mod <- lm(y~x)
# m1= mod$coefficients[2][["x"]] # slope


# Cannot calculate slopes for subjects that only has a single data point
lookup <- fsys %>% select(Subject_ID, Result, Testing_Date) %>% distinct()
lookup_singles <- lookup[!(duplicated(lookup$Subject_ID)|duplicated(lookup$Subject_ID, fromLast=TRUE)),]
data <- ata %>% filter(!Subject_ID %in% lookup_singles$Subject_ID)


df_final <- as.data.frame(NULL)
for (subject in unique(data$Subject_ID)){
  
 
  
  ## YEAR 1
  df <- data %>% filter(Subject_ID == subject) %>% filter(YR1=="T")
  if (nrow(df) != 0){
    x <- df$Testing_Date
    y <- df$Result
    mod <- lm(y~x)
    slope <- mod$coefficients[2][["x"]] * 365 %>% as_tibble()# to convert days to year
   
    df_temp <- data.frame(Subject_ID = subject, Slope_YR1 = slope$value)
    df_final <- df_final %>% bind_rows(df_temp)
    
  }
  
  
  ## YEAR 2
  df <- data %>% filter(Subject_ID == subject) %>% filter(YR2=="T")
  if (nrow(df) != 0){
    x <- df$Testing_Date
    y <- df$Result
    mod <- lm(y~x)
    slope <- mod$coefficients[2][["x"]] * 365 %>% as_tibble()# to convert days to year

    df_temp <- data.frame(Subject_ID = subject, Slope_YR2 = slope$value)
    df_final <- df_final %>% bind_rows(df_temp)

  }
}

# cleanup  (some subjects only have one data point because YR1 and YR2 data points not available eg. 02G0006)
df_slopes <- df_final %>% left_join(df_final, by="Subject_ID") %>% 
  mutate(Slope_YR1 =  coalesce(Slope_YR1.x,Slope_YR1.y),
         Slope_YR2 =  coalesce(Slope_YR2.x,Slope_YR2.y)) %>% 
  select(Subject_ID, Slope_YR1, Slope_YR2) %>% 
  filter(!is.na(Slope_YR1)) %>% 
  filter(!is.na(Slope_YR2)) %>% 
  distinct()


# save output
write_csv(df_slopes paste0("~/test_", run_date,".csv"), na="NA", col_names=T)

  





# Mininum time value
min_time <-  gather(data, PARAM, AVAL, 
                `Time to YYY(%) >= 10%`:`Time to transplant`, 
                factor_key=TRUE) %>% 
  group_by(Subject_ID) %>% 
  slice(which.min(AVAL)) %>% 
  select(-PARAM, `Time to FYY(%) >= 10%, death, or transplant`= AVAL)
  




# Updating CNSR for composite events (previously unpopulated or populated as default NA)

#(1.) if all components of the composite are censored then the composite should also be censored (CNSR==1), and 
#(2.) if one of the components happens, and the other components are censored, then the composite is not censored (CNSR==0), and 
#(3.) if we don't have data for one of the components (e.g. the patient does not have FVC data so we have no way of knowing if they have an event or not) then CNSR is NA for that individual component and the entire composite. 



# initiate empty data frame
df_composite <- as.data.frame(NULL)

for(subject in unique(data$Subject_ID)){
  
    # Composite event #1: Time to FVC(%) >= 10%, death, or transplant  
    uniq_CNSR <- PFF_tte_CNSR_updated %>% 
      filter(Subject_ID == subject) %>% 
      filter(!str_detect(PARAM, "espiratory|\\,")) %>% 
      pull(CNSR) %>% unique()
  

    df1 <- PFF_tte_CNSR_updated %>% 
      filter(Subject_ID == subject) %>% 
      filter(PARAM == "Time to FVC(%) >= 10%, death, or transplant") %>% 
      mutate(CNSR = ifelse(NA %in% uniq_CNSR, NA,
                           ifelse(length(uniq_CNSR) == 1 & "1" %in% uniq_CNSR, "1", "0")))
        

    # Composite event #2: Time to FVC(%) >= 10%, death, or respiratory hospitalization
    uniq_CNSR <- data %>% 
      filter(Subject_ID == subject) %>% 
      filter(!str_detect(PARAM, "ransplant|\\,")) %>% 
      pull(CNSR) %>% unique()

    df2 <- data %>% 
      filter(Subject_ID == subject) %>% 
      filter(PARAM == "Time to FVC(%) >= 10%, death, or respiratory hospitalization") %>% 
      mutate(CNSR = ifelse(NA %in% uniq_CNSR, NA,
                           ifelse(length(uniq_CNSR) == 1 & "1" %in% uniq_CNSR, "1", "0")))
    
    
    df_composite <- df_composite %>% bind_rows(df1, df2)            
             
}


