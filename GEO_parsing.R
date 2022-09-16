######## GEO curation ######## 


# Load packages
library(dplyr)
library(tidyverse)
library(readxl)
library(stringr)
library(stringi)


# Setup filepaths
setwd("~/GEO_curation")
geo_filepath <- "~/geo.csv"


# Read raw file
geo_file <- read_csv(geo_filepath, col_types = cols(.default = col_character()))

####### RAWDATA ####### 
# species
unique(geo_file$sample_organism_ch1)

# characteristics
unique(geo_file$sample_characteristics_ch1)  


length(unique(geo_file$X1)) 



##########################

####### SAMPLE CHARACTERISITCS CH1 ####### 

# select only ch1 column for parsing name value pairs from this column
sample_characterisics_table <- geo_file %>% select(sample_characteristics_ch1)

sample_characterisics_table_uniq <- geo_file %>% select(X1,sample_characteristics_ch1) %>% 
  rename(unique_row=X1)  # renamed so that wont confuse with self populated X1, X2... columns in later section


#######

## STRATEGY: split by comma
# In many cases, if the comma separated values belong to the same leading column header, keep them together
# Identify such cases by utilizing ":" that seprates name and value
# Special cases including ":" and "::" to represent ratio and double colons (eg. 60:40, Thy1::ChR2) will be handled separately 

#######

## split by comma and concatenate values that belongs to same key and add comma separator between those values
test_table <- sample_characterisics_table
test_df <- data.frame(test_table,do.call(rbind,str_split(test_table$sample_characteristics_ch1,",")))

# X cols
colnames(test_df)
max_X <- length(colnames(test_df))-1  # subtract 1 to create length of X by removing first non-X columna


for(i in 1:ncol(test_df)) {       # for-loop over columns
  test_df[,i] <- ifelse(grepl(":",test_df[,i]),
                        paste0(",",test_df[,i]),
                        paste0("COMMA",test_df[,i]))
}


# merge new cols
test_df$merged<- ""
i <- 1 
while(i <=max_X) {  
  j=i+1
  test_df$merged <- paste0(test_df$merged,test_df[,j])
  i=i+1
}

# remove leading ,
test_df$merged <- str_sub(test_df$merged,start=2) 
test_df$sample_characteristics_ch1 <- str_sub(test_df$sample_characteristics_ch1,start=2) 

test_df <- test_df %>% select(sample_characteristics_ch1,merged)



test_df1 <- data.frame(test_df,do.call(rbind,str_split(test_df$merged,","))) %>% 
  select(-merged)





## add sample_characterisics_table_uniq unique row values
test_df1_uniq <- sample_characterisics_table_uniq %>% cbind(test_df1)

# sanity check and cleanup
all(test_df1_uniq[,2]==test_df1_uniq[,3]) # TRUE
test_df1_uniq <- test_df1_uniq %>% select(-2)




#######

# gather
test_df2 <- test_df1_uniq %>% 
  gather(TYPE, X_cols,-sample_characteristics_ch1,-unique_row)%>% 
  select(-TYPE) %>% distinct() %>% 
  filter(!X_cols=="")
test_df2$X_cols <- str_replace_all(test_df2$X_cols,"COMMA",",")


#######

# special cases with ":" and "::"

# check if ratio ":" or double-colon "::" are present and temporarily change it before separating name value
str_extract(test_df2$X_cols,"[:digit:]\\:") %>% unique()  # "3:" "2:" "4:" "6:"   "7:???  CD8:???
#primary tumor: 16: melanoma; 26: breast carcinoma; 27: lung adenocarcinoma, hashing antibodies: patient 16: C0251; patient 26: C0252; patient 27: C0253; healthy donor: C0254; CD8: C0046; CD4: C0045; PD-1/streptavidin: C0961
#tissue: Dorsal pancreas, cell source: Ngn3-Cre;Rosa-RFP+:Ngn3-Cre;Rosa-RFP-=3:1, developmental time point: E16.5

str_extract(test_df2$X_cols,"\\:\\:") %>% unique()  # NA
str_extract(test_df2$X_cols,"\\::") %>% unique()    # NA
#str_extract(test_df2$X_cols,"[a-zA-Z]\\:") %>% unique()



# temporarily change ":" to "isto"
test_df2$X_cols <- str_replace_all(test_df2$X_cols,"2\\:","2isto")
test_df2$X_cols <- str_replace_all(test_df2$X_cols,"3\\:","3isto")
test_df2$X_cols <- str_replace_all(test_df2$X_cols,"4\\:","4isto")
test_df2$X_cols <- str_replace_all(test_df2$X_cols,"6\\:","6isto")
test_df2$X_cols <- str_replace_all(test_df2$X_cols,"7\\:","7isto")
test_df2$X_cols <- str_replace_all(test_df2$X_cols,"8\\:","8isto")
test_df2$X_cols <- str_replace_all(test_df2$X_cols,"\\+\\:","plusisto")  

## In addition some values have ":" in them and cannot be used as separator, eg.
#primary tumor: 16: melanoma; 26: breast carcinoma; 27: lung adenocarcinoma, hashing antibodies: patient 16: C0251; patient 26: C0252; patient 27: C0253; healthy donor: C0254; CD8: C0046; CD4: C0045; PD-1/streptavidin: C0961
test_df2$X_cols <- str_replace_all(test_df2$X_cols,"healthy\\sdonor\\:","healthy donoristo")
test_df2$X_cols <- str_replace_all(test_df2$X_cols,"streptavidin\\:","streptavidinisto")  

# #eg. "cell type: 1:1 mix of UNGKO Hap1 cell line and RNASEH"
# #eg. "strain: Thy1::ChR2"
# #tissue: Blood Buffy coat, patient diagnosis: COVID-19, group: Mild, type of library: expression, subject id: sample contains cells from two patients: CM1, CM2
# #source: BMMC, targeted mutation: CALR exon9 L367Rfs*46 (c.1099_1150del), vaf (targeted exon sequencing): 15% (source: CD34+ cells), Sex: Female, age: 69, diagnosis: ET, wbc(k/mcl): 6.5, hg(g/dl): 12.6, platelets(k/mcl): 519, pb blast count: 0, bm blast count: 0.03, bm fibrosis: MF-1/3, treatment: Aspirin; previously hydroxyurea
# #tissue: melanoma, tumor site: Involved regional lymph node, treatment status: naive, sort purification strategy: Cells were sort-purified as live, singlet, CD45+CD3+CD4-CD8+ and CD45+CD3+CD4+CD8- loaded into a single channel in a 60:40 ratio
# #tissue: melanoma, tumor site: Involved regional lymph node, treatment status: naive, sort purification strategy: Cells were sort-purified as live, singlet, CD45+CD3+CD4-CD8+ and CD45+CD3+CD4+CD8- loaded into a single channel in a 80:20 ratio
# 
# # experiment #: 2, differentiation day: 16, representative population: early_RAG2, sort parameters: RAG1:GFP+
# # experiment #: 1, differentiation day: 20, representative population: early_RAG1, sort parameters: RAG1:GFP+
# 
# #tissue: melanoma, tumor site: Involved regional lymph node, treatment status: naive, sort purification strategy: Cells were sort-purified as live, singlet, CD45+CD3+CD4-CD8+, CD45+CD3+CD4+CD8-, and CD45+CD3- and loaded into a single channel in a 40:40:20 ratio
# 
# # temporarily change ":" to "isto"; "::" to "doubleisto"
# test_df2$X_cols <- str_replace_all(test_df2$X_cols,"1\\:1","1isto1")
# test_df2$X_cols <- str_replace_all(test_df2$X_cols,"\\::","doubleisto")
# test_df2$X_cols <- str_replace_all(test_df2$X_cols,"two patients\\:","two patientsisto")
# test_df2$X_cols[str_detect(test_df2$X_cols,"vaf")] <- str_replace_all(test_df2$X_cols[str_detect(test_df2$X_cols,"vaf")],"source\\:","sourceisto")
# test_df2$X_cols <- str_replace_all(test_df2$X_cols,"60\\:40","60isto40")
# test_df2$X_cols <- str_replace_all(test_df2$X_cols,"80\\:20","80isto20")
# test_df2$X_cols <- str_replace_all(test_df2$X_cols,"RAG1\\:GFP","RAG1istoGFP")
# test_df2$X_cols <- str_replace_all(test_df2$X_cols,"40\\:40\\:20","40isto40isto20")
# 


# separate names from values
# hashing antibodies: patient 16isto C0251; patient 26isto C0252; patient 27isto C0253; healthy donor: C0254; CD8isto C0046; CD4isto C0045; PD-1/streptavidin: C0961

test_df2 <- test_df2 %>% 
  separate(X_cols, c("name","value"), "\\:", convert = TRUE) 



# undo change ":" to "isto"
test_df2$value <- str_replace_all(test_df2$value,"2isto","2:")
test_df2$value <- str_replace_all(test_df2$value,"3isto","3:")
test_df2$value <- str_replace_all(test_df2$value,"4isto","4:")
test_df2$value <- str_replace_all(test_df2$value,"6isto","6:")
test_df2$value <- str_replace_all(test_df2$value,"7isto","7:")
test_df2$value <- str_replace_all(test_df2$value,"8isto","8:")
test_df2$value <- str_replace_all(test_df2$value,"plusisto","+:")
test_df2$value <- str_replace_all(test_df2$value,"healthy donoristo","healthy donor:")
test_df2$value <- str_replace_all(test_df2$value,"streptavidinisto","streptavidin:")


## check if any ":" in values (alphabet:)
str_extract(test_df2$value,"[A-Za-z]\\:") %>% unique() # "r:" is correctly applied




 # 
# # cleanup row with 60:40 ratio and 80:20 ratio and 40:40:20  (: created incorrect parsing in test_df1)
# test_df2$value[str_detect(test_df2$sample_characteristics_ch1,"single channel in a 60\\:40 ratio") & str_detect(test_df2$name,"sort purification strategy")] <- 
#   "Cells were sort-purified as live, singlet, CD45+CD3+CD4-CD8+ and CD45+CD3+CD4+CD8- loaded into a single channel in a 60:40 ratio"
# 
# test_df2$value[str_detect(test_df2$sample_characteristics_ch1,"single channel in a 80\\:20 ratio")& str_detect(test_df2$name,"sort purification strategy")] <- 
#   "Cells were sort-purified as live, singlet, CD45+CD3+CD4-CD8+ and CD45+CD3+CD4+CD8- loaded into a single channel in a 80:20 ratio"
# 
# test_df2$value[str_detect(test_df2$sample_characteristics_ch1,"40\\:40\\:20")& str_detect(test_df2$name,"sort purification strategy")] <- 
#   "Cells were sort-purified as live, singlet, CD45+CD3+CD4-CD8+, CD45+CD3+CD4+CD8-, and CD45+CD3- and loaded into a single channel in a 40:40:20 ratio"
# 
# 
# 
# # remove incompletely parsed row
# test_df2  <- test_df2[which(!str_detect(test_df2$name,"60isto40|80isto20|40isto40isto20")), ]


# count keys
length(unique(test_df2$name)) #254


# remove leading white space from name
test_df2 <- data.frame(lapply(test_df2, trimws), stringsAsFactors = FALSE)


# new column to remove duplicate containing incompletely parsed values
test_df2 <- test_df2 %>% 
  mutate(new = paste0(unique_row,"-",name)) %>% 
  distinct(new, .keep_all = T) %>% 
  select(-new)



################

# wider
test_df2_wider <- test_df2 %>%  
  select(unique_row, sample_characteristics_ch1, name, value) %>%
  pivot_wider(
    names_from = name,
    values_from = value) %>% 
  rename(X1=unique_row)  # rename unique_row back to original name X1
dim(test_df2_wider)
test_df2_wider <- test_df2_wider %>% mutate_all(na_if,"NULL") 

dim(test_df2_wider)  # 6025, set2: 1374 by 221

# character
test_df2_wider <- test_df2_wider %>% mutate_all(as.character)


# join separated name value columns back to geo file
geo_file_separated <- geo_file %>% select(-sample_characteristics_ch1) %>% left_join(test_df2_wider, by="X1")
dim(geo_file_separated)  # 1374 by 322

# cleanup
separated_colnames <- unique(colnames(geo_file_separated))


# Categorize for ontology mapping based on criteria:
common_cols <- c("X1", "sample_geo_accession","series_title","series_summary","series_overall_design","sample_organism_ch1","sample_characteristics_ch1")
tissue_cols <- separated_colnames[grepl("tissue",separated_colnames)]
cell_line_cols <- separated_colnames[grepl("cell|line",separated_colnames)]
cell_type_cols <- separated_colnames[grepl("cell|type",separated_colnames)]
separated_colnames[grepl("xenograft",separated_colnames)]# null
separated_colnames[grepl("species",separated_colnames)]  # null
experiment_cols <- separated_colnames[grepl("experiment",separated_colnames)]
disease_cols <- separated_colnames[grepl("disease",separated_colnames)]
facs_cols <- separated_colnames[grepl("facs|marker|sort|genotype|cell type|subpopulation|nuclei type|wbc",separated_colnames)]
age_cols <-separated_colnames[grepl("^age|^gender",separated_colnames)]
age_cols <- age_cols[!grepl("agent",age_cols)]
sex_cols <- separated_colnames[grepl("Sex|sex|gender|mouse train|strain",separated_colnames)]
technology_cols <- separated_colnames[grepl("platform|technology|chemistry|sequencing|library|10x",separated_colnames)]
subject_cols <- separated_colnames[grepl("subject",separated_colnames)]
development_cols <- separated_colnames[grepl("stage|development|^age$|^tag",separated_colnames)]
development_cols <- development_cols[!grepl("tumor stage|tumorstage|cell id at each stage|agent|differentiation stage",development_cols)]
mousestrain_cols <- separated_colnames[grepl("strain",separated_colnames)]
genotype_cols <- separated_colnames[grepl("genotype",separated_colnames)]
treatment_cols <- separated_colnames[grepl("treatment",separated_colnames)]
dose_cols <- separated_colnames[grepl("dose",separated_colnames)]
donor_cols <- separated_colnames[grepl("donor",separated_colnames)]
subject_cols <- separated_colnames[grepl("subject",separated_colnames)]




## TISSUE
tissue_curation <- geo_file_separated %>% select(append(common_cols, tissue_cols))
## DISEASE
disease_curation <- geo_file_separated %>% select(append(common_cols, disease_cols))

## CELL TYPE 
cell_type_curation <- geo_file_separated %>% select(append(common_cols,cell_type_cols))


## CELL LINE # only map to ontology not controlled vocab such as cellosaurus
cell_line_curation <- geo_file_separated %>% select(append(common_cols,cell_line_cols))

## FACS
facs_curation <- geo_file_separated %>% select(append(common_cols,facs_cols))







geo <- geo %>% 
  select(-`concatenated column`, -`QC assignment`) %>% 
  relocate("QC comment", .after=last_col())  # relocate a general spot check QC comment to the lat column

# write file 
## Cleanup symbol/special character issues
# example: Foxp3eGFP-CreERT2Â Rosa26NRG (NOD-Rag2-/- Î³c-/-) mice Î±-SMACreERT2:RosaYFP 3â€™ v3 chemistry and barcoded with a 10Ã 500ÂµL
# example: as per manufacturerâ€™s

# to ensure symbol/special characters (eg. 10Ã 500ÂµL) are correctly represented in the final table, use write_excel_csv
write_excel_csv(geo,"~/GEO.csv")   


























