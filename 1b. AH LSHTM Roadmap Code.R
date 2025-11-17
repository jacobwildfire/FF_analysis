############ AH LSHTM Roadmap Roadmap Code
#######
#######
####### Created by Dr Jacob Wildfire as part of the Fleming Fund



####### This code was designed to collate the answers to the Animal Health
####### Site Planning and Monitoring tool to determine the roadmap status for
####### each site at each time period.
#######



# Note: All path examples provide a default
#       that can be run using the files provided
#       in the "FF_analysis" GitHub repository,
#       provided appropriate modifications
#       are made (see README.txt).




#################################### Required packages

# List of required packages
required_packages <- c("dplyr", "readxl", "writexl", "stringr", 
                       "tidyr", "DescTools")

# Install missing packages
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages)) install.packages(missing_packages)

# Load all required packages
for(pkg in required_packages) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  message(paste("Loaded package:", pkg))
}


#################################### Working Directory

wd <- "~/FF_analysis-main/" # Replace with your working directory. 


if (getwd() != wd) { 
  setwd(wd)
}


#################################### Loading dataframes

## Load in the appropriate DHIS2 datafile 
df <- read.csv("DHIS2 exported files/AH_site_data_example.csv") # Replace with your DHIS2 AH site tool data .csv filepath

## Load in site masterlist to obtain site information:
site_info <- read_xlsx("Resource files/Site masterlist template.xlsx", sheet = "Sentinel Sites") # Replace with your site masterlist .xlsx filepath 



#################################### Generating necessary dataframes

ah_all_df <- data.frame(
  site = NA, sitecode = NA, type = NA, sector = NA, reporting.month = NA,
  a1_guidelines = NA, b1_sops = NA, b2_soptraining = NA, b3_qc = NA, b4_qctraining = NA,
  b5_participateeqa = NA, b6_provideeqa = NA,
  a1_transport_sop = NA, a2_transop_training = NA, a3_transop_followed = NA,
  ecoli = NA, salmonella = NA, enterococci = NA, campylobacter = NA, others = NA,
  staph = NA, listeria = NA,
  b2_bacid_automated = NA, b3_bacid_system = NA,
  c1_ast_system = NA, c2_mic = NA, c3_ast_sop = NA,
  d1_astsop_training = NA, d2_ast_training_othersites = NA,
  a1_isolates_stored = NA, a2_freezer_backupupsreliable = NA, a3_freezers_servicecontract = NA,
  a4_isolatespecimen_linkage = NA, a5_isolates_inventory = NA,
  b1_biobanking = NA, b2_biobanking_freq = NA,
  c1_isolatetransport_sop = NA, c2_isolatetransport_training = NA, c3_sampleStorageSOP = NA,
  a1_datasubmitted_amrcc = NA, a2_datasubmitted_amrcc_freq = NA,
  a3_datasharing_guidelines = NA, a4_datasharing_other = NA,
  b1_datalinkage = NA, b2_uniqueid = NA, c_datasharing_policy = NA,
  check.names = FALSE
)

## Generate a workbook into which tiers will be calculated
ah_tiers_df <- data.frame(
  site = NA, sitecode = NA, type = NA, sector = NA, reporting.month = NA,
  tier1a = NA, tier1b1 = NA, tier1b2 = NA, tier1b3 = NA, tier1b4 = NA, tier1b5 = NA, tier1b6 = NA, tier1b = NA,
  tier2a1 = NA, tier2a2 = NA, tier2a3 = NA, tier2a = NA,
  tier2b1ecoli = NA, tier2b1salmonella = NA, tier2b1enterococci = NA, tier2b1campylobacter = NA,
  tier2b1staph = NA, tier2b1listeria = NA,
  tier2b2 = NA, tier2b3 = NA, tier2b = NA,
  tier2c1 = NA, tier2c2 = NA, tier2c3 = NA, tier2c = NA,
  tier2d1 = NA, tier2d2 = NA, tier2d = NA,
  tier3a1 = NA, tier3a2 = NA, tier3a3 = NA, tier3a4 = NA, tier3a5 = NA, tier3a = NA,
  tier3b1 = NA, tier3b2 = NA, tier3b = NA,
  tier3c1 = NA, tier3c2 = NA, tier3c3 = NA, tier3c = NA,
  tier4a1 = NA, tier4a2 = NA, tier4a3 = NA, tier4a4 = NA, tier4a = NA,
  tier4b1 = NA, tier4b2 = NA, tier4b = NA, tier4c = NA, check.names = FALSE
)



#################################### Prepare relevant dataframes
  
## Identify LSHTM roadmap relevant dataelements:
dataelements <- c("ah_lshtm_1a1_guidelines",
                  "ah_lshtm_1b1_sops",
                  "ah_lshtm_1b2_soptraining",
                  "ah_lshtm_1b3_qc",
                  "ah_lshtm_1b4_qctraining",
                  "ah_lshtm_1b5_participateeqa",
                  "ah_lshtm_1b6_provideeqa",
                  "ah_lshtm_2a1_transport_sop",
                  "ah_lshtm_2a2_transop_training",
                  "ah_lshtm_2a3_transop_followed",
                  "ah_lshtm_2b1_ecoli",
                  "ah_lshtm_2b1_salmonella",
                  "ah_lshtm_2b1_enterococci",
                  "ah_lshtm_2b1_campylobacter",
                  "ah_lshtm_2b1_others",
                  "ah_6b1_staph",
                  "ah_6a2_listeria",
                  "ah_lshtm_2b2_bacid_automated",
                  "ah_lshtm_2b3_bacid_system",
                  "ah_lshtm_2c1_ast_system",
                  "ah_lshtm_2c2_mic",
                  "ah_lshtm_2c3_ast_sop",
                  "ah_lshtm_2d1_astsop_training",
                  "ah_lshtm_2d2_ast_training_othersites",
                  "ah_lshtm_3a1_isolates_stored",
                  "ah_lshtm_3a2_freezer_backupupsreliable",
                  "ah_lshtm_3a3_freezers_servicecontract",
                  "ah_lshtm_3a4_isolatespecimen_linkage",
                  "ah_lshtm_3a5_isolates_inventory",
                  "ah_lshtm_3b1_biobanking",
                  "ah_lshtm_3b2_biobanking_freq",
                  "ah_lshtm_3c1_isolatetransport_sop",
                  "ah_lshtm_3c2_isolatetransport_training",
                  "ah_6c2_sampleStorageSOP",
                  "ah_lshtm_4a1_datasubmitted_amrcc",
                  "ah_lshtm_4a2_datasubmitted_amrcc_freq",
                  "ah_lshtm_4a3_datasharing_guidelines",
                  "ah_lshtm_4a4_datasharing_other",
                  "ah_lshtm_4b1_datalinkage",
                  "ah_lshtm_4b2_uniqueid",
                  "ah_lshtm_4c_datasharing_policy")

## Convert the df$lastupdated column to only the first 10 characters to get the date:
df <- df %>%
  mutate(lastupdated = str_sub(lastupdated, 1, 10))

## Remove all of the non-relevant SP&M dataelements
df <- df[df$dataelement %in% dataelements,]

## Remove useless rows and remove all orgunit characters after the first 5 letters:
df <- select(df,c(1:3,6))%>%
  mutate(orgunit = str_sub(orgunit, 1, 5))

## Make df "orgunit" into "site code"
colnames(df) <- c("dataelement", "reporting.month", "sitecode", "value")


#################################### Mapping DHIS2 values to ah_all_df

## Transform df to a wide format
df_wide <- df %>%
  mutate(dataelement = str_replace(dataelement, "ah_", "")) %>%
  pivot_wider(names_from = dataelement, values_from = value)


## Create an adequate number of blank rows (one per site per report period)
## add blank rows.
blank_rows <- data.frame(matrix(ncol = ncol(ah_all_df), nrow = nrow(df_wide)))
colnames(blank_rows) <- colnames(ah_all_df)

# Bind the blank rows to the empty hh_all_df dataframe
ah_all_df <- blank_rows

rm(blank_rows)

## Stick the site codes in so they can be tracked
ah_all_df$sitecode <- df_wide$sitecode
ah_all_df$reporting.month<- df_wide$reporting.month

## Generate a "key" column that can be used to quickly run site/date loops simultaneously:
df_wide$key <- paste(df_wide$sitecode, df_wide$reporting.month, sep = "_")
ah_all_df$key <- df_wide$key

## Locate the relevant data from the site info list and insert it into the df
for (i in ah_all_df$sitecode) {
  ah_all_df[ah_all_df$sitecode == i, "site"] <- site_info[site_info$`Site Code` == i, "Laboratory"]
  ah_all_df[ah_all_df$sitecode == i, "type"] <- site_info[site_info$`Site Code` == i, "Type"]
  ah_all_df[ah_all_df$sitecode == i, "sector"] <- site_info[site_info$`Site Code` == i, "Sub-sector"]
}


## Find the relevant values and plug them into the ah_all_df
for (i in df_wide$key) {
  if (!is.na(df_wide[df_wide$key == i, "lshtm_1a1_guidelines"])) {
    ah_all_df[ah_all_df$key == i, "a1_guidelines"] <- df_wide[df_wide$key == i, "lshtm_1a1_guidelines"]
  }
  
  if (!is.na(df_wide[df_wide$key == i, "lshtm_1b1_sops"])) {
    ah_all_df[ah_all_df$key == i, "b1_sops"] <- df_wide[df_wide$key == i, "lshtm_1b1_sops"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_1b2_soptraining"])) {
    ah_all_df[ah_all_df$key == i, "b2_soptraining"] <- df_wide[df_wide$key == i, "lshtm_1b2_soptraining"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_1b3_qc"])) {
    ah_all_df[ah_all_df$key == i, "b3_qc"] <- df_wide[df_wide$key == i, "lshtm_1b3_qc"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_1b4_qctraining"])) {
    ah_all_df[ah_all_df$key == i, "b4_qctraining"] <- df_wide[df_wide$key == i, "lshtm_1b4_qctraining"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_1b5_participateeqa"])) {
    ah_all_df[ah_all_df$key == i, "b5_participateeqa"] <- df_wide[df_wide$key == i, "lshtm_1b5_participateeqa"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_1b6_provideeqa"])) {
    ah_all_df[ah_all_df$key == i, "b6_provideeqa"] <- df_wide[df_wide$key == i, "lshtm_1b6_provideeqa"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_2a1_transport_sop"])) {
    ah_all_df[ah_all_df$key == i, "a1_transport_sop"] <- df_wide[df_wide$key == i, "lshtm_2a1_transport_sop"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_2a2_transop_training"])) {
    ah_all_df[ah_all_df$key == i, "a2_transop_training"] <- df_wide[df_wide$key == i, "lshtm_2a2_transop_training"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_2a3_transop_followed"])) {
    ah_all_df[ah_all_df$key == i, "a3_transop_followed"] <- df_wide[df_wide$key == i, "lshtm_2a3_transop_followed"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_2b1_ecoli"])) {
    ah_all_df[ah_all_df$key == i, "ecoli"] <- df_wide[df_wide$key == i, "lshtm_2b1_ecoli"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_2b1_salmonella"])) {
    ah_all_df[ah_all_df$key == i, "salmonella"] <- df_wide[df_wide$key == i, "lshtm_2b1_salmonella"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_2b1_enterococci"])) {
    ah_all_df[ah_all_df$key == i, "enterococci"] <- df_wide[df_wide$key == i, "lshtm_2b1_enterococci"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_2b1_campylobacter"])) {
    ah_all_df[ah_all_df$key == i, "campylobacter"] <- df_wide[df_wide$key == i, "lshtm_2b1_campylobacter"]
  }
  if (!is.na(df_wide[df_wide$key == i, "6b1_staph"])) {
    ah_all_df[ah_all_df$key == i, "staph"] <- df_wide[df_wide$key == i, "6b1_staph"]
  }
  if (!is.na(df_wide[df_wide$key == i, "6a2_listeria"])) {
    ah_all_df[ah_all_df$key == i, "listeria"] <- df_wide[df_wide$key == i, "6a2_listeria"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_2b2_bacid_automated"])) {
    ah_all_df[ah_all_df$key == i, "b2_bacid_automated"] <- df_wide[df_wide$key == i, "lshtm_2b2_bacid_automated"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_2b3_bacid_system"])) {
    ah_all_df[ah_all_df$key == i, "b3_bacid_system"] <- df_wide[df_wide$key == i, "lshtm_2b3_bacid_system"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_2c1_ast_system"])) {
    ah_all_df[ah_all_df$key == i, "c1_ast_system"] <- df_wide[df_wide$key == i, "lshtm_2c1_ast_system"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_2c2_mic"])) {
    ah_all_df[ah_all_df$key == i, "c2_mic"] <- df_wide[df_wide$key == i, "lshtm_2c2_mic"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_2c3_ast_sop"])) {
    ah_all_df[ah_all_df$key == i, "c3_ast_sop"] <- df_wide[df_wide$key == i, "lshtm_2c3_ast_sop"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_2d1_astsop_training"])) {
    ah_all_df[ah_all_df$key == i, "d1_astsop_training"] <- df_wide[df_wide$key == i, "lshtm_2d1_astsop_training"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_2d2_ast_training_othersites"])) {
    ah_all_df[ah_all_df$key == i, "d2_ast_training_othersites"] <- df_wide[df_wide$key == i, "lshtm_2d2_ast_training_othersites"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_3a1_isolates_stored"])) {
    ah_all_df[ah_all_df$key == i, "a1_isolates_stored"] <- df_wide[df_wide$key == i, "lshtm_3a1_isolates_stored"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_3a2_freezer_backupupsreliable"])) {
    ah_all_df[ah_all_df$key == i, "a2_freezer_backupupsreliable"] <- df_wide[df_wide$key == i, "lshtm_3a2_freezer_backupupsreliable"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_3a3_freezers_servicecontract"])) {
    ah_all_df[ah_all_df$key == i, "a3_freezers_servicecontract"] <- df_wide[df_wide$key == i, "lshtm_3a3_freezers_servicecontract"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_3a4_isolatespecimen_linkage"])) {
    ah_all_df[ah_all_df$key == i, "a4_isolatespecimen_linkage"] <- df_wide[df_wide$key == i, "lshtm_3a4_isolatespecimen_linkage"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_3a5_isolates_inventory"])) {
    ah_all_df[ah_all_df$key == i, "a5_isolates_inventory"] <- df_wide[df_wide$key == i, "lshtm_3a5_isolates_inventory"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_3b1_biobanking"])) {
    ah_all_df[ah_all_df$key == i, "b1_biobanking"] <- df_wide[df_wide$key == i, "lshtm_3b1_biobanking"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_3b2_biobanking_freq"])) {
    ah_all_df[ah_all_df$key == i, "b2_biobanking_freq"] <- df_wide[df_wide$key == i, "lshtm_3b2_biobanking_freq"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_3c1_isolatetransport_sop"])) {
    ah_all_df[ah_all_df$key == i, "c1_isolatetransport_sop"] <- df_wide[df_wide$key == i, "lshtm_3c1_isolatetransport_sop"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_3c2_isolatetransport_training"])) {
    ah_all_df[ah_all_df$key == i, "c2_isolatetransport_training"] <- df_wide[df_wide$key == i, "lshtm_3c2_isolatetransport_training"]
  }
  if (!is.na(df_wide[df_wide$key == i, "6c2_sampleStorageSOP"])) {
    ah_all_df[ah_all_df$key == i, "c3_sampleStorageSOP"] <- df_wide[df_wide$key == i, "6c2_sampleStorageSOP"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_4a1_datasubmitted_amrcc"])) {
    ah_all_df[ah_all_df$key == i, "a1_datasubmitted_amrcc"] <- df_wide[df_wide$key == i, "lshtm_4a1_datasubmitted_amrcc"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_4a2_datasubmitted_amrcc_freq"])) {
    ah_all_df[ah_all_df$key == i, "a2_datasubmitted_amrcc_freq"] <- df_wide[df_wide$key == i, "lshtm_4a2_datasubmitted_amrcc_freq"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_4a3_datasharing_guidelines"])) {
    ah_all_df[ah_all_df$key == i, "a3_datasharing_guidelines"] <- df_wide[df_wide$key == i, "lshtm_4a3_datasharing_guidelines"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_4a4_datasharing_other"])) {
    ah_all_df[ah_all_df$key == i, "a4_datasharing_other"] <- df_wide[df_wide$key == i, "lshtm_4a4_datasharing_other"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_4b1_datalinkage"])) {
    ah_all_df[ah_all_df$key == i, "b1_datalinkage"] <- df_wide[df_wide$key == i, "lshtm_4b1_datalinkage"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_4b2_uniqueid"])) {
    ah_all_df[ah_all_df$key == i, "b2_uniqueid"] <- df_wide[df_wide$key == i, "lshtm_4b2_uniqueid"]
  }
  if (!is.na(df_wide[df_wide$key == i, "lshtm_4c_datasharing_policy"])) {
    ah_all_df[ah_all_df$key == i, "c_datasharing_policy"] <- df_wide[df_wide$key == i, "lshtm_4c_datasharing_policy"]
  }
}


## For those that remain that have NA values, change NA to "Please choose", indicating
## that no option was put.
ah_all_df$others <- as.character(ah_all_df$others)
ah_all_df <- ah_all_df %>% 
  mutate(across(c(14:ncol(ah_all_df)), ~tidyr::replace_na(.x, "Please choose")))



if ((nrow(ah_all_df)-nrow(ah_tiers_df)) > 0) {
  row_diff <- nrow(ah_all_df)-nrow(ah_tiers_df)
  # Create a dataframe with 'row_diff' number of blank rows
  blank_rows <- data.frame(matrix(ncol = ncol(ah_tiers_df), nrow = row_diff))
  colnames(blank_rows) <- colnames(ah_tiers_df)
  
  # Bind the blank rows to df2
  ah_tiers_df <- rbind(ah_tiers_df, blank_rows)
}

rm(blank_rows, row_diff)

## Set the tiers dataset sitecode and reporting month to be the same as hh_all_df
ah_tiers_df$sitecode <- ah_all_df$sitecode
ah_tiers_df$site <- ah_all_df$site
ah_tiers_df$type <- ah_all_df$type
ah_tiers_df$sector <- ah_all_df$sector
ah_tiers_df$reporting.month <- ah_all_df$reporting.month
ah_tiers_df$key <- ah_all_df$key

######################





################################## Calculating tiers for each site

## First, change all "true" values to "Yes", and all "false" values to "No"

ah_all_df <- ah_all_df %>%
  mutate_if(is.character, ~ ifelse(. %in% c("true", "TRUE"), "Yes", .))


ah_all_df <- ah_all_df %>%
  mutate_if(is.character, ~ ifelse(. %in% c("false", "FALSE"), "No", .))




## Calculate subcomponent 1

############# Calculating tiers of component 1


for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "a1_guidelines"] %like% c("Yes")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier1a"] <- "Core"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier1a"] <- "Precore"
  }
}


## Calculating tiers for Subcomponent 1b - 
## 
##
##

## Calculating tiers for 1b1
##
## Required for "Core" for 1b


for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "b1_sops"] %like% c("Yes")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier1b1"] <- "Core"
  } else { 
    ah_tiers_df[ah_tiers_df$key == i, "tier1b1"] <- "Precore"}
}

## Calculating tiers for 1b2
##
## "Yes" is assumed to be "All staff"

for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "b2_soptraining"] %like% c("Yes", "All staff")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier1b2"] <- "Core"
  } else { 
    ah_tiers_df[ah_tiers_df$key == i, "tier1b2"] <- "Precore"}
}


## Calculating tiers for 1b3
##


for (i in ah_all_df$key) {
  if (ah_all_df[ah_all_df$key == i, "b3_qc"] %like% c("Yes")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier1b3"] <- "Core"
  } else { 
    ah_tiers_df[ah_tiers_df$key == i, "tier1b3"] <- "Precore"}
}


## Calculating tiers for 1b4
##
## "Yes" is assumed to be "All staff"

for (i in ah_all_df$key) {
  if (ah_all_df[ah_all_df$key == i, "b4_qctraining"] %like% c("Yes", "All staff")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier1b4"] <- "Core"
  } else { 
    ah_tiers_df[ah_tiers_df$key == i, "tier1b4"] <- "Precore"}
}


## Calculating tiers for 1b5
##

for (i in ah_all_df$key) {
  if (ah_all_df[ah_all_df$key == i, "b5_participateeqa"] %in% c("Yes")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier1b5"] <- "Extended"
  } else { 
    ah_tiers_df[ah_tiers_df$key == i, "tier1b5"] <- "Precore"}
}


## Calculating tiers for 1b6
##

for (i in ah_all_df$key) {
  if (ah_all_df[ah_all_df$key == i, "b6_provideeqa"] %in% c("Yes")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier1b6"] <- "Advanced"
  } else { 
    ah_tiers_df[ah_tiers_df$key == i, "tier1b6"] <- "Precore"}
}


##### Calculating overall tiers for 1b
##
##
## Requires 1b1-1b4 to be "Core" for "Core" to be achieved. I have also included
## calculations where "Extended" and "Advanced" are given based on the results of
## 1b5 and 1b6.

for (i in ah_all_df$key){
  if (ah_tiers_df[ah_tiers_df$key == i, "tier1b1"] == "Core" &
      ah_tiers_df[ah_tiers_df$key == i, "tier1b2"] == "Core" &
      ah_tiers_df[ah_tiers_df$key == i, "tier1b3"] == "Core" &
      ah_tiers_df[ah_tiers_df$key == i, "tier1b4"] == "Core" &
      ah_tiers_df[ah_tiers_df$key == i, "tier1b5"] == "Extended" &
      ah_tiers_df[ah_tiers_df$key == i, "tier1b6"] == "Advanced"){
    ah_tiers_df[ah_tiers_df$key == i, "tier1b"] <- "Advanced"
  } else if (ah_tiers_df[ah_tiers_df$key == i, "tier1b1"] == "Core" &
             ah_tiers_df[ah_tiers_df$key == i, "tier1b2"] == "Core" &
             ah_tiers_df[ah_tiers_df$key == i, "tier1b3"] == "Core" &
             ah_tiers_df[ah_tiers_df$key == i, "tier1b4"] == "Core" &
             ah_tiers_df[ah_tiers_df$key == i, "tier1b5"] == "Extended" &
             ah_tiers_df[ah_tiers_df$key == i, "tier1b6"] != "Advanced") {
    ah_tiers_df[ah_tiers_df$key == i, "tier1b"] <- "Extended"
  } else if (ah_tiers_df[ah_tiers_df$key == i, "tier1b1"] == "Core" &
             ah_tiers_df[ah_tiers_df$key == i, "tier1b2"] == "Core" &
             ah_tiers_df[ah_tiers_df$key == i, "tier1b3"] == "Core" &
             ah_tiers_df[ah_tiers_df$key == i, "tier1b4"] == "Core" &
             ah_tiers_df[ah_tiers_df$key == i, "tier1b5"] != "Extended")  {
    ah_tiers_df[ah_tiers_df$key == i, "tier1b"] <- "Core"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier1b"] <- "Precore"
  }
}





############# Calculating tiers of component 2

## Calculating tiers for Subcomponent 2a - 
##



## Calculating tiers for 2a1

for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "a1_transport_sop"] %like% c("Yes")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier2a1"] <- "Core"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier2a1"] <- "Precore"
  }
}

## Calculating tiers for 2a2
##
## "Yes" is assumed to be "All staff"

for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "a2_transop_training"] %like% c("All staff", "Yes")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier2a2"] <- "Core"
  } else { 
    ah_tiers_df[ah_tiers_df$key == i, "tier2a2"] <- "Precore"}
}

## Calculating tiers for 2a3

for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "a3_transop_followed"] %like% c("Yes", "All staff")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier2a3"] <- "Core"
  } else { 
    ah_tiers_df[ah_tiers_df$key == i, "tier2a3"] <- "Precore"}
}




###### Calculating overall tiers for 2a


## Since the SOPs were developed to be according to international safety standards,
## achievement of at all levels for 2a should equal "Extended" for 2a. 

for (i in ah_all_df$key){
  if (ah_tiers_df[ah_tiers_df$key == i, "tier2a1"] == "Core" &
      ah_tiers_df[ah_tiers_df$key == i, "tier2a2"] == "Core" &
      ah_tiers_df[ah_tiers_df$key == i, "tier2a3"] == "Core") {
    ah_tiers_df[ah_tiers_df$key == i, "tier2a"] <- "Extended"
  } else if (ah_tiers_df[ah_tiers_df$key == i, "tier2a1"] == "Not applicable") {
    ah_tiers_df[ah_tiers_df$key == i, "tier2a"] <- "Not applicable"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier2a"] <- "Precore"
  }
}



###### Calculating overall tiers for 2b
##
##
## The relevant priority pathogens are E. coli and Salmonella

## Calculating tiers for Subcomponent ecoli - processing of E. coli samples

for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "ecoli"] == "Yes, according to SOP") {
    ah_tiers_df[ah_tiers_df$key == i, "tier2b1ecoli"] <- "Core"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier2b1ecoli"] <- "Precore"
  }
}

## Calculating tiers for Subcomponent salmonella - processing of Salmonella samples

for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "salmonella"] == "Yes, according to SOP") {
    ah_tiers_df[ah_tiers_df$key == i, "tier2b1salmonella"] <- "Core"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier2b1salmonella"] <- "Precore"
  }
}


## Calculating tiers for Subcomponent staph - processing of Staphylococcus samples

for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "staph"] == "Yes, according to SOP") {
    ah_tiers_df[ah_tiers_df$key == i, "tier2b1staph"] <- "Core"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier2b1staph"] <- "Precore"
  }
}

## Calculating tiers for Subcomponent enterococci - processing of Enterococci samples

for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "enterococci"] == "Yes, according to SOP") {
    ah_tiers_df[ah_tiers_df$key == i, "tier2b1enterococci"] <- "Extended"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier2b1enterococci"] <- "Precore"
  }
}


## Calculating tiers for Subcomponent campylobacter - processing of Campylobacter samples

for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "campylobacter"] == "Yes, according to SOP") {
    ah_tiers_df[ah_tiers_df$key == i, "tier2b1campylobacter"] <- "Advanced"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier2b1campylobacter"] <- "Precore"
  }
}

## Calculating tiers for Subcomponent listeria - processing of Listeria samples

for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "listeria"] == "Yes, according to SOP") {
    ah_tiers_df[ah_tiers_df$key == i, "tier2b1listeria"] <- "Advanced"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier2b1listeria"] <- "Precore"
  }
}

## Calculating tiers for 2b2
##
## This differs in that now SOPs are called for, and I assume are mandatory for "Advanced"
 
for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "b2_bacid_automated"] == "Yes, according to SOP") {
    ah_tiers_df[ah_tiers_df$key == i, "tier2b2"] <- "Advanced"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier2b2"] <- "Precore"
  }
}

## Calculating tiers for 2b3
## Seems redundant if 2b2 is there, as it's just asking for which automated identification system.
## However equally, I don't think it will affect progress calculation.

for (i in ah_all_df$key) {
  if (ah_all_df[ah_all_df$key == i, "b3_bacid_system"] %like% c("Vitek%", 
                                                                     "VITEK%", "Other%",
                                                                     "MALDI%",
                                                                     "Maldi%",
                                                                     "BD%")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier2b3"] <- "Advanced"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier2b3"] <- "Precore"
  }
}



###### Calculating overall tiers for 2b
##
##
##
##
## Calculated under the assumption that E. coli and Salmonella are required for "Core", 
## and that Enterococci are required for "Extended" (as per AH
## LSHTM roadmap), and that Campylobacter and an answer which indicates that automated identification
## is available is sufficient for "Advanced"

for (i in ah_all_df$key){
  if(as.numeric(substr(ah_all_df[ah_all_df$key == i,"reporting.month"], 1, 4))>2023){
    if (ah_tiers_df[ah_tiers_df$key == i, "tier2b1ecoli"] == "Core" &
        ah_tiers_df[ah_tiers_df$key == i, "tier2b1salmonella"] == "Core" &
        ah_tiers_df[ah_tiers_df$key == i, "tier2b1staph"] == "Core" &
        ah_tiers_df[ah_tiers_df$key == i, "tier2b1enterococci"] == "Extended" &
        ah_tiers_df[ah_tiers_df$key == i, "tier2b1campylobacter"] == "Advanced" &
        ah_tiers_df[ah_tiers_df$key == i, "tier2b1listeria"] == "Advanced" &
        (ah_tiers_df[ah_tiers_df$key == i, "tier2b2"] == "Advanced" |
         ah_tiers_df[ah_tiers_df$key == i, "tier2b3"] == "Advanced")) {
      ah_tiers_df[ah_tiers_df$key == i, "tier2b"] <- "Advanced"
    } else if (ah_tiers_df[ah_tiers_df$key == i, "tier2b1ecoli"] == "Core" &
               ah_tiers_df[ah_tiers_df$key == i, "tier2b1salmonella"] == "Core" &
               ah_tiers_df[ah_tiers_df$key == i, "tier2b1staph"] == "Core" &
               ah_tiers_df[ah_tiers_df$key == i, "tier2b1enterococci"] == "Extended" &
               (ah_tiers_df[ah_tiers_df$key == i, "tier2b1campylobacter"] != "Advanced" |
                ah_tiers_df[ah_tiers_df$key == i, "tier2b1listeria"] != "Advanced" |
                (ah_tiers_df[ah_tiers_df$key == i, "tier2b2"] != "Advanced" &
                 ah_tiers_df[ah_tiers_df$key == i, "tier2b3"] != "Advanced"))) {
      ah_tiers_df[ah_tiers_df$key == i, "tier2b"] <- "Extended"
    } else if (ah_tiers_df[ah_tiers_df$key == i, "tier2b1ecoli"] == "Core" &
               ah_tiers_df[ah_tiers_df$key == i, "tier2b1salmonella"] == "Core" &
               ah_tiers_df[ah_tiers_df$key == i, "tier2b1staph"] == "Core" &
               ah_tiers_df[ah_tiers_df$key == i, "tier2b1enterococci"] == "Precore") {
      ah_tiers_df[ah_tiers_df$key == i, "tier2b"] <- "Core"
    } else {
      ah_tiers_df[ah_tiers_df$key == i, "tier2b"] <- "Precore"
    }
  } else {
    if (ah_tiers_df[ah_tiers_df$key == i, "tier2b1ecoli"] == "Core" &
        ah_tiers_df[ah_tiers_df$key == i, "tier2b1salmonella"] == "Core" &
        ah_tiers_df[ah_tiers_df$key == i, "tier2b1enterococci"] == "Extended" &
        ah_tiers_df[ah_tiers_df$key == i, "tier2b1campylobacter"] == "Advanced" &
        (ah_tiers_df[ah_tiers_df$key == i, "tier2b2"] == "Advanced" |
         ah_tiers_df[ah_tiers_df$key == i, "tier2b3"] == "Advanced")) {
      ah_tiers_df[ah_tiers_df$key == i, "tier2b"] <- "Advanced"
    } else if (ah_tiers_df[ah_tiers_df$key == i, "tier2b1ecoli"] == "Core" &
               ah_tiers_df[ah_tiers_df$key == i, "tier2b1salmonella"] == "Core" &
               ah_tiers_df[ah_tiers_df$key == i, "tier2b1enterococci"] == "Extended" &
               (ah_tiers_df[ah_tiers_df$key == i, "tier2b1campylobacter"] != "Advanced" |
                (ah_tiers_df[ah_tiers_df$key == i, "tier2b2"] != "Advanced" &
                 ah_tiers_df[ah_tiers_df$key == i, "tier2b3"] != "Advanced"))) {
      ah_tiers_df[ah_tiers_df$key == i, "tier2b"] <- "Extended"
    } else if (ah_tiers_df[ah_tiers_df$key == i, "tier2b1ecoli"] == "Core" &
               ah_tiers_df[ah_tiers_df$key == i, "tier2b1salmonella"] == "Core" &
               ah_tiers_df[ah_tiers_df$key == i, "tier2b1enterococci"] == "Precore") {
      ah_tiers_df[ah_tiers_df$key == i, "tier2b"] <- "Core"
    } else {
      ah_tiers_df[ah_tiers_df$key == i, "tier2b"] <- "Precore"
    }
  }
}


## Calculating tiers for Subcomponent 2c - 
##
## 

## Calculating tiers for 2c1
## 
## MIC testing is an option, although this is not explicitly called for in the LSHTM roadmap.
## I have therefore assumed that this is not a broth based method (as it could be either),
## So I have made the conservative option to assign "Core".

for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "c1_ast_system"] %like% c("Automated method%")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier2c1"] <- "Advanced"
  } else if (ah_all_df[ah_all_df$key == i, "c1_ast_system"] %in% c("Disc diffusion", "MIC testing")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier2c1"] <- "Core"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier2c1"] <- "Precore"
  }
}


## Calculating tiers for 2c2

for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "c2_mic"] %like% c("By Etest", "By broth dilution",
                                                            "Using both Etest and broth dilution",
                                                            "Other (e.g., automated methods)")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier2c2"] <- "Advanced"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier2c2"] <- "Precore"
  }
}

## Calculating tiers for 2c3


for (i in  ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "c3_ast_sop"] %in% c("The EUCAST standard of year 20XX",
                                                                   "The latest EUCAST standard",
                                                                   "The CLSI standard of year 20XX",
                                                                   "The latest CLSI standard")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier2c3"] <- "Core"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier2c3"] <- "Precore"
  }
}

###### Calculating overall tiers for 2c
##
##
##
##
## Since "Extended" requires the additional pathogens, I will set it as such that "Extended" 
## can be achieved if Enterococci and Campylobacter are tested for.
## This also requires the 2b "Core" pathogens to be tested for for "Core" to be
## achieved for 2c. Might need to be changed in the LSHTM HH calculation.




for (i in ah_all_df$key){
  if ((ah_tiers_df[ah_tiers_df$key == i, "tier2c1"] == "Advanced" |
       ah_tiers_df[ah_tiers_df$key == i, "tier2c2"] == "Advanced") &
      ah_tiers_df[ah_tiers_df$key == i, "tier2c1"] != "Precore" &
      ah_tiers_df[ah_tiers_df$key == i, "tier2c3"] == "Core" &
      ah_tiers_df[ah_tiers_df$key == i, "tier2b"] %in% c("Extended", "Advanced")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier2c"] <- "Advanced"
  } else if (ah_tiers_df[ah_tiers_df$key == i, "tier2c1"] == "Core" &
             ah_tiers_df[ah_tiers_df$key == i, "tier2c2"] == "Precore" &
             ah_tiers_df[ah_tiers_df$key == i, "tier2c3"] == "Core" &
             ah_tiers_df[ah_tiers_df$key == i, "tier2b"] %in% c("Extended", "Advanced")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier2c"] <- "Extended"
  } else if (ah_tiers_df[ah_tiers_df$key == i, "tier2c1"] == "Core" &
             ah_tiers_df[ah_tiers_df$key == i, "tier2c3"] == "Core" &
             ah_tiers_df[ah_tiers_df$key == i, "tier2b"] == "Core") {
    ah_tiers_df[ah_tiers_df$key == i, "tier2c"] <- "Core"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier2c"] <- "Precore"
  }
}




## Calculating tiers for Subcomponent 2d - 
##
##
## 

## Calculating tiers for 2d1
##
## "Yes" is assumed to be the same as "All staff"


for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "d1_astsop_training"] %in% c("Yes", "All staff")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier2d1"] <- "Core"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier2d1"] <- "Precore"
  }
}

## Calculating tiers for 2d2

for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "d2_ast_training_othersites"] %like any% c("%Yes%", "%yes%")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier2d2"] <- "Extended"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier2d2"] <- "Precore"
  }
}



###### Calculating overall tiers for 2d

## 2d1 required to achieve "Core", and 2d2 requird to achieve "Extended"



for (i in ah_all_df$key){
  if (ah_tiers_df[ah_tiers_df$key == i, "tier2d1"] == "Core" &
      ah_tiers_df[ah_tiers_df$key == i, "tier2d2"] == "Extended") {
    ah_tiers_df[ah_tiers_df$key == i, "tier2d"] <- "Extended"
  } else if (ah_tiers_df[ah_tiers_df$key == i, "tier2d1"] == "Core" &
             ah_tiers_df[ah_tiers_df$key == i, "tier2d2"] != "Extended") {
    ah_tiers_df[ah_tiers_df$key == i, "tier2d"] <- "Core"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier2d"] <- "Precore"
  }
}



############# Calculating tiers of component 3

## Calculating tiers for Subcomponent 3a - 
##
## 
## 


## Calculating tiers for 3a1

for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "a1_isolates_stored"] %in% c("In -80°C freezers", "Freeze dried")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier3a1"] <- "Extended"
  } else if (ah_all_df[ah_all_df$key == i, "a1_isolates_stored"] %in% c("In -20°C freezers", 
                                                                             "In -40°C freezers",
                                                                             "On agar slopes / agar stabs")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier3a1"] <- "Core"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier3a1"] <- "Precore"
  }
}

## Calculating tiers for 3a2

for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "a2_freezer_backupupsreliable"] %in% c("Yes")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier3a2"] <- "Extended"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier3a2"] <- "Precore"
  }
}

## Calculating tiers for 3a3

for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "a3_freezers_servicecontract"] %in% c("Yes")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier3a3"] <- "Extended"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier3a3"] <- "Precore"
  }
}

## Calculating tiers for 3a4

for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "a4_isolatespecimen_linkage"] %in% c("Linked via electronic database")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier3a4"] <- "Extended"
  } else if (ah_all_df[ah_all_df$key == i, "a4_isolatespecimen_linkage"] %in% c("Linked via paper records / logbooks")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier3a4"] <- "Core" 
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier3a4"] <- "Precore" 
  }
}


## Calculating tiers for 3a5

for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "a5_isolates_inventory"] %in% c("LIMS",
                                                                         "Formal biorepository software") |
      ah_all_df[ah_all_df$key == i, "a5_isolates_inventory"] %like any% c("%xcel%")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier3a5"] <- "Extended"
  } else if (ah_all_df[ah_all_df$key == i, "a5_isolates_inventory"] %in% c("Paper-based record / log book") |
             ah_all_df[ah_all_df$key == i, "a5_isolates_inventory"] %like% c("Yes%")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier3a5"] <- "Core" 
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier3a5"] <- "Precore" 
  }
}

##### Calculating overall tiers for 3a - 
##
## For 3a to achieve "Core", only 3a1 and 3a4 must be "Core" or higher.
## For "Extended" to be achieved, all four questions must be "Extended". Therefore,
## provided that 3a1 and 3a4 achieve at least "Core", if any of the other questions
## do not achieve "Extended", then 3a is "Core".

for (i in ah_all_df$key){
  if (ah_tiers_df[ah_tiers_df$key == i, "tier3a1"] == "Extended" &
      ah_tiers_df[ah_tiers_df$key == i, "tier3a2"] == "Extended" &
      ah_tiers_df[ah_tiers_df$key == i, "tier3a3"] == "Extended" &
      ah_tiers_df[ah_tiers_df$key == i, "tier3a4"] == "Extended" &
      ah_tiers_df[ah_tiers_df$key == i, "tier3a5"] == "Extended") {
    ah_tiers_df[ah_tiers_df$key == i, "tier3a"] <- "Extended"
  } else if ((ah_tiers_df[ah_tiers_df$key == i, "tier3a1"] != "Extended" |
              ah_tiers_df[ah_tiers_df$key == i, "tier3a4"] != "Extended" |
              ah_tiers_df[ah_tiers_df$key == i, "tier3a2"] != "Extended" |
              ah_tiers_df[ah_tiers_df$key == i, "tier3a3"] != "Extended" |
              ah_tiers_df[ah_tiers_df$key == i, "tier3a5"] != "Extended") &
             (ah_tiers_df[ah_tiers_df$key == i, "tier3a1"] != c("Precore") &
              ah_tiers_df[ah_tiers_df$key == i, "tier3a4"] != c("Precore") &
              ah_tiers_df[ah_tiers_df$key == i, "tier3a5"] != c("Precore"))) {
    ah_tiers_df[ah_tiers_df$key == i, "tier3a"] <- "Core"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier3a"] <- "Precore"
  }
}

## Calculating tiers for Subcomponent 3b - 
##
## 
## 3b1 is required for "Core", 3b2 is required for "Core" but also "Extended".


## Calculating tiers for 3b1

for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "b1_biobanking"] %in% c("Yes")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier3b1"] <- "Core"
  } else if (ah_all_df[ah_all_df$key == i, "b1_biobanking"] %in% c("Not applicable")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier3b1"] <- "Not applicable"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier3b1"] <- "Precore"
  }
}

## Calculating tiers for 3b2

for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "b2_biobanking_freq"] %in% c("Every 1-3 months")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier3b2"] <- "Extended"
  } else if (ah_all_df[ah_all_df$key == i, "b2_biobanking_freq"] %in% c("Once a year",
                                                                             "Every 3-6 months",
                                                                             "Yes")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier3b2"] <- "Core"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier3b2"] <- "Precore"
  }
}



##### Calculating overall tiers for 3b - 
##
## 3b
##

for (i in ah_all_df$key){
  if (ah_tiers_df[ah_tiers_df$key == i, "tier3b1"] == "Core" &
      ah_tiers_df[ah_tiers_df$key == i, "tier3b2"] == "Extended") {
    ah_tiers_df[ah_tiers_df$key == i, "tier3b"] <- "Extended"
  } else if (ah_tiers_df[ah_tiers_df$key == i, "tier3b1"] == "Core" &
             ah_tiers_df[ah_tiers_df$key == i, "tier3b2"] == "Core") {
    ah_tiers_df[ah_tiers_df$key == i, "tier3b"] <- "Core" 
  } else if (ah_tiers_df[ah_tiers_df$key == i, "tier3b1"] == "Not applicable") {
    ah_tiers_df[ah_tiers_df$key == i, "tier3b"] <- "Not applicable" 
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier3b"] <- "Precore" 
  }
}



## Calculating tiers for Subcomponent 3c - 
##
## 
##


## Calculating tiers for 3c1

for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "c1_isolatetransport_sop"] %like% c("Yes")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier3c1"] <- "Core"
  } else if (ah_all_df[ah_all_df$key == i, "c1_isolatetransport_sop"] %in% c("Not applicable")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier3c1"] <- "Not applicable"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier3c1"] <- "Precore"
  }
}

## Calculating tiers for 3c2

for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "c2_isolatetransport_training"] %like% c("All staff", "all staff", "Yes")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier3c2"] <- "Core"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier3c2"] <- "Precore"
  }
}

## Calculating tiers for 3c3

for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "c3_sampleStorageSOP"] %like% c("Yes")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier3c3"] <- "Core"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier3c3"] <- "Precore"
  }
}



##### Calculating overall tiers for 3c - 
##
## 
## Given that it may not be relevant for some sites to refer isolates to a 
## national reference site due to being a reference site themselves, if 
## "Not applicable" if given for tier3c1 or tier3c2, a site can still achieve
## "Core" or "Extended" as long as they have SOPs for isolate storage.

for (i in ah_all_df$key){
  if ((ah_tiers_df[ah_tiers_df$key == i, "tier3c1"] == "Core" &
       ah_tiers_df[ah_tiers_df$key == i, "tier3c2"] == "Core" &
       ah_tiers_df[ah_tiers_df$key == i, "tier3c3"] == "Core" &
       ah_tiers_df[ah_tiers_df$key == i, "tier1b5"] == "Extended")|
      (ah_tiers_df[ah_tiers_df$key == i, "tier3c1"] == "Not applicable" &
       ah_tiers_df[ah_tiers_df$key == i, "tier3c3"] == "Core" &
       ah_tiers_df[ah_tiers_df$key == i, "tier1b5"] == "Extended")
  ) {
    ah_tiers_df[ah_tiers_df$key == i, "tier3c"] <- "Extended"
  } else if ((ah_tiers_df[ah_tiers_df$key == i, "tier3c1"] == "Core" &
             ah_tiers_df[ah_tiers_df$key == i, "tier3c2"] == "Core"&
             ah_tiers_df[ah_tiers_df$key == i, "tier1b5"] == "Precore") |
             (ah_tiers_df[ah_tiers_df$key == i, "tier3c1"] == "Not applicable" &
              ah_tiers_df[ah_tiers_df$key == i, "tier3c3"] == "Core"&
              ah_tiers_df[ah_tiers_df$key == i, "tier1b5"] == "Precore")
             ) {
    ah_tiers_df[ah_tiers_df$key == i, "tier3c"] <- "Core" 
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier3c"] <- "Precore" 
  }
}


############# Calculating tiers of component 4

## Calculating tiers for Subcomponent 4a - 
##
## 
## 


## Calculating tiers for 4a1
##

for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "a1_datasubmitted_amrcc"] %in% c("Yes, anonymised")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier4a1"] <- "Core"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier4a1"] <- "Precore"
  }
}


## Calculating tiers for 4a2

for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "a2_datasubmitted_amrcc_freq"] %in% c("Daily automated submission")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier4a2"] <- "Advanced"
  } else if (ah_all_df[ah_all_df$key == i, "a2_datasubmitted_amrcc_freq"] %in% c("Annually",
                                                                                      "Annually/ad hoc",
                                                                                      "Every 3-6 months",
                                                                                      "Monthly",
                                                                                      "Weekly")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier4a2"] <- "Core"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier4a2"] <- "Precore"
  }
}


## Calculating tiers for 4a3

for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "a3_datasharing_guidelines"] %like% c("Yes")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier4a3"] <- "Core"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier4a3"] <- "Precore"
  }
}


## Calculating tiers for 4a4

for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "a4_datasharing_other"] %like% c("Yes")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier4a4"] <- "Core"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier4a4"] <- "Precore"
  }
}


##### Calculating overall tiers for 4a - 
##
## 
## There is no extended, requires 4a1-4a3 to be "Core" to achieve "Core",
## however if 4a2 achieves "Advanced" as well, "Advanced" tier achieved.

for (i in ah_all_df$key){
  if (ah_tiers_df[ah_tiers_df$key == i, "tier4a1"] == "Core" &
      ah_tiers_df[ah_tiers_df$key == i, "tier4a2"] == "Advanced" &
      ah_tiers_df[ah_tiers_df$key == i, "tier4a3"] == "Core" &
      ah_tiers_df[ah_tiers_df$key == i, "tier4a4"] == "Core") {
    ah_tiers_df[ah_tiers_df$key == i, "tier4a"] <- "Advanced"
  } else if (ah_tiers_df[ah_tiers_df$key == i, "tier4a1"] == "Core" &
             ah_tiers_df[ah_tiers_df$key == i, "tier4a2"] == "Core"&
             ah_tiers_df[ah_tiers_df$key == i, "tier4a3"] == "Core" &
             ah_tiers_df[ah_tiers_df$key == i, "tier4a4"] == "Core") {
    ah_tiers_df[ah_tiers_df$key == i, "tier4a"] <- "Core" 
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier4a"] <- "Precore" 
  }
}


## Calculating tiers for Subcomponent 4b - 
##
## 
## 


## Calculating tiers for 4b1
##

for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "b1_datalinkage"] %like% c("Automated linkage%")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier4b1"] <- "Advanced"
  } else if (ah_all_df[ah_all_df$key == i, "b1_datalinkage"] %like% c("Clinical and laboratory databases use same unique patient identifier and data can be retrieved by request to IT department")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier4b1"] <- "Extended"
  } else if (ah_all_df[ah_all_df$key == i, "b1_datalinkage"] %like% c("Paper linkage%",
                                                                           "Yes",
                                                                           "Linked manually")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier4b1"] <- "Core"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier4b1"] <- "Precore"
  }
}


## Calculating tiers for 4b2

for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "b2_uniqueid"] %like% c("Yes")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier4b2"] <- "Core"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier4b2"] <- "Precore"
  }
}



##### Calculating overall tiers for 4b - 
##
## 
## 


for (i in ah_all_df$key){
  if (ah_tiers_df[ah_tiers_df$key == i, "tier4b1"] == "Advanced" &
      ah_tiers_df[ah_tiers_df$key == i, "tier4b2"] == "Core" ) {
    ah_tiers_df[ah_tiers_df$key == i, "tier4b"] <- "Advanced"
  } else if (ah_tiers_df[ah_tiers_df$key == i, "tier4b1"] == "Extended" &
             ah_tiers_df[ah_tiers_df$key == i, "tier4b2"] == "Core") {
    ah_tiers_df[ah_tiers_df$key == i, "tier4b"] <- "Extended" 
  } else if (ah_tiers_df[ah_tiers_df$key == i, "tier4b1"] == "Core" &
             ah_tiers_df[ah_tiers_df$key == i, "tier4b2"] == "Core") {
    ah_tiers_df[ah_tiers_df$key == i, "tier4b"] <- "Core" 
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier4b"] <- "Precore" 
  }
}


## Calculating overall tiers for Subcomponent 4c - 
##
## 
##
## Only uses one question, 4c. Only core can be achieved.

for (i in ah_all_df$key){
  if (ah_all_df[ah_all_df$key == i, "c_datasharing_policy"] %like% c("Yes")) {
    ah_tiers_df[ah_tiers_df$key == i, "tier4c"] <- "Core"
  } else {
    ah_tiers_df[ah_tiers_df$key == i, "tier4c"] <- "Precore"
  }
}

## df_wide serves no purpose anymore, so remove
rm(df_wide)

## Remove the key column
ah_all_df <- select(ah_all_df, !(key))
ah_tiers_df <- select(ah_tiers_df, !(key))


############################# Producing helpful resources

## Save the human readable, wide format of all of the LSHTM roadmap questions
write_xlsx(ah_all_df, "Output files/AH/1a. AH site data - Human readable.xlsx")

## Save the file which contains "Precore", "Core", "Extended" and "Advanced" status
## calculations for each LSHTM Roadmap relevant AH site tool question.
write_xlsx(ah_tiers_df, "Output files/AH/1b. AH site status - All questions.xlsx")

# Note: Individual questions contribute to an overall subcomponent's status, in a
#       question/answer dependent manner. The level that each answer contributes to
#       is indicated in the '1b. AH Site Status - All questions.xlsx' spreadsheet.


############################# LSHTM Roadmap Status Mastersheet

tiers <- c("tier1a", "tier1b", "tier2a", "tier2b", "tier2c", "tier2d", "tier3a", "tier3b", "tier3c", "tier4a", "tier4b", "tier4c")

ah_tiers_df <- select(ah_tiers_df, c(1:5, all_of(tiers)))

## Next, row by row count all of the "values the core, extended and advanced values,
## and place them in their own rows.

ah_tiers_df <- ah_tiers_df %>%
  rowwise() %>%
  mutate(
    core = sum(c_across(starts_with("tier")) == "Core"),
    extended = sum(c_across(starts_with("tier")) == "Extended"),
    advanced = sum(c_across(starts_with("tier")) == "Advanced"),
    `core or above` = sum(c_across(c(core, extended,advanced)))
  ) %>%
  ungroup()

write_xlsx(ah_tiers_df, "Output files/AH/1. AH LSHTM Roadmap status.xlsx")
