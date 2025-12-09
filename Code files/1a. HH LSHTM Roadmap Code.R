############ HH LSHTM Roadmap Code
#######
#######
####### Created by Dr Jacob Wildfire as part of the Fleming Fund



####### This code was designed to collate the answers to the Human Health
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
df <- read.csv("DHIS2 exported files/HH_site_data_example.csv") # Replace with your DHIS2 HH site tool data .csv filepath

## Load in site masterlist to obtain site information:
site_info <- read_xlsx("Resource files/Site masterlist template.xlsx", sheet = "Sentinel Sites") # Replace with your site masterlist .xlsx filepath 



#################################### Generating necessary dataframes

## Generate a workbook into which raw data will be entered
hh_all_df <- data.frame(
  site = NA, sitecode = NA, type = NA, reporting.month = NA, `clinical_care` = NA,
  `1a1_guidelines` = NA, `1a2_algorithms` = NA, `1a3_proforma` = NA,
  `1b1_uniqueid` = NA, `1b1_gender` = NA, `1b1_age` = NA, `1b1_HospOrComm` = NA,
  `1b2_dataformat` = NA, `1c1_clindetails` = NA, `1c2_clindetailsrec` = NA, `1c3_clininvest` = NA, 
  `1d1_sursop_training` = NA, `1d2_qc_training` = NA, `1d3_participates_eqa` = NA,
  `1d4_provides_eqa` = NA, `2a1_transport_sop` = NA,
  `2a2_transop_training` = NA, `2a3_transop_followed` = NA, `2b_samplesubmitted` = NA,
  `2c1_bcs_automated` = NA, `2c2cerebrospinalfluid` = NA, `2c2urine` = NA,
  `2c2stool` = NA, `2c2swab` = NA, `2c2genit` = NA, `2c3streppnue` = NA,
  `2c3staphaur` = NA, `2c3ecoli` = NA, `2c3klebpnue` = NA, `2c3acinetobacter` = NA,
  `2c3salmonella` = NA, `2c3shigella` = NA, `2c3ngoNorrhoea` = NA,
  `2c3pseudom` = NA, `2c3styphi` = NA, `2c3spara` = NA, `2c3nmening` = NA,
  `2c4_bacid_automated` = NA, `2c5_bacid_system` = NA, `2d1_ast_system` = NA,
  `2d2_mic` = NA, `2d3_ast_sop` = NA, `2e1_astsop_training` = NA,
  `2e2_ast_training_othersites` = NA, `3a1_isolates_stored` = NA,
  `3a2_freezer_backupUPSreliable` = NA, `3a3_freezers_servicecontract` = NA,
  `3a4_isolates_inventory` = NA, `3b1_biobanking` = NA, `3b2_biobanking_freq` = NA,
  `3c1_isolatetransport_sop` = NA, `3c2_isolatetransport_training` = NA,
  `4a1_datasubmitted_amrcc` = NA, `4a2_datasubmitted_amrcc_freq` = NA,
  `4a3_datasharing_other` = NA, `4a3_datasharing_int` = NA,
  `4b_datalinkage` = NA, `4c_datasharing_policy` = NA, check.names = FALSE
)


## Generate a workbook into which tiers will be calculated
hh_tiers_df <- data.frame(
  site = NA, sitecode = NA, type = NA, reporting.month = NA, `clinical_care` = NA,
  tier1a1 = NA, tier1a2 = NA, tier1a3 = NA, tier1a = NA,
  tier1b1 = NA, tier1b2 = NA, tier1b = NA,
  tier1c1 = NA, tier1c2 = NA, tier1c3 = NA, tier1c = NA,
  tier1d1 = NA, tier1d2 = NA, tier1d3 = NA, tier1d4 = NA, tier1d = NA,
  tier2a1 = NA, tier2a2 = NA, tier2a3 = NA, tier2a = NA,
  tier2b = NA,
  tier2c1 = NA, tier2c2csf = NA, tier2c2urine = NA, tier2c2stool = NA,
  tier2c2swab = NA, tier2c2genit = NA,
  tier2c3strep = NA, tier2c3staph = NA, tier2c3ecoli = NA, tier2c3kleb = NA,
  tier2c3acine = NA, tier2c3salmonella = NA, tier2c3shigella = NA,
  tier2c3ngonor = NA, tier2c3pseudom = NA, tier2c3styphi = NA,
  tier2c3spara = NA, tier2c3nmening = NA,
  tier2c4 = NA, tier2c = NA,
  tier2d1 = NA, tier2d2 = NA, tier2d3 = NA, tier2d = NA,
  tier2e1 = NA, tier2e2 = NA, tier2e = NA,
  tier3a1 = NA, tier3a2 = NA, tier3a3 = NA, tier3a4 = NA, tier3a = NA,
  tier3b1 = NA, tier3b2 = NA, tier3b = NA,
  tier3c1 = NA, tier3c2 = NA, tier3c = NA,
  tier4a1 = NA, tier4a2 = NA, tier4a3local = NA, tier4a3int = NA, tier4a = NA,
  tier4b = NA, tier4c = NA, check.names = FALSE
)



#################################### Prepare relevant dataframes

## Identify LSHTM roadmap relevant dataelements:
dataelements <- c("hh_clinical_care",
                  "hh_1a1_guidelines",
                  "hh_1a2_algorithms",
                  "hh_1a3_proforma",
                  "hh_1b1_uniqueid",
                  "hh_1b1_gender",
                  "hh_1b1_age",
                  "hh_1b1_HospOrComm",
                  "hh_1b2_dataformat",
                  "hh_1c1_clininvest",
                  "hh_1c2_clindetails",
                  "hh_1c3_clindetailsrec",
                  "hh_2a1_sursop_training",
                  "hh_2a2_qc_training",
                  "hh_2a3_participates_eqa",
                  "hh_2a12_provides_eqa",
                  "hh_3a1_transport_sop",
                  "hh_3a2_transop_training",
                  "hh_3a3_transop_followed",
                  "hh_3b1_sampleregistered",
                  "hh_3c1_bcs_automated",
                  "hh_3c2cerebrospinalfluid",
                  "hh_3c3urine",
                  "hh_3c4stool",
                  "hh_3c5swab",
                  "hh_3c6genit",
                  "hh_3c7streppnue",
                  "hh_3c8staphaur",
                  "hh_3c9ecoli",
                  "hh_3c10klebpnue",
                  "hh_3c11acinetobacter",
                  "hh_3c12salmonella",
                  "hh_3c13shigella",
                  "hh_3c14ngonorrhoea",
                  "hh_3c15_bacid_automated",
                  "hh_3c16_bacid_system",
                  "hh_3c17_pseudomonas",
                  "hh_3c18_salmonella_typhi",
                  "hh_3c19_salmonella_para",
                  "hh_3c20_neisseria",
                  "hh_3d1_ast_system",
                  "hh_3d2_mic",
                  "hh_3d3_ast_sop",
                  "hh_3e1_astsop_training",
                  "hh_3e2_ast_training_othersites",
                  "hh_4a1_isolates_stored",
                  "hh_4a2_freezer_backupUPSreliable",
                  "hh_4a3_freezers_servicecontract",
                  "hh_4a4_isolates_inventory",
                  "hh_4b1_biobanking",
                  "hh_4b2_biobanking_freq",
                  "hh_4c1_isolatetransport_sop",
                  "hh_4c2_isolatetransport_training",
                  "hh_5a1_datasubmitted_amrcc",
                  "hh_5a2_datasubmitted_amrcc_freq",
                  "hh_5a6_datasharing", 
                  "hh_5a7_datasharing_other",
                  "hh_5b_datalinkage",
                  "hh_5c_datasharing_policy")

## Convert the df$lastupdated column to only the first 10 characters to get the date:
df <- df %>%
  mutate(lastupdated = str_sub(lastupdated, 1, 10))

## Remove all of the non-relevant SP&M dataelements
df <- df[df$dataelement %in% dataelements,]

## Remove useless rows and remove all orgunit characters after the first 5 letters:
df <- select(df,c(1:3,6)) %>%
  mutate(orgunit = str_sub(orgunit, 1, 5))

## Make df "orgunit" into "sitecode"
colnames(df) <- c("dataelement", "reporting.month", "sitecode", "value")


#################################### Mapping DHIS2 values to hh_all_df

## Transform df to a wide format
df_wide <- df %>%
  mutate(dataelement = str_replace(dataelement, "hh_", "")) %>%
  pivot_wider(names_from = dataelement, values_from = value)


## Create an adequate number of blank rows (one per site per report period)
## add blank rows.
blank_rows <- data.frame(matrix(ncol = ncol(hh_all_df), nrow = nrow(df_wide)))
colnames(blank_rows) <- colnames(hh_all_df) # Ensure the column names are consistent

hh_all_df <- blank_rows # Bind the blank rows to the empty hh_all_df dataframe

rm(blank_rows) # Remove the now not needed blank_rows

## Stick the sitecodes and reporting.month values from the df_wide dataframe.
hh_all_df$`sitecode` <- df_wide$`sitecode`
hh_all_df$`reporting.month`<- df_wide$reporting.month

## Generate a "key" column that can be used to quickly run site/date loops simultaneously:
df_wide$key <- paste(df_wide$sitecode, df_wide$reporting.month, sep = "_")
hh_all_df$key <- df_wide$key

## Locate the relevant data from the site info list and insert it into the df
for (i in hh_all_df$`sitecode`) {
  hh_all_df[hh_all_df$`sitecode` == i, "site"] <- site_info[site_info$`Site Code` == i, "Laboratory"]
  hh_all_df[hh_all_df$`sitecode` == i, "type"] <- site_info[site_info$`Site Code` == i, "Type"]
}



## Find the relevant values and plug them into the hh_all_df
for (i in df_wide$key) {
  
  if (!is.na(df_wide[df_wide$key == i, "clinical_care"])) {
    hh_all_df[hh_all_df$key == i, "clinical_care"] <- df_wide[df_wide$key == i, "clinical_care"]
  }
  
  if (!is.na(df_wide[df_wide$key == i, "1a1_guidelines"])) {
    hh_all_df[hh_all_df$key == i, "1a1_guidelines"] <- df_wide[df_wide$key == i, "1a1_guidelines"]
  }
  
  if (!is.na(df_wide[df_wide$key == i, "1a2_algorithms"])) {
    hh_all_df[hh_all_df$key == i, "1a2_algorithms"] <- df_wide[df_wide$key == i, "1a2_algorithms"]
  }
  if (!is.na(df_wide[df_wide$key == i, "1a3_proforma"])) {
    hh_all_df[hh_all_df$key == i, "1a3_proforma"] <- df_wide[df_wide$key == i, "1a3_proforma"]
  }
  if (!is.na(df_wide[df_wide$key == i, "1b1_uniqueid"])) {
    hh_all_df[hh_all_df$key == i, "1b1_uniqueid"] <- df_wide[df_wide$key == i, "1b1_uniqueid"]
  }
  if (!is.na(df_wide[df_wide$key == i, "1b1_gender"])) {
    hh_all_df[hh_all_df$key == i, "1b1_gender"] <- df_wide[df_wide$key == i, "1b1_gender"]
  }
  if (!is.na(df_wide[df_wide$key == i, "1b1_age"])) {
    hh_all_df[hh_all_df$key == i, "1b1_age"] <- df_wide[df_wide$key == i, "1b1_age"]
  }
  if (!is.na(df_wide[df_wide$key == i, "1b1_HospOrComm"])) {
    hh_all_df[hh_all_df$key == i, "1b1_HospOrComm"] <- df_wide[df_wide$key == i, "1b1_HospOrComm"]
  }
  if (!is.na(df_wide[df_wide$key == i, "1b2_dataformat"])) {
    hh_all_df[hh_all_df$key == i, "1b2_dataformat"] <- df_wide[df_wide$key == i, "1b2_dataformat"]
  }
  if (!is.na(df_wide[df_wide$key == i, "1c1_clininvest"])) {
    hh_all_df[hh_all_df$key == i, "1c3_clininvest"] <- df_wide[df_wide$key == i, "1c1_clininvest"]
  }
  if (!is.na(df_wide[df_wide$key == i, "1c2_clindetails"])) {
    hh_all_df[hh_all_df$key == i, "1c1_clindetails"] <- df_wide[df_wide$key == i, "1c2_clindetails"]
  }
  if (!is.na(df_wide[df_wide$key == i, "1c3_clindetailsrec"])) {
    hh_all_df[hh_all_df$key == i, "1c2_clindetailsrec"] <- df_wide[df_wide$key == i, "1c3_clindetailsrec"]
  }
  if (!is.na(df_wide[df_wide$key == i, "2a1_sursop_training"])) {
    hh_all_df[hh_all_df$key == i, "1d1_sursop_training"] <- df_wide[df_wide$key == i, "2a1_sursop_training"]
  }
  if (!is.na(df_wide[df_wide$key == i, "2a2_qc_training"])) {
    hh_all_df[hh_all_df$key == i, "1d2_qc_training"] <- df_wide[df_wide$key == i, "2a2_qc_training"]
  }
  if (!is.na(df_wide[df_wide$key == i, "2a3_participates_eqa"])) {
    hh_all_df[hh_all_df$key == i, "1d3_participates_eqa"] <- df_wide[df_wide$key == i, "2a3_participates_eqa"]
  }
  if (!is.na(df_wide[df_wide$key == i, "2a12_provides_eqa"])) {
    hh_all_df[hh_all_df$key == i, "1d4_provides_eqa"] <- df_wide[df_wide$key == i, "2a12_provides_eqa"]
  }
  if (!is.na(df_wide[df_wide$key == i, "3a1_transport_sop"])) {
    hh_all_df[hh_all_df$key == i, "2a1_transport_sop"] <- df_wide[df_wide$key == i, "3a1_transport_sop"]
  }
  if (!is.na(df_wide[df_wide$key == i, "3a2_transop_training"])) {
    hh_all_df[hh_all_df$key == i, "2a2_transop_training"] <- df_wide[df_wide$key == i, "3a2_transop_training"]
  }
  if (!is.na(df_wide[df_wide$key == i, "3a3_transop_followed"])) {
    hh_all_df[hh_all_df$key == i, "2a3_transop_followed"] <- df_wide[df_wide$key == i, "3a3_transop_followed"]
  }
  if (!is.na(df_wide[df_wide$key == i, "3b1_sampleregistered"])) {
    hh_all_df[hh_all_df$key == i, "2b_samplesubmitted"] <- df_wide[df_wide$key == i, "3b1_sampleregistered"]
  }
  if (!is.na(df_wide[df_wide$key == i, "3c1_bcs_automated"])) {
    hh_all_df[hh_all_df$key == i, "2c1_bcs_automated"] <- df_wide[df_wide$key == i, "3c1_bcs_automated"]
  }
  if (!is.na(df_wide[df_wide$key == i, "3c2cerebrospinalfluid"])) {
    hh_all_df[hh_all_df$key == i, "2c2cerebrospinalfluid"] <- df_wide[df_wide$key == i, "3c2cerebrospinalfluid"]
  }
  if (!is.na(df_wide[df_wide$key == i, "3c3urine"])) {
    hh_all_df[hh_all_df$key == i, "2c2urine"] <- df_wide[df_wide$key == i, "3c3urine"]
  }
  if (!is.na(df_wide[df_wide$key == i, "3c4stool"])) {
    hh_all_df[hh_all_df$key == i, "2c2stool"] <- df_wide[df_wide$key == i, "3c4stool"]
  }
  if (!is.na(df_wide[df_wide$key == i, "3c5swab"])) {
    hh_all_df[hh_all_df$key == i, "2c2swab"] <- df_wide[df_wide$key == i, "3c5swab"]
  }
  if (!is.na(df_wide[df_wide$key == i, "3c6genit"])) {
    hh_all_df[hh_all_df$key == i, "2c2genit"] <- df_wide[df_wide$key == i, "3c6genit"]
  }
  if (!is.na(df_wide[df_wide$key == i, "3c7streppnue"])) {
    hh_all_df[hh_all_df$key == i, "2c3streppnue"] <- df_wide[df_wide$key == i, "3c7streppnue"]
  }
  if (!is.na(df_wide[df_wide$key == i, "3c8staphaur"])) {
    hh_all_df[hh_all_df$key == i, "2c3staphaur"] <- df_wide[df_wide$key == i, "3c8staphaur"]
  }
  if (!is.na(df_wide[df_wide$key == i, "3c9ecoli"])) {
    hh_all_df[hh_all_df$key == i, "2c3ecoli"] <- df_wide[df_wide$key == i, "3c9ecoli"]
  }
  if (!is.na(df_wide[df_wide$key == i, "3c10klebpnue"])) {
    hh_all_df[hh_all_df$key == i, "2c3klebpnue"] <- df_wide[df_wide$key == i, "3c10klebpnue"]
  }
  if (!is.na(df_wide[df_wide$key == i, "3c11acinetobacter"])) {
    hh_all_df[hh_all_df$key == i, "2c3acinetobacter"] <- df_wide[df_wide$key == i, "3c11acinetobacter"]
  }
  if (!is.na(df_wide[df_wide$key == i, "3c12salmonella"])) {
    hh_all_df[hh_all_df$key == i, "2c3salmonella"] <- df_wide[df_wide$key == i, "3c12salmonella"]
  }
  if (!is.na(df_wide[df_wide$key == i, "3c13shigella"])) {
    hh_all_df[hh_all_df$key == i, "2c3shigella"] <- df_wide[df_wide$key == i, "3c13shigella"]
  }
  if (!is.na(df_wide[df_wide$key == i, "3c14ngonorrhoea"])) {
    hh_all_df[hh_all_df$key == i, "2c3ngoNorrhoea"] <- df_wide[df_wide$key == i, "3c14ngonorrhoea"]
  }
  if (!is.na(df_wide[df_wide$key == i, "3c15_bacid_automated"])) {
    hh_all_df[hh_all_df$key == i, "2c4_bacid_automated"] <- df_wide[df_wide$key == i, "3c15_bacid_automated"]
  }
  if (!is.na(df_wide[df_wide$key == i, "3c16_bacid_system"])) {
    hh_all_df[hh_all_df$key == i, "2c5_bacid_system"] <- df_wide[df_wide$key == i, "3c16_bacid_system"]
  }
  if (!is.na(df_wide[df_wide$key == i, "3c17_pseudomonas"])) {
    hh_all_df[hh_all_df$key == i, "2c3pseudom"] <- df_wide[df_wide$key == i, "3c17_pseudomonas"]
  }
  if (!is.na(df_wide[df_wide$key == i, "3c18_salmonella_typhi"])) {
    hh_all_df[hh_all_df$key == i, "2c3styphi"] <- df_wide[df_wide$key == i, "3c18_salmonella_typhi"]
  }
  if (!is.na(df_wide[df_wide$key == i, "3c19_salmonella_para"])) {
    hh_all_df[hh_all_df$key == i, "2c3spara"] <- df_wide[df_wide$key == i, "3c19_salmonella_para"]
  }
  if (!is.na(df_wide[df_wide$key == i, "3c20_neisseria"])) {
    hh_all_df[hh_all_df$key == i, "2c3nmening"] <- df_wide[df_wide$key == i, "3c20_neisseria"]
  }
  if (!is.na(df_wide[df_wide$key == i, "3d1_ast_system"])) {
    hh_all_df[hh_all_df$key == i, "2d1_ast_system"] <- df_wide[df_wide$key == i, "3d1_ast_system"]
  }
  if (!is.na(df_wide[df_wide$key == i, "3d2_mic"])) {
    hh_all_df[hh_all_df$key == i, "2d2_mic"] <- df_wide[df_wide$key == i, "3d2_mic"]
  }
  if (!is.na(df_wide[df_wide$key == i, "3d3_ast_sop"])) {
    hh_all_df[hh_all_df$key == i, "2d3_ast_sop"] <- df_wide[df_wide$key == i, "3d3_ast_sop"]
  }
  if (!is.na(df_wide[df_wide$key == i, "3e1_astsop_training"])) {
    hh_all_df[hh_all_df$key == i, "2e1_astsop_training"] <- df_wide[df_wide$key == i, "3e1_astsop_training"]
  }
  if (!is.na(df_wide[df_wide$key == i, "3e2_ast_training_othersites"])) {
    hh_all_df[hh_all_df$key == i, "2e2_ast_training_othersites"] <- df_wide[df_wide$key == i, "3e2_ast_training_othersites"]
  }
  if (!is.na(df_wide[df_wide$key == i, "4a1_isolates_stored"])) {
    hh_all_df[hh_all_df$key == i, "3a1_isolates_stored"] <- df_wide[df_wide$key == i, "4a1_isolates_stored"]
  }
  if (!is.na(df_wide[df_wide$key == i, "4a2_freezer_backupUPSreliable"])) {
    hh_all_df[hh_all_df$key == i, "3a2_freezer_backupUPSreliable"] <- df_wide[df_wide$key == i, "4a2_freezer_backupUPSreliable"]
  }
  if (!is.na(df_wide[df_wide$key == i, "4a3_freezers_servicecontract"])) {
    hh_all_df[hh_all_df$key == i, "3a3_freezers_servicecontract"] <- df_wide[df_wide$key == i, "4a3_freezers_servicecontract"]
  }
  if (!is.na(df_wide[df_wide$key == i, "4a4_isolates_inventory"])) {
    hh_all_df[hh_all_df$key == i, "3a4_isolates_inventory"] <- df_wide[df_wide$key == i, "4a4_isolates_inventory"]
  }
  if (!is.na(df_wide[df_wide$key == i, "4b1_biobanking"])) {
    hh_all_df[hh_all_df$key == i, "3b1_biobanking"] <- df_wide[df_wide$key == i, "4b1_biobanking"]
  }
  if (!is.na(df_wide[df_wide$key == i, "4b2_biobanking_freq"])) {
    hh_all_df[hh_all_df$key == i, "3b2_biobanking_freq"] <- df_wide[df_wide$key == i, "4b2_biobanking_freq"]
  }
  if (!is.na(df_wide[df_wide$key == i, "4c1_isolatetransport_sop"])) {
    hh_all_df[hh_all_df$key == i, "3c1_isolatetransport_sop"] <- df_wide[df_wide$key == i, "4c1_isolatetransport_sop"]
  }
  if (!is.na(df_wide[df_wide$key == i, "4c2_isolatetransport_training"])) {
    hh_all_df[hh_all_df$key == i, "3c2_isolatetransport_training"] <- df_wide[df_wide$key == i, "4c2_isolatetransport_training"]
  }
  if (!is.na(df_wide[df_wide$key == i, "5a1_datasubmitted_amrcc"])) {
    hh_all_df[hh_all_df$key == i, "4a1_datasubmitted_amrcc"] <- df_wide[df_wide$key == i, "5a1_datasubmitted_amrcc"]
  }
  if (!is.na(df_wide[df_wide$key == i, "5a2_datasubmitted_amrcc_freq"])) {
    hh_all_df[hh_all_df$key == i, "4a2_datasubmitted_amrcc_freq"] <- df_wide[df_wide$key == i, "5a2_datasubmitted_amrcc_freq"]
  }
  if (!is.na(df_wide[df_wide$key == i, "5a6_datasharing"])) {
    hh_all_df[hh_all_df$key == i, "4a3_datasharing_other"] <- df_wide[df_wide$key == i, "5a6_datasharing"]
  }
  if (!is.na(df_wide[df_wide$key == i, "5a7_datasharing_other"])) {
    hh_all_df[hh_all_df$key == i, "4a3_datasharing_int"] <- df_wide[df_wide$key == i, "5a7_datasharing_other"]
  }
  if (!is.na(df_wide[df_wide$key == i, "5b_datalinkage"])) {
    hh_all_df[hh_all_df$key == i, "4b_datalinkage"] <- df_wide[df_wide$key == i, "5b_datalinkage"]
  }
  if (!is.na(df_wide[df_wide$key == i, "5c_datasharing_policy"])) {
    hh_all_df[hh_all_df$key == i, "4c_datasharing_policy"] <- df_wide[df_wide$key == i, "5c_datasharing_policy"]
  }
  
}


## For those that remain that have NA values, change NA to "Please choose", indicating
## that no option was put. If there are NAs, they may disrupt the if/for loops below.
hh_all_df <- hh_all_df %>% 
  mutate(across(c(6:ncol(hh_all_df)), ~tidyr::replace_na(.x, "Please choose")))



## Add additional to hh_tiers_df so that it matches hh_all_df

if ((nrow(hh_all_df)-nrow(hh_tiers_df)) > 0) {
  row_diff <- nrow(hh_all_df)-nrow(hh_tiers_df)
  # Create a dataframe with 'row_diff' number of blank rows
  blank_rows <- data.frame(matrix(ncol = ncol(hh_tiers_df), nrow = row_diff))
  colnames(blank_rows) <- colnames(hh_tiers_df)
  
  # Bind the blank rows to df2
  hh_tiers_df <- rbind(hh_tiers_df, blank_rows)
}
rm(blank_rows, row_diff)

## Set the tiers dataset sitecode and reporting.month to be the same as hh_all_df
hh_tiers_df$sitecode <- hh_all_df$`sitecode`
hh_tiers_df$site <- hh_all_df$site
hh_tiers_df$type <- hh_all_df$type
hh_tiers_df$reporting.month <- hh_all_df$`reporting.month`
hh_tiers_df$key <- hh_all_df$key

######################




################################## Calculating tiers for each site

## First, change all "true" values to "Yes", and all "false" values to "No"

hh_all_df <- hh_all_df %>%
  mutate_if(is.character, ~ ifelse(. == "true", "Yes", .))


hh_all_df <- hh_all_df %>%
  mutate_if(is.character, ~ ifelse(. == "false", "No", .))




## Calculate subcomponent 1

for (i in  hh_all_df$key){
  ## If a site has reported that they do not provide clinical care to patients,
  ## they should therefore should either have left component 1 blank, or should
  ## have entered "Not applicable" for all answers within this section.
  
  ## This section of code therefore sets the answer to "Not applicable" for all
  ## sites not providing clinical care to patients for all component
  ## 1 questions.
  
  if (hh_all_df[hh_all_df$key == i, "clinical_care"] == c("No - do not complete component 1; skip to component 2") & 
      !is.na(hh_all_df[hh_all_df$key == i, "clinical_care"])) {
    hh_all_df[hh_all_df$key == i, c("1a1_guidelines",
                                    "1a2_algorithms",
                                    "1a3_proforma",
                                    "1b1_uniqueid",
                                    "1b1_gender",
                                    "1b1_age",
                                    "1b1_HospOrComm",
                                    "1b2_dataformat",
                                    "1c3_clininvest",
                                    "1c2_clindetailsrec",
                                    "1c1_clindetails")] <- "Not applicable"
  } 
  
  
  ## Assign the value clinical care column in the tiers column. 
  hh_tiers_df[hh_tiers_df$key == i,"clinical_care"] <- hh_all_df[hh_all_df$key == i, "clinical_care"]
  
  
  
  if ( hh_all_df[ hh_all_df$key == i, "1a1_guidelines"] %in% c("Yes")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1a1"] <- "Core"
  } else if ( hh_all_df[ hh_all_df$key == i, "1a1_guidelines"] %in% c("Not applicable")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1a1"] <- "Not applicable" 
  } else { 
     hh_tiers_df[ hh_tiers_df$key == i, "tier1a1"] <- "Precore"}
}



for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "1a2_algorithms"] %in% c("Yes")){
     hh_tiers_df[ hh_tiers_df$key == i, "tier1a2"] <- "Extended"
  } else if ( hh_all_df[ hh_all_df$key == i, "1a2_algorithms"] %in% c("Not applicable")) {
    hh_tiers_df[ hh_tiers_df$key == i, "tier1a2"] <- "Not applicable" 
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1a2"] <- "Precore"
  }
}


for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "1a3_proforma"] %in% c("Yes")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1a3"] <- "Advanced"
  } else if ( hh_all_df[ hh_all_df$key == i, "1a3_proforma"] %in% c("Not applicable")) {
    hh_tiers_df[ hh_tiers_df$key == i, "tier1a3"] <- "Not applicable" 
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1a3"] <- "Precore"
  }
}

##### The tier of 1a can then be calculated by determining the highest continual,
## unbroken achievement of 1a1 to 1a3. However, if 1b1 is "Not applicable" then
## "Not applicable" is set as the tier.

for (i in  hh_all_df$key){
  if ( hh_tiers_df[ hh_tiers_df$key == i, "tier1a1"] == "Core" &
       hh_tiers_df[ hh_tiers_df$key == i, "tier1a2"] == "Extended" &
       hh_tiers_df[ hh_tiers_df$key == i, "tier1a3"] == "Advanced") {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1a"] <- "Advanced"
  } else if ( hh_tiers_df[ hh_tiers_df$key == i, "tier1a1"] == "Core" &
              hh_tiers_df[ hh_tiers_df$key == i, "tier1a2"] == "Extended" &
              hh_tiers_df[ hh_tiers_df$key == i, "tier1a3"] == "Precore") {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1a"] <- "Extended"
  } else if ( hh_tiers_df[ hh_tiers_df$key == i, "tier1a1"] == "Core" &
              hh_tiers_df[ hh_tiers_df$key == i, "tier1a2"] == "Precore") {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1a"] <- "Core" 
  } else if ( hh_tiers_df[ hh_tiers_df$key == i, "tier1a1"] == "Not applicable"){
     hh_tiers_df[ hh_tiers_df$key == i, "tier1a"] <- "Not applicable" 
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1a"] <- "Precore"
  }
}


## Calculating tiers for Subcomponent 1b - 
## Subcomponent 1b1 calculates whether "Core" has been achieved, whereas 
## subcomponent 1b2 determines whether "Extended" or "Advanced" has been 
## achieved based on its response. However, it also can determine whether 
## a site is "Precore", as 1b1 looks at whether a unique identifier is applied 
## (a prerequisite of "Core"), however 1b2 looks at collection of clinical data
## (also a prerequisite of "Core")

## Calculating tiers for 1b1

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "1b1_uniqueid"] %in% c("Yes")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1b1"] <- "Core"
  } else if ( hh_all_df[ hh_all_df$key == i, "1b1_uniqueid"] %in% c("Not applicable")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1b1"] <- "Not applicable" 
  } else { 
     hh_tiers_df[ hh_tiers_df$key == i, "tier1b1"] <- "Precore"}
}

## Calculating tiers for 1b2

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "1b2_dataformat"] %like% c("Paperless system with multiple data sources%",
                                                                       "Linkage of extended clinical data%")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1b2"] <- "Advanced"
  } else if ( hh_all_df[ hh_all_df$key == i, "1b2_dataformat"] %in% c("Clinical data entered into hospital database and linked to laboratory data by unique alphanumeric number",
                                                                              "Clinical data included in electronic request for laboratory investigation, with unique identifier")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1b2"] <- "Extended" 
  } else if ( hh_all_df[ hh_all_df$key == i, "1b2_dataformat"] %like% c("Clinical data included in paper request for laboratory investigation, with unique identifier",
                                                                              "Other%",
                                                                              "Clinical information included on paper request form")) { 
     hh_tiers_df[ hh_tiers_df$key == i, "tier1b2"] <- "Core" 
  } else if ( hh_all_df[ hh_all_df$key == i, "1b2_dataformat"] %in% c("Not applicable")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1b2"] <- "Not applicable" 
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1b2"] <- "Precore" 
  }
}

##### Calculating overall tiers for 1b

for (i in  hh_all_df$key){
  if ( hh_tiers_df[ hh_tiers_df$key == i, "tier1b1"] == "Core" &
       hh_tiers_df[ hh_tiers_df$key == i, "tier1b2"] == "Advanced"){
     hh_tiers_df[ hh_tiers_df$key == i, "tier1b"] <- "Advanced"
  } else if ( hh_tiers_df[ hh_tiers_df$key == i, "tier1b1"] == "Core" &
              hh_tiers_df[ hh_tiers_df$key == i, "tier1b2"] == "Extended") {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1b"] <- "Extended"
  } else if ( hh_tiers_df[ hh_tiers_df$key == i, "tier1b1"] == "Not applicable" |
              hh_tiers_df[ hh_tiers_df$key == i, "tier1b2"] == "Not applicable")  {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1b"] <- "Not applicable"
  } else if ( hh_tiers_df[ hh_tiers_df$key == i, "tier1b1"] == "Core" &
              hh_tiers_df[ hh_tiers_df$key == i, "tier1b2"] == "Core") {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1b"] <- "Core"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1b"] <- "Precore"
  }
}


## Calculating tiers for Subcomponent 1c - 

## Calculating tiers for 1c1

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "1c1_clindetails"] %in% c("Yes")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1c1"] <- "Core"
  } else if ( hh_all_df[ hh_all_df$key == i, "1c1_clindetails"] %in% c("Not applicable")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1c1"] <- "Not applicable"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1c1"] <- "Precore"
  }
}

## Calculating tiers for 1c2

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "1c2_clindetailsrec"] %like% c("Recorded as a standardised case definition%")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1c2"] <- "Extended"
  } else if ( hh_all_df[ hh_all_df$key == i, "1c2_clindetailsrec"] %in% c("Chosen from a checklist of syndromes", "Free text")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1c2"] <- "Core"
  } else if ( hh_all_df[ hh_all_df$key == i, "1c2_clindetailsrec"] %in% c("Not applicable")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1c2"] <- "Not applicable"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1c2"] <- "Precore"
  }
}

## Calculating tiers for 1c3

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "1c3_clininvest"] %in% c("Systematic investigation based on clinician syndromic diagnosis")) {
    hh_tiers_df[ hh_tiers_df$key == i, "tier1c3"] <- "Extended"
  } else if ( hh_all_df[ hh_all_df$key == i, "1c3_clininvest"] %like% c("Systematic investigation based on clinical findings",
                                                                              "Other%")) {
    hh_tiers_df[ hh_tiers_df$key == i, "tier1c3"] <- "Core"
  } else if ( hh_all_df[ hh_all_df$key == i, "1c3_clininvest"] %in% c("Not applicable")) {
    hh_tiers_df[ hh_tiers_df$key == i, "tier1c3"] <- "Not applicable"
  } else {
    hh_tiers_df[ hh_tiers_df$key == i, "tier1c3"] <- "Precore"
  }
}

##### Calculating overall tiers for 1c

## Note this script also generates a list called "flagged_1c". This list contains all of 
## the sites that reported "Not applicable" for 1c2 despite recording that they
## record clinical data in 1c1. These sites should be looked at further.

for (i in  hh_all_df$key){
  if (as.numeric(substr(hh_all_df[hh_all_df$key == i,"reporting.month"], 1, 4))>2023) {
    if ( hh_tiers_df[ hh_tiers_df$key == i, "tier1c3"] == "Extended"){
      hh_tiers_df[ hh_tiers_df$key == i, "tier1c"] <- "Extended"
    } else if ( hh_tiers_df[ hh_tiers_df$key == i, "tier1c3"] == "Core") {
      hh_tiers_df[ hh_tiers_df$key == i, "tier1c"] <- "Core"
    } else if ( hh_tiers_df[ hh_tiers_df$key == i, "tier1c1"] == "Not applicable") {
      hh_tiers_df[ hh_tiers_df$key == i, "tier1c"] <- "Not applicable"
    } else {
      hh_tiers_df[ hh_tiers_df$key == i, "tier1c"] <- "Precore"
    }
  } else {
    if (hh_tiers_df[hh_tiers_df$key == i, "tier1c1"] == "Core" &
        hh_tiers_df[hh_tiers_df$key == i, "tier1c2"] == "Extended"){
      hh_tiers_df[hh_tiers_df$key == i, "tier1c"] <- "Extended"
    } else if (hh_tiers_df[hh_tiers_df$key == i, "tier1c1"] == "Core" &
               hh_tiers_df[hh_tiers_df$key == i, "tier1c2"] == "Core") {
      hh_tiers_df[hh_tiers_df$key == i, "tier1c"] <- "Core"
    } else if (hh_tiers_df[hh_tiers_df$key == i, "tier1c1"] == "Not applicable") {
      hh_tiers_df[hh_tiers_df$key == i, "tier1c"] <- "Not applicable"
    } else {
      hh_tiers_df[hh_tiers_df$key == i, "tier1c"] <- "Precore"
    }
  }
}


## Calculating tiers for Subcomponent 1d - 
## 1d1 and 1d2 are essential for "Core" function, whereas 1d3 is necessary for "Extended",
## and 1d4 is necessary for "Advanced"

## Calculating tiers for 1d1


for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "1d1_sursop_training"] %in% c("All staff")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1d1"] <- "Core"
     } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1d1"] <- "Precore"
  }
}

## Calculating tiers for 1d2


for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "1d2_qc_training"] %in% c("All staff")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1d2"] <- "Core"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1d2"] <- "Precore"
  }
}

## Calculating tiers for 1d3

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "1d3_participates_eqa"] %like% c("Yes%")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1d3"] <- "Extended"
 } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1d3"] <- "Precore"
  }
}

## Calculating tiers for 1d4

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "1d4_provides_eqa"] %like% c("Yes")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1d4"] <- "Advanced"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1d4"] <- "Precore"
  }
}


##### Calculating overall tiers for 1d

for (i in  hh_all_df$key){
  if ( hh_tiers_df[ hh_tiers_df$key == i, "tier1d1"] == "Core" &
       hh_tiers_df[ hh_tiers_df$key == i, "tier1d2"] == "Core" &
       hh_tiers_df[ hh_tiers_df$key == i, "tier1d3"] == "Extended" &
       hh_tiers_df[ hh_tiers_df$key == i, "tier1d4"] == "Advanced") {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1d"] <- "Advanced"
  } else if ( hh_tiers_df[ hh_tiers_df$key == i, "tier1d1"] == "Core" &
              hh_tiers_df[ hh_tiers_df$key == i, "tier1d2"] == "Core" &
              hh_tiers_df[ hh_tiers_df$key == i, "tier1d3"] == "Extended" &
              hh_tiers_df[ hh_tiers_df$key == i, "tier1d4"] == "Precore") {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1d"] <- "Extended"
  } else if ( hh_tiers_df[ hh_tiers_df$key == i, "tier1d1"] == "Core" &
              hh_tiers_df[ hh_tiers_df$key == i, "tier1d2"] == "Core") {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1d"] <- "Core" 
 } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier1d"] <- "Precore"
  }
}


############# Calculating tiers of component 2

## Calculating tiers for Subcomponent 2a - 
##

## Calculating tiers for 2a1

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "2a1_transport_sop"] %in% c("Yes")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2a1"] <- "Core"
  } else if ( hh_all_df[ hh_all_df$key == i, "2a1_transport_sop"] %in% c("Not applicable")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2a1"] <- "Not applicable"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2a1"] <- "Precore"
  }
}

## Calculating tiers for 2a2

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "2a2_transop_training"] %in% c("All staff")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2a2"] <- "Core"
  } else { 
     hh_tiers_df[ hh_tiers_df$key == i, "tier2a2"] <- "Precore"}
}

## Calculating tiers for 2a3

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "2a3_transop_followed"] %in% c("Yes")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2a3"] <- "Core"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2a3"] <- "Precore"}
}


###### Calculating overall tiers for 2a


for (i in  hh_all_df$key){
  if ( hh_tiers_df[ hh_tiers_df$key == i, "tier2a1"] == "Core" &
       hh_tiers_df[ hh_tiers_df$key == i, "tier2a2"] == "Core" &
       hh_tiers_df[ hh_tiers_df$key == i, "tier2a3"] == "Core") {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2a"] <- "Extended"
  } else if ( hh_tiers_df[ hh_tiers_df$key == i, "tier2a1"] == "Not applicable") {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2a"] <- "Not applicable"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2a"] <- "Precore"
  }
}



###### Calculating overall tiers for 2b
##
##
##
## Only uses one question, 2b

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "2b_samplesubmitted"] %in% c("Electronic requesting system allowing linkage of specimen details with patient information",
                                                                           "Electronic laboratory data system")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2b"] <- "Extended"
  } else if ( hh_all_df[ hh_all_df$key == i, "2b_samplesubmitted"] %in% c("With a paper form giving patient information",
                                                                                  "Local laboratory paper based data system",
                                                                                  "Other (please describe briefly)")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2b"] <- "Core"
  } else if (is.na( hh_all_df[ hh_all_df$key == i, "2b_samplesubmitted"])) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2b"] <- "Precore"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2b"] <- "Precore"
  }
}

## Calculating tiers for Subcomponent 2c - 
##
## 

## Calculating tiers for Subcomponent 2c1 - 
## "Core" is calculated as long for "Working blood culture instrument in place 
## and under a service contract". 
## "Not applicable" is calculated for "Laboratory is not performing blood cultures".
## Everything else is "Precore".


for (i in  hh_all_df$key){
  if (hh_all_df[ hh_all_df$key == i, "2c1_bcs_automated"] %in% c("Working blood culture instrument in place and under a service contract",
                                                                          "Working automated blood culture instrument in place and under a service contract")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c1"] <- "Core"
  } else if ( hh_all_df[ hh_all_df$key == i, "2b_samplesubmitted"] == "Laboratory is not performing blood cultures") {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c1"] <- "Not applicable"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c1"] <- "Precore"
  }
}

##### Calculating tiers for Subcomponent 2c2 - processing of all sample sources 
## 
##
## Because the LSHTM roadmap calls for sample processing according to SOPs,
## if any 2c2 answers are not "Yes, according to SOP", the tier is "Precore".
## Otherwise, they are "Extended".


## Calculating tiers for Subcomponent 2c2csf - processing of csf samples

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "2c2cerebrospinalfluid"] %in% c("Yes, according to SOP")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c2csf"] <- "Extended"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c2csf"] <- "Precore"
  }
}

## Calculating tiers for Subcomponent 2c2urine - processing of all urine samples 

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "2c2urine"] %in% c("Yes, according to SOP")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c2urine"] <- "Extended"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c2urine"] <- "Precore"
  }
}

## Calculating tiers for Subcomponent 2c2stool - processing of all stool samples 

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "2c2stool"] %in% c("Yes, according to SOP")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c2stool"] <- "Extended"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c2stool"] <- "Precore"
  }
}

## Calculating tiers for Subcomponent 2c2swab - processing of all swab samples 

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "2c2swab"] %in% c("Yes, according to SOP")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c2swab"] <- "Extended"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c2swab"] <- "Precore"
  }
}

## Calculating tiers for Subcomponent 2c2genit - processing of all genit samples 

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "2c2genit"] %in% c("Yes, according to SOP")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c2genit"] <- "Extended"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c2genit"] <- "Precore"
  }
}


##### Calculating tiers for Subcomponent 2c3 - identification of bacteria 
## 
## Because the LSHTM roadmap calls for sample processing according to SOPs,
## if any 2c3 answers are not "Yes, according to SOP", the tier is "Precore".
## Otherwise, they are "Core", as relevant priority pathogens are required.

## Calculating tiers for Subcomponent 2c3strep - processing of all streppnue samples 

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "2c3streppnue"] %in% c("Yes, according to SOP")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c3strep"] <- "Core"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c3strep"] <- "Precore"
  }
}

## Calculating tiers for Subcomponent 2c3stapha - processing of all stapha samples 

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "2c3staphaur"] %in% c("Yes, according to SOP")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c3staph"] <- "Core"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c3staph"] <- "Precore"
  }
}

## Calculating tiers for Subcomponent 2c3stapha - processing of all stapha samples 

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "2c3ecoli"] %in% c("Yes, according to SOP")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c3ecoli"] <- "Core"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c3ecoli"] <- "Precore"
  }
}

## Calculating tiers for Subcomponent 2c3kleb - processing of all kleb samples 

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "2c3klebpnue"] %in% c("Yes, according to SOP")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c3kleb"] <- "Core"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c3kleb"] <- "Precore"
  }
}

## Calculating tiers for Subcomponent 2c3acine - processing of all acine samples 

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "2c3acinetobacter"] %in% c("Yes, according to SOP")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c3acine"] <- "Core"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c3acine"] <- "Precore"
  }
}


## Calculating tiers for Subcomponent 2c3salmonella - processing of all salmonella samples 

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "2c3salmonella"] %in% c("Yes, according to SOP")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c3salmonella"] <- "Core"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c3salmonella"] <- "Precore"
  }
}

## Calculating tiers for Subcomponent 2c3shigella - processing of all shig samples 

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "2c3shigella"] %in% c("Yes, according to SOP")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c3shigella"] <- "Core"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c3shigella"] <- "Precore"
  }
}

## Calculating tiers for Subcomponent 2c3ngoNorrhoea - processing of all 2c3ngoNorrhoea samples 

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "2c3ngoNorrhoea"] %in% c("Yes, according to SOP")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c3ngonor"] <- "Core"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c3ngonor"] <- "Precore"
  }
}

## Calculating tiers for Subcomponent 2c3pseudom - processing of all 2c3pseudom samples 

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "2c3pseudom"] %in% c("Yes, according to SOP")) {
    hh_tiers_df[ hh_tiers_df$key == i, "tier2c3pseudom"] <- "Core"
  } else {
    hh_tiers_df[ hh_tiers_df$key == i, "tier2c3pseudom"] <- "Precore"
  }
}

## Calculating tiers for Subcomponent 2c3styphim - processing of all 2c3styphi samples 

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "2c3styphi"] %in% c("Yes, according to SOP")) {
    hh_tiers_df[ hh_tiers_df$key == i, "tier2c3styphi"] <- "Core"
  } else {
    hh_tiers_df[ hh_tiers_df$key == i, "tier2c3styphi"] <- "Precore"
  }
}

## Calculating tiers for Subcomponent 2c3spara - processing of all 2c3spara samples 

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "2c3spara"] %in% c("Yes, according to SOP")) {
    hh_tiers_df[ hh_tiers_df$key == i, "tier2c3spara"] <- "Core"
  } else {
    hh_tiers_df[ hh_tiers_df$key == i, "tier2c3spara"] <- "Precore"
  }
}

## Calculating tiers for Subcomponent 2c3nmening - processing of all 2c3nmening samples 

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "2c3nmening"] %in% c("Yes, according to SOP")) {
    hh_tiers_df[ hh_tiers_df$key == i, "tier2c3nmening"] <- "Core"
  } else {
    hh_tiers_df[ hh_tiers_df$key == i, "tier2c3nmening"] <- "Precore"
  }
}


## Calculating tiers for Subcomponent 2c4 - processing of all streppnue samples 

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "2c4_bacid_automated"] %in% c("Yes")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c4"] <- "Advanced"
  } else if (hh_all_df[ hh_all_df$key == i, "2c4_bacid_automated"] == "No" &
             hh_all_df[ hh_all_df$key == i, "2c1_bcs_automated"] %in% c("Working blood culture instrument in place and under a service contract",
                                                                                 "Working automated blood culture instrument in place and under a service contract") &
             hh_all_df[ hh_all_df$key == i, "2c5_bacid_system"] %in% c("MALDI TOF",
                                                                               "Vitek II",
                                                                               "BD Phoenix")) {
    hh_tiers_df[ hh_tiers_df$key == i, "tier2c4"] <- "Advanced"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2c4"] <- "Precore"
  }
}


#####
#####
#####
##### Calculating overall tiers for 2c
##
##
##
## GLASS 2.0 has additional priority pathogens, therefore 
## for a site to achieve "Core", it must be able to identify them all
## according to SOPs.
## For a site to achieve "Extended", it must achieve them all plus sample all
## samples sources (e.g. csf).
## For a site to achieve "Advanced", it must also have an automated bacterial
## identification system.

for (i in  hh_all_df$key){
  if(as.numeric(substr(hh_all_df[hh_all_df$key == i,"reporting.month"], 1, 4))>2023){ ## If the row has a reporting month after
    if ( hh_tiers_df[ hh_tiers_df$key == i, "tier2c1"] %in% c("Core") &               ## 2024, determine the tier2c status using
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c2csf"] %in% c("Extended") &        ## the following code which uses all of the
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c2urine"] %in% c("Extended") &      ## GLASS priority pathogens included in the
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c2stool"] %in% c("Extended") &      ## 2024 update. (This includes the new 
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c2swab"] %in% c("Extended") &       ## pathogens including S. typhi, S. paratyphi
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c2genit"] %in% c("Extended") &      ## Pseudomonas aeruginosa and N. meningitidis)
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c3strep"] %in% c("Core") &
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c3staph"] %in% c("Core") &
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c3ecoli"] %in% c("Core") &
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c3kleb"] %in% c("Core") &
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c3acine"] %in% c("Core") &
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c3salmonella"] %in% c("Core") &
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c3shigella"] %in% c("Core") &
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c3ngonor"] %in% c("Core") &
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c3pseudom"] %in% c("Core") &
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c3styphi"] %in% c("Core") &
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c3spara"] %in% c("Core") &
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c3nmening"] %in% c("Core") &
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c4"] %in% c("Advanced")) {
      hh_tiers_df[ hh_tiers_df$key == i, "tier2c"] <- "Advanced"
    } else if ( hh_tiers_df[ hh_tiers_df$key == i, "tier2c1"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c2csf"] %in% c("Extended") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c2urine"] %in% c("Extended") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c2stool"] %in% c("Extended") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c2swab"] %in% c("Extended") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c2genit"] %in% c("Extended") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3strep"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3staph"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3ecoli"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3kleb"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3acine"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3salmonella"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3shigella"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3ngonor"] %in% c("Core")&
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3pseudom"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3styphi"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3spara"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3nmening"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c4"] %in% c("Precore")) {
      hh_tiers_df[ hh_tiers_df$key == i, "tier2c"] <- "Extended" 
    } else if ( hh_tiers_df[ hh_tiers_df$key == i, "tier2c1"] %in% c("Core") &
                ( hh_tiers_df[ hh_tiers_df$key == i, "tier2c2csf"] %in% c("Precore") |
                  hh_tiers_df[ hh_tiers_df$key == i, "tier2c2urine"] %in% c("Precore") |
                  hh_tiers_df[ hh_tiers_df$key == i, "tier2c2stool"] %in% c("Precore") |
                  hh_tiers_df[ hh_tiers_df$key == i, "tier2c2swab"] %in% c("Precore") |
                  hh_tiers_df[ hh_tiers_df$key == i, "tier2c2genit"] %in% c("Precore")) &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3strep"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3staph"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3ecoli"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3kleb"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3acine"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3salmonella"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3shigella"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3ngonor"] %in% c("Core")&
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3pseudom"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3styphi"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3spara"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3nmening"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c4"] %in% c("Precore")) {
      hh_tiers_df[ hh_tiers_df$key == i, "tier2c"] <- "Core" 
    } else {
      hh_tiers_df[ hh_tiers_df$key == i, "tier2c"] <- "Precore" 
    }
  } else {                                                                            ## If the reporting month is before
    if ( hh_tiers_df[ hh_tiers_df$key == i, "tier2c1"] %in% c("Core") &               ## 2024, determine the tier2c status using
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c2csf"] %in% c("Extended") &        ## the old list of GLASS priority pathogens
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c2urine"] %in% c("Extended") &      ## which excludes the new ones, as the new
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c2stool"] %in% c("Extended") &      ## list was not in place at the beginning of
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c2swab"] %in% c("Extended") &       ## the programme.
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c2genit"] %in% c("Extended") &      
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c3strep"] %in% c("Core") &
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c3staph"] %in% c("Core") &
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c3ecoli"] %in% c("Core") &
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c3kleb"] %in% c("Core") &
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c3acine"] %in% c("Core") &
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c3salmonella"] %in% c("Core") &
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c3shigella"] %in% c("Core") &
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c3ngonor"] %in% c("Core") &
         hh_tiers_df[ hh_tiers_df$key == i, "tier2c4"] %in% c("Advanced")) {
      hh_tiers_df[ hh_tiers_df$key == i, "tier2c"] <- "Advanced"
    } else if ( hh_tiers_df[ hh_tiers_df$key == i, "tier2c1"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c2csf"] %in% c("Extended") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c2urine"] %in% c("Extended") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c2stool"] %in% c("Extended") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c2swab"] %in% c("Extended") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c2genit"] %in% c("Extended") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3strep"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3staph"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3ecoli"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3kleb"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3acine"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3salmonella"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3shigella"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3ngonor"] %in% c("Core")&
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c4"] %in% c("Precore")) {
      hh_tiers_df[ hh_tiers_df$key == i, "tier2c"] <- "Extended" 
    } else if ( hh_tiers_df[ hh_tiers_df$key == i, "tier2c1"] %in% c("Core") &
                ( hh_tiers_df[ hh_tiers_df$key == i, "tier2c2csf"] %in% c("Precore") |
                  hh_tiers_df[ hh_tiers_df$key == i, "tier2c2urine"] %in% c("Precore") |
                  hh_tiers_df[ hh_tiers_df$key == i, "tier2c2stool"] %in% c("Precore") |
                  hh_tiers_df[ hh_tiers_df$key == i, "tier2c2swab"] %in% c("Precore") |
                  hh_tiers_df[ hh_tiers_df$key == i, "tier2c2genit"] %in% c("Precore")) &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3strep"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3staph"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3ecoli"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3kleb"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3acine"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3salmonella"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3shigella"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c3ngonor"] %in% c("Core") &
                hh_tiers_df[ hh_tiers_df$key == i, "tier2c4"] %in% c("Precore")) {
      hh_tiers_df[ hh_tiers_df$key == i, "tier2c"] <- "Core" 
    } else {
      hh_tiers_df[ hh_tiers_df$key == i, "tier2c"] <- "Precore" 
    }
  }
}


## Calculating tiers for Subcomponent 2d - 
##
##
## Question 2d1 is important for identification of "Core", as is 2d3. However,
## 2d1 is also important for determining the "Advanced" tier through the 
## identification of automated systems. 

## Calculating tiers for 2d1



for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "2d1_ast_system"] %like any% c("%Vitek%", "%vitek%", "%utomated%")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2d1"] <- "Advanced"
  } else if ( hh_all_df[ hh_all_df$key == i, "2d1_ast_system"] %like any% c("%disc%", "%Disc%", "MIC testing")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2d1"] <- "Core"
  } else if ( hh_all_df[ hh_all_df$key == i, "2d1_ast_system"] %in% c("Not applicable")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2d1"] <- "Not applicable"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2d1"] <- "Precore"
  }
}

## Calculating tiers for 2d2

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "2d2_mic"] %like any% c("%Yes%", "%yes%",
                                                                      "By Etest", "By broth dilution",
                                                                      "Using both Etest and broth dilution",
                                                                      "Other (e.g., automated methods)")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2d2"] <- "Extended"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2d2"] <- "Precore"
  }
}

## Calculating tiers for 2d3

for (i in  hh_all_df$key){
  if (hh_all_df[hh_all_df$key == i, "2d3_ast_sop"] %in% c("Yes, based on up to date EUCAST standards (<3 years old)",
                                                                    "Yes, based on the latest EUCAST standard",
                                                                    "Yes, based on up to date CSLI standards (<3 years old)",
                                                                    "Yes, based on the latest CLSI standard")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2d3"] <- "Core"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2d3"] <- "Precore"
  }
}


##### Calculating overall tiers for 2d - 

for (i in  hh_all_df$key){
  if ( hh_tiers_df[ hh_tiers_df$key == i, "tier2d1"] == "Advanced" &
       hh_tiers_df[ hh_tiers_df$key == i, "tier2d2"] == "Extended" &
       hh_tiers_df[ hh_tiers_df$key == i, "tier2d3"] == "Core") {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2d"] <- "Advanced"
  } else if ( hh_tiers_df[ hh_tiers_df$key == i, "tier2d1"] == "Core" &
              hh_tiers_df[ hh_tiers_df$key == i, "tier2d2"] == "Extended" &
              hh_tiers_df[ hh_tiers_df$key == i, "tier2d3"] == "Core") {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2d"] <- "Extended"
  } else if ( hh_tiers_df[ hh_tiers_df$key == i, "tier2d1"] %in% c("Advanced", "Core" ) &
              hh_tiers_df[ hh_tiers_df$key == i, "tier2d2"] == "Precore" &
              hh_tiers_df[ hh_tiers_df$key == i, "tier2d3"] == "Core") {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2d"] <- "Core"
  } else if ( hh_tiers_df[ hh_tiers_df$key == i, "tier2d1"] == "Not applicable") {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2d"] <- "Not applicable"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2d"] <- "Precore"
  }
}



## Calculating tiers for Subcomponent 2e - 

## Calculating tiers for 2e1

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "2e1_astsop_training"] %in% c("All staff", "Yes")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2e1"] <- "Core"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2e1"] <- "Precore"
  }
}


## Calculating tiers for 2e2

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "2e2_ast_training_othersites"] %in% c("Yes")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2e2"] <- "Advanced"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2e2"] <- "Precore"
  }
}


##### Calculating overall tiers for 2e - 
##
##
## Since question 1d3 asks about eqa participation, this is used to determine
## whether extended was achieved.

for (i in  hh_all_df$key){
  if ( hh_tiers_df[ hh_tiers_df$key == i, "tier2e1"] == "Core" &
       hh_tiers_df[ hh_tiers_df$key == i, "tier1d3"] == "Extended" &
       hh_tiers_df[ hh_tiers_df$key == i, "tier2e2"] == "Advanced") {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2e"] <- "Advanced"
  } else if ( hh_tiers_df[ hh_tiers_df$key == i, "tier2e1"] == "Core" &
              hh_tiers_df[ hh_tiers_df$key == i, "tier1d3"] == "Extended" &
              hh_tiers_df[ hh_tiers_df$key == i, "tier2e2"] == "Precore") {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2e"] <- "Extended"
  } else if ( hh_tiers_df[ hh_tiers_df$key == i, "tier2e1"] == "Core" &
              hh_tiers_df[ hh_tiers_df$key == i, "tier1d3"] == "Precore") {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2e"] <- "Core"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier2e"] <- "Precore"
  }
}


############# Calculating tiers of component 3

## Calculating tiers for Subcomponent 3a - 
##
## 
## 


## Calculating tiers for 3a1

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "3a1_isolates_stored"] %in% c("In -80°C freezers", "Freeze dried")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier3a1"] <- "Extended"
  } else if ( hh_all_df[ hh_all_df$key == i, "3a1_isolates_stored"] %in% c("In -20°C freezers", 
                                                                                 "On agar slopes / agar stabs")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier3a1"] <- "Core"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier3a1"] <- "Precore"
  }
}

## Calculating tiers for 3a2

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "3a2_freezer_backupUPSreliable"] %in% c("Yes")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier3a2"] <- "Extended"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier3a2"] <- "Precore"
  }
}

## Calculating tiers for 3a3

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "3a3_freezers_servicecontract"] %in% c("Yes")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier3a3"] <- "Extended"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier3a3"] <- "Precore"
  }
}

## Calculating tiers for 3a4

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "3a4_isolates_inventory"] %in% c("Other LIMS",
                                                                             "Formal biorepository software",
                                                                             "LIMS") |
       hh_all_df[ hh_all_df$key == i, "3a4_isolates_inventory"] %like any% c("%xcel%")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier3a4"] <- "Extended"
  } else if ( hh_all_df[ hh_all_df$key == i, "3a4_isolates_inventory"] %in% c("Paper-based record / log book") |
              hh_all_df[ hh_all_df$key == i, "3a4_isolates_inventory"] %in% c("Yes")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier3a4"] <- "Core" 
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier3a4"] <- "Precore" 
  }
}


##### Calculating overall tiers for 3a - 
##
## For 3a to achieve "Core", only 3a1 and 3a4 must be "Core" or higher.
## For "Extended" to be achieved, all four questions must be "Extended". Therefore,
## provided that 3a1 and 3a4 achieve at least "Core", if any of the other questions
## do not achieve "Extended", then 3a is "Core".

for (i in  hh_all_df$key){
  if ( hh_tiers_df[ hh_tiers_df$key == i, "tier3a1"] == "Extended" &
       hh_tiers_df[ hh_tiers_df$key == i, "tier3a2"] == "Extended" &
       hh_tiers_df[ hh_tiers_df$key == i, "tier3a3"] == "Extended" &
       hh_tiers_df[ hh_tiers_df$key == i, "tier3a4"] == "Extended") {
     hh_tiers_df[ hh_tiers_df$key == i, "tier3a"] <- "Extended"
  } else if (( hh_tiers_df[ hh_tiers_df$key == i, "tier3a1"] != c("Extended") |
               hh_tiers_df[ hh_tiers_df$key == i, "tier3a4"] != c("Extended") |
               hh_tiers_df[ hh_tiers_df$key == i, "tier3a2"] != c("Extended") |
               hh_tiers_df[ hh_tiers_df$key == i, "tier3a3"] != c("Extended")) &
             ( hh_tiers_df[ hh_tiers_df$key == i, "tier3a1"] != c("Precore") &
               hh_tiers_df[ hh_tiers_df$key == i, "tier3a4"] != c("Precore"))) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier3a"] <- "Core"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier3a"] <- "Precore"
  }
}

## Calculating tiers for Subcomponent 3b - 
##
## 
## 3b1 is required for "Core", 3b2 is required for "Core" but also "Extended".


## Calculating tiers for 3b1

## Given that reference sites may be the coordinating centre for biobanking,
## "Not applicable" is recorded.

for (i in  hh_all_df$key){
  if (hh_all_df[ hh_all_df$key == i, "3b1_biobanking"] %in% c("Yes")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier3b1"] <- "Core"
  } else if (hh_all_df[ hh_all_df$key == i, "3b1_biobanking"] %in% c("Not applicable")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier3b1"] <- "Not applicable"
  } else {
    hh_tiers_df[ hh_tiers_df$key == i, "tier3b1"] <- "Precore"
  }
}

## Calculating tiers for 3b2

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "3b2_biobanking_freq"] %in% c("Every 1-3 months")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier3b2"] <- "Extended"
  } else if ( hh_all_df[ hh_all_df$key == i, "3b2_biobanking_freq"] %in% c("Once a year",
                                                                                 "Every 3-6 months")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier3b2"] <- "Core"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier3b2"] <- "Precore"
  }
}

##### Calculating overall tiers for 3b - 
##
## 3b
##

for (i in  hh_all_df$key){
  if ( hh_tiers_df[ hh_tiers_df$key == i, "tier3b1"] == "Core" &
       hh_tiers_df[ hh_tiers_df$key == i, "tier3b2"] == "Extended") {
    hh_tiers_df[ hh_tiers_df$key == i, "tier3b"] <- "Extended"
  } else if (hh_tiers_df[ hh_tiers_df$key == i, "tier3b1"] == "Core" &
             hh_tiers_df[ hh_tiers_df$key == i, "tier3b2"] == "Core") {
    hh_tiers_df[ hh_tiers_df$key == i, "tier3b"] <- "Core" 
  } else if (hh_tiers_df[ hh_tiers_df$key == i, "tier3b1"] == "Not applicable") {
    hh_tiers_df[ hh_tiers_df$key == i, "tier3b"] <- "Not applicable"
  } else {
    hh_tiers_df[ hh_tiers_df$key == i, "tier3b"] <- "Precore" 
  }
}



## Calculating tiers for Subcomponent 3c - 
##
## 
##


## Calculating tiers for 3c1

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "3c1_isolatetransport_sop"] %in% c("Yes")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier3c1"] <- "Core"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier3c1"] <- "Precore"
  }
}

## Calculating tiers for 3c2

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "3c2_isolatetransport_training"] %like% c("All staff", "all staff")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier3c2"] <- "Core"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier3c2"] <- "Precore"
  }
}



##### Calculating overall tiers for 3c - 
##
## 
##

for (i in  hh_all_df$key){
  if ( hh_tiers_df[ hh_tiers_df$key == i, "tier3c1"] == "Core" &
       hh_tiers_df[ hh_tiers_df$key == i, "tier3c2"] == "Core" &
       hh_tiers_df[ hh_tiers_df$key == i, "tier1d3"] == "Extended") {
     hh_tiers_df[ hh_tiers_df$key == i, "tier3c"] <- "Extended"
  } else if ( hh_tiers_df[ hh_tiers_df$key == i, "tier3c1"] == "Core" &
              hh_tiers_df[ hh_tiers_df$key == i, "tier3c2"] == "Core"&
              hh_tiers_df[ hh_tiers_df$key == i, "tier1d3"] == "Precore") {
     hh_tiers_df[ hh_tiers_df$key == i, "tier3c"] <- "Core" 
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier3c"] <- "Precore" 
  }
}


############# Calculating tiers of component 4

## Calculating tiers for Subcomponent 4a - 
##
## 
## 


## Calculating tiers for 4a1

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "4a1_datasubmitted_amrcc"] %in% c("Yes, anonymised")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier4a1"] <- "Core"
  } else if ( hh_all_df[ hh_all_df$key == i, "4a1_datasubmitted_amrcc"] %in% c("Not applicable")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier4a1"] <- "Not applicable"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier4a1"] <- "Precore"
  }
}


## Calculating tiers for 4a2

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "4a2_datasubmitted_amrcc_freq"] %in% c("Daily automated submission")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier4a2"] <- "Advanced"
  } else if ( hh_all_df[ hh_all_df$key == i, "4a2_datasubmitted_amrcc_freq"] %in% c("Annually",
                                                                                          "Every 3-6 months",
                                                                                          "Monthly",
                                                                                          "Weekly")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier4a2"] <- "Core"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier4a2"] <- "Precore"
  }
}
## needs to be checked that it's working

## Calculating tiers for 4a3

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "4a3_datasharing_other"] %like% c("Yes%")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier4a3local"] <- "Core"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier4a3local"] <- "Precore"
  }
}

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "4a3_datasharing_int"] %like% c("Yes%")) {
    hh_tiers_df[ hh_tiers_df$key == i, "tier4a3int"] <- "Core"
  } else {
    hh_tiers_df[ hh_tiers_df$key == i, "tier4a3int"] <- "Precore"
  }
}


##### Calculating overall tiers for 4a - 
##
## 
## There is no extended, requires 4a1-4a3 to be "Core" to achieve "Core",
## however if 4a2 achieves "Advanced" as well, "Advanced" tier achieved.

for (i in  hh_all_df$key){
  if ( hh_tiers_df[ hh_tiers_df$key == i, "tier4a1"] == "Core" &
       hh_tiers_df[ hh_tiers_df$key == i, "tier4a2"] == "Advanced" &
       (hh_tiers_df[ hh_tiers_df$key == i, "tier4a3local"] == "Core" |
        hh_tiers_df[ hh_tiers_df$key == i, "tier4a3int"] == "Core")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier4a"] <- "Advanced"
  } else if ( hh_tiers_df[ hh_tiers_df$key == i, "tier4a1"] == "Core" &
              hh_tiers_df[ hh_tiers_df$key == i, "tier4a2"] == "Core"&
              (hh_tiers_df[ hh_tiers_df$key == i, "tier4a3local"] == "Core" |
               hh_tiers_df[ hh_tiers_df$key == i, "tier4a3int"] == "Core")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier4a"] <- "Core" 
  } else if ( hh_tiers_df[ hh_tiers_df$key == i, "tier4a1"] == "Not applicable") {
     hh_tiers_df[ hh_tiers_df$key == i, "tier4a"] <- "Not applicable"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier4a"] <- "Precore" 
  }
}


## Calculating overall tiers for Subcomponent 4b - 
##
## 
##
## Only uses one question, 4b. This provides information on "Core", "Extended" and
## "Advanced".

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "4b_datalinkage"] %in% c("Automated linkage of laboratory and clinical databases")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier4b"] <- "Advanced"
  } else if ( hh_all_df[ hh_all_df$key == i, "4b_datalinkage"] == "Clinical and laboratory databases use same unique patient identifier and data can be retrieved by request to IT department") {
     hh_tiers_df[ hh_tiers_df$key == i, "tier4b"] <- "Extended"
  } else if ( hh_all_df[ hh_all_df$key == i, "4b_datalinkage"] %in% c("Paper linkage (e.g. clinical data recorded on archived sample request/reporting form)",
                                                                            "Paper linkage (e.g. clinical data recorded and archived sample request/reporting form)")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier4b"] <- "Core"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier4b"] <- "Precore"
  }
}


## Calculating overall tiers for Subcomponent 4c - 
##
## 
##
## Only uses one question, 4c. Only core can be achieved.

for (i in  hh_all_df$key){
  if ( hh_all_df[ hh_all_df$key == i, "4c_datasharing_policy"] %like% c("Yes")) {
     hh_tiers_df[ hh_tiers_df$key == i, "tier4c"] <- "Core"
  } else {
     hh_tiers_df[ hh_tiers_df$key == i, "tier4c"] <- "Precore"
  }
}

## df_wide serves no purpose anymore, so remove
rm(df_wide)


## Remove the key column
hh_all_df <- select(hh_all_df, !(key))
hh_tiers_df <- select(hh_tiers_df, !(key))



############################# Producing helpful resources

## Save the human readable, wide format of all of the LSHTM roadmap questions
write_xlsx(hh_all_df, "Output files/HH/1a. HH site data - Human readable.xlsx")

## Save the file which contains "Precore", "Core", "Extended" and "Advanced" status
## calculations for each LSHTM Roadmap relevant HH site tool question.
write_xlsx(hh_tiers_df, "Output files/HH/1b. HH site status - All questions.xlsx")

# Note: Individual questions contribute to an overall subcomponent's status, in a
#       question/answer dependent manner. The level that each answer contributes to
#       is indicated in the '1b. HH Site Status - All questions.xlsx' spreadsheet.



############################# LSHTM Roadmap Status Mastersheet

tiers <- c("tier1a", "tier1b", "tier1c", "tier1d", "tier2a", "tier2b", "tier2c", "tier2d", "tier2e", "tier3a", "tier3b", "tier3c", "tier4a", "tier4b", "tier4c")

hh_tiers_df <- select(hh_tiers_df, c(1:5, all_of(tiers)))

## Next, row by row count all of the "values the core, extended and advanced values,
## and place them in their own rows.

hh_tiers_df <- hh_tiers_df %>%
  rowwise() %>%
  mutate(
    core = sum(c_across(starts_with("tier")) == "Core"),
    extended = sum(c_across(starts_with("tier")) == "Extended"),
    advanced = sum(c_across(starts_with("tier")) == "Advanced"),
    `core or above` = sum(c_across(c(core, extended,advanced)))
  ) %>%
  ungroup()

## Save as a mastersheet.
write_xlsx(hh_tiers_df, "Output files/HH/1. HH LSHTM Roadmap status.xlsx") ## Modify the output directory as required.

