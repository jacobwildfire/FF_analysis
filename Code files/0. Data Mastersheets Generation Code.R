############ Data Mastersheets Generation Code
#######
#######
####### Created by Dr Jacob Wildfire as part of the Fleming Fund


####### This code was designed to collate the answers to the Planning and
####### Monitoring tools so that they are human readable in excel formats.
####### These data mastersheets can then be easily referred to when querying
####### data.
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
hh_df <- read.csv("DHIS2 exported files/HH_site_data_example.csv") # Replace with your HH data .csv filepath
ah_df <- read.csv("DHIS2 exported files/AH_site_data_example.csv") # Replace with your AH data .csv filepath

df <- rbind(hh_df, ah_df)

## Load in site masterlist to obtain site information:
site_info <- read_xlsx("Resource files/Site masterlist template.xlsx", sheet = "Sentinel Sites") # Replace with your site masterlist .xlsx filepath 

## Load in the DHIS2 dictionary which contains information about all DHIS2 data elements
dictionary <- read_excel("Resource files/DHIS2 data dictionary - DON'T MODIFY ME.xlsx") %>% # If you have moved the DHIS2 dictionary, enter the new filepath
  select(1:8)


#################################### Manipulating/generating necessary dataframes

## Convert "Action to change (please specify)" into a simpler format.
df <- df %>% 
  mutate_if(is.character, ~ ifelse(. == "Action to change (Please specify)", "Action to change", .))

## Create a dictionary with only the HH SP&M questions
dictionary_HH <- dictionary %>%
  subset(Form == "HH SP&M")

## Create a dictionary with only the AH SP&M questions
dictionary_AH <- dictionary %>%
  subset(Form == "AH SP&M")

# Create a list to store the new dictionary dataframes
new_dictionaries <- list()

# Get unique orgunits
unique_orgunits <- unique(df$orgunit)
unique_dates <- unique(df$period)



#################################### Generating the HH SP&M Mastersheet

## Generate an empty dataframe with the dictionary_HH structure
hh_spam <- data.frame(matrix(ncol = 10, nrow = 0))

#provide column names
colnames(hh_spam) <- c(colnames(dictionary_HH), "Orgunit","Period")

# Loop through each unique orgunit
for (date in unique_dates) {
  for (orgunit in unique_orgunits){
  # Filter df for the current orgunit and date
  df_filtered <- df %>% filter(orgunit == !!orgunit & period == !!date)
  
  # Filter dictionary for rows where Data element, Comment, Planned change, Change description, or Achievement date aim match df$dataelement
  dictionary_filtered <- dictionary_HH %>%
    filter(`Data element` %in% df_filtered$dataelement |
             Comment %in% df_filtered$dataelement |
             `Planned change` %in% df_filtered$dataelement |
             `Change description` %in% df_filtered$dataelement |
             `Achievement date aim` %in% df_filtered$dataelement)
  
  # Replace values in the filtered dictionary
  dictionary_filtered <- dictionary_filtered %>%
    mutate(`Data element` = ifelse(`Data element` %in% df_filtered$dataelement, df_filtered$value[match(`Data element`, df_filtered$dataelement)], NA),
           Comment = ifelse(Comment %in% df_filtered$dataelement, df_filtered$value[match(Comment, df_filtered$dataelement)], NA),
           `Planned change` = ifelse(`Planned change` %in% df_filtered$dataelement, df_filtered$value[match(`Planned change`, df_filtered$dataelement)], NA),
           `Change description` = ifelse(`Change description` %in% df_filtered$dataelement, df_filtered$value[match(`Change description`, df_filtered$dataelement)], NA),
           `Achievement date aim` = ifelse(`Achievement date aim` %in% df_filtered$dataelement, df_filtered$value[match(`Achievement date aim`, df_filtered$dataelement)], NA))
  
  ## Rename the column Data element to Answer to make the spreadsheet more clear
  dictionary_filtered <- dictionary_filtered %>%
    rename(Answer = `Data element`)
  
  ## Add the orgunit and appropriate date for these values
  dictionary_filtered$Orgunit <- orgunit
  dictionary_filtered$Period <- date
  
  ## Add the dictionary_filtered data frame to the hh_spam dataframe
  hh_spam <- rbind(hh_spam, dictionary_filtered)
  
  }
}


## Insert the LSHTM roadmap that is relevant to each variable/question for analysis
## purposes

hh_spam <- hh_spam %>%
  mutate(roadmap_subcomponent = case_when(
    `Number (if applicable)` %in% c("1a1", "1a2", "1a3") ~ "1a",
    `Number (if applicable)` %in% c("1b1", "1b2", "1b3", "1b4", "1b5") ~ "1b",
    `Number (if applicable)` %in% c("1c1", "1c2", "1c3") ~ "1c",
    `Number (if applicable)` %in% c("2a1", "2a2", "2a3", "2a12") ~ "1d",
    `Number (if applicable)` %in% c("3a1", "3a2", "3a3") ~ "2a",
    `Number (if applicable)` %in% c("3b1") ~ "2b",
    `Number (if applicable)` %in% c("3c1", "3c2", "3c3", "3c4", "3c5", "3c6", "3c7",
                                    "3c8", "3c9", "3c10", "3c11", "3c12", "3c13", "3c14",
                                    "3c15", "3c16", "3c17", "3c18", "3c19", "3c20") ~ "2c",
    `Number (if applicable)` %in% c("3d1", "3d2", "3d3") ~ "2d",
    `Number (if applicable)` %in% c("3e1", "3e2") ~ "2e",
    `Number (if applicable)` %in% c("4a1", "4a2", "4a3", "4a4") ~ "3a",
    `Number (if applicable)` %in% c("4b1", "4b2") ~ "3b",
    `Number (if applicable)` %in% c("4c1", "4c2") ~ "3c",
    `Number (if applicable)` %in% c("5a1", "5a2", "5a6", "5a7") ~ "4a",
    `Number (if applicable)` %in% c("5b") ~ "4b",
    `Number (if applicable)` %in% c("5c") ~ "4c",
  ))


## Produce a site code column
hh_spam <- hh_spam  %>%
  mutate(`sitecode` = str_sub(Orgunit, 1, 5))

## Just make sure that the site Type is in there
## Make a new column for the region
hh_spam$Type <- NA

## Using the site mastersheet, fill in the country, region and type of each site.
for (i in unique(hh_spam$`sitecode`)) {
  hh_spam[hh_spam$`sitecode` == i,"Type"] <- site_info[site_info$`Site Code` == i, "Type"]
}


## Reorganise the columns to emphasise the orgunit and date 
hh_spam <- select(hh_spam, c(sitecode, Orgunit, Type, Period,`Number (if applicable)`,
                             roadmap_subcomponent, Question, Answer, Comment,
                             `Planned change`, `Change description`, `Achievement date aim`))








#################################### Generating the AH SP&M Mastersheet

ah_spam <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(ah_spam) <- c(colnames(dictionary_AH), "Orgunit","Period")

# Loop through each unique orgunit
for (date in unique_dates) {
  for (orgunit in unique_orgunits){
    # Filter df for the current orgunit and date
    df_filtered <- df %>% filter(orgunit == !!orgunit & period == !!date)
    
    # Filter dictionary for rows where Data element, Comment, Planned change, Change description, or Achievement date aim match df$dataelement
    dictionary_filtered <- dictionary_AH %>%
      filter(`Data element` %in% df_filtered$dataelement |
               Comment %in% df_filtered$dataelement |
               `Planned change` %in% df_filtered$dataelement |
               `Change description` %in% df_filtered$dataelement |
               `Achievement date aim` %in% df_filtered$dataelement)
    
    # Replace values in the filtered dictionary
    dictionary_filtered <- dictionary_filtered %>%
      mutate(`Data element` = ifelse(`Data element` %in% df_filtered$dataelement, df_filtered$value[match(`Data element`, df_filtered$dataelement)], NA),
             Comment = ifelse(Comment %in% df_filtered$dataelement, df_filtered$value[match(Comment, df_filtered$dataelement)], NA),
             `Planned change` = ifelse(`Planned change` %in% df_filtered$dataelement, df_filtered$value[match(`Planned change`, df_filtered$dataelement)], NA),
             `Change description` = ifelse(`Change description` %in% df_filtered$dataelement, df_filtered$value[match(`Change description`, df_filtered$dataelement)], NA),
             `Achievement date aim` = ifelse(`Achievement date aim` %in% df_filtered$dataelement, df_filtered$value[match(`Achievement date aim`, df_filtered$dataelement)], NA))
    
    ## Rename the column Data element to Answer to make the spreadsheet more clear
    dictionary_filtered <- dictionary_filtered %>%
      rename(Answer = `Data element`)
    
    ## Add the orgunit and appropriate date for these values
    dictionary_filtered$Orgunit <- orgunit
    dictionary_filtered$Period <- date
    
    ## Add the dictionary_filtered data frame to the hh_spam dataframe
    ah_spam <- rbind(ah_spam, dictionary_filtered)
    
  }
}


## Insert the LSHTM roadmap that is relevant to each variable/question for analysis
## purposes

ah_spam <- ah_spam %>%
  mutate(roadmap_subcomponent = case_when(
    `Number (if applicable)` %in% c("A2.1.4") ~ "1a",
    `Number (if applicable)` %in% c("A2.1.2", "A2.2.1","A2.1.5","A2.2.2","B4.7","B4.9") ~ "1b",
    `Number (if applicable)` %in% c("A3.1","A3.2","A3.3") ~ "2a",
    `Number (if applicable)` %in% c("B2.1.1","B2.1.2","B2.1.5","B2.1.4","B2.1.7",
                                    "B2.1.3","B2.1.6","B2.1.8","B2.1.9") ~ "2b",
    `Number (if applicable)` %in% c("B2.2.2","B2.2.3","B2.2.4") ~ "2c",
    `Number (if applicable)` %in% c("B5.1.1","B5.1.4") ~ "2d",
    `Number (if applicable)` %in% c("B3.1.2","B3.1.4","B3.1.5","B3.1.6","B3.1.7") ~ "3a",
    `Number (if applicable)` %in% c("B3.2.1","B3.2.2") ~ "3b",
    `Number (if applicable)` %in% c("B5.1.5","B5.1.6") ~ "3c",
    `Number (if applicable)` %in% c("B6.3.1","B6.3.2","B6.3.3","B6.3.5") ~ "4a",
    `Number (if applicable)` %in% c("B6.4.1","B6.4.2") ~ "4b",
    `Number (if applicable)` %in% c("B6.5") ~ "4c"
  ))

## Produce a site code column
ah_spam <- ah_spam  %>%
  mutate(`sitecode` = str_sub(Orgunit, 1, 5))

## Just make sure that the site Type is in there
## Make a new column for the region
ah_spam$Type <- NA
ah_spam$`Sub-sector` <- NA

## Using the site mastersheet, fill in the country, region and type of each site.
for (i in unique(ah_spam$`sitecode`)) {
  ah_spam[ah_spam$`sitecode` == i,"Type"] <- unique(site_info[site_info$`Site Code` == i, "Type"])
  ah_spam[ah_spam$`sitecode` == i,"Sub-sector"] <- unique(site_info[site_info$`Site Code` == i, "Sub-sector"])
}

## Reorganise the columns to emphasise the orgunit and date 
ah_spam <- select(ah_spam, c(`sitecode`, Orgunit, Type, `Sub-sector`,
                             Period, `Number (if applicable)`, roadmap_subcomponent,
                             Question, Answer, Comment, `Planned change`, `Change description`,
                             `Achievement date aim`))



############################# Saving Mastersheets

write_xlsx(path = "Output files/0. AH site tool masterlist.xlsx", ah_spam)
write_xlsx(path = "Output files/0. HH site tool masterlist.xlsx", hh_spam)
