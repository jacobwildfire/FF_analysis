############ AH Sample Processing Code
#######
#######
####### Created by Dr Jacob Wildfire as part of the Fleming Fund



####### This code was designed to collate the number of samples processed by
####### sites to produce both excel master documents and graphics. 



# Note: All path examples provide a default
#       that can be run using the files provided
#       in the "FF_analysis" GitHub repository,
#       provided appropriate modifications
#       are made (see README.txt).




#################################### Required packages

# List of required packages
required_packages <- c("ggplot2", "dplyr", "readxl", "writexl", "stringr", 
                       "tidyr", "DescTools", "shiny", "plotly", "DT")

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
df <- read.csv("DHIS2 exported files/AH_sample_data_example.csv") # Replace with your DHIS2 AH sample data .csv filepath

## Load in site masterlist to obtain site information:
site_info <- read_xlsx("Resource files/Site masterlist template.xlsx", sheet = "Sentinel Sites") # Replace with your site masterlist .xlsx filepath 



#################################### Prepare relevant dataframes

## Specify the DHIS2 dataelements that should be isolated.
dataelements <- c("ah_7a1_largeRuminant_faecal_qtr", 
                  "ah_7a2_largeRuminant_milk_qtr", 
                  "ah_7a6_largeRuminant_other_qtr",
                  "ah_7b1_smallRuminant_faecal_qtr", 
                  "ah_7b2_smallRuminant_milk_qtr", 
                  "ah_7b6_smallRuminant_other_qtr",
                  "ah_7c1_pig_faecal_qtr", 
                  "ah_7c6_pig_other_qtr",
                  "ah_7d1_poultry_faecal_qtr", 
                  "ah_7d2_poultry_postmortem_qtr",
                  "ah_7d6_poultry_other_qtr",
                  "ah_7e1_aquaculture_postmortem_qtr", 
                  "ah_7e6_aquaculture_other_qtr",
                  "ah_8a1_largeRuminant_faecal_qtr", 
                  "ah_8a2_largeRuminant_milk_qtr", 
                  "ah_8a3_largeRuminant_urine_qtr",
                  "ah_8a4_largeRuminant_wound_qtr",
                  "ah_8a5_largeRuminant_postmortem_qtr",
                  "ah_8a6_largeRuminant_other_qtr",
                  "ah_8b1_smallRuminant_faecal_qtr", 
                  "ah_8b2_smallRuminant_milk_qtr", 
                  "ah_8b3_smallRuminant_urine_qtr",
                  "ah_8b4_smallRuminant_wound_qtr",
                  "ah_8b5_smallRuminant_postmortem_qtr",
                  "ah_8b6_smallRuminant_other_qtr",
                  "ah_8c1_pig_faecal_qtr",
                  "ah_8c3_pig_urine_qtr", 
                  "ah_8c4_pig_wound_qtr",
                  "ah_8c5_pig_postmortem_qtr", 
                  "ah_8c6_pig_other_qtr",
                  "ah_8d1_poultry_faecal_qtr", 
                  "ah_8d5_poultry_postmortem_qtr", 
                  "ah_8d6_poultry_other_qtr",
                  "ah_8e1_horse_faecal_qtr", 
                  "ah_8e3_horse_urine_qtr",
                  "ah_8e4_horse_wound_qtr", 
                  "ah_8e5_horse_postmortem_qtr",
                  "ah_8e6_horse_other_qtr",
                  "ah_8f5_aquaculture_postmortem_qtr", 
                  "ah_8f6_aquaculture_other_qtr",
                  "ah_9a1_largeRuminant_faecal_qtr", 
                  "ah_9a2_largeRuminant_milk_qtr", 
                  "ah_9a3_largeRuminant_urine_qtr",
                  "ah_9a5_largeRuminant_postmortem_qtr", 
                  "ah_9a6_largeRuminant_other_qtr",
                  "ah_9b1_smallRuminant_faecal_qtr", 
                  "ah_9b2_smallRuminant_milk_qtr",
                  "ah_9b3_smallRuminant_urine_qtr",
                  "ah_9b5_smallRuminant_postmortem_qtr",
                  "ah_9b6_smallRuminant_other_qtr",
                  "ah_9c1_pig_faecal_qtr", 
                  "ah_9c3_pig_urine_qtr", 
                  "ah_9c5_pig_postmortem_qtr",
                  "ah_9c6_pig_other_qtr",
                  "ah_9d1_poultry_faecal_qtr", 
                  "ah_9d5_poultry_postmortem_qtr", 
                  "ah_9d6_poultry_other_qtr",
                  "ah_9e1_horse_faecal_qtr", 
                  "ah_9e3_horse_urine_qtr", 
                  "ah_9e5_horse_postmortem_qtr", 
                  "ah_9e6_horse_other_qtr",
                  "ah_9f5_aquaculture_postmortem_qtr",
                  "ah_9f6_aquaculture_other_qtr",
                  "ah_7f1_wildlife_faecal_qtr",
                  "ah_7f2_wildlife_postmortem_qtr", 
                  "ah_7f6_wildlife_other_qtr",
                  "ah_8g1_wildlife_faecal_qtr",
                  "ah_8g5_Wildlife_postmortem_qtr",
                  "ah_9g1_wildlife_faecal_qtr", 
                  "ah_9g5_Wildlife_postmortem_qtr")

## Use this to select all non-relevant dataelements
df <- df[df$dataelement %in% dataelements,] %>%
  mutate(lastupdated = str_sub(lastupdated, 1, 10)) %>% ## Convert the df$lastupdated column to only the first 10 characters to get the date:
  select(c(1:3,6)) %>% ## Select only the useful rows
  mutate(orgunit = str_sub(orgunit, 1, 5)) ## Select only the first 5 characters of the orgunit to get the site code

 
## Make df "orgunit" into "sitecode"
colnames(df) <- c("dataelement", "reporting.month", "sitecode", "value")


#################################### Producing the sample processing masterlist

## Transform df to a wide format
df_wide <- df %>%
  pivot_wider(names_from = dataelement, values_from = value)

df_wide$site <- NA
df_wide$type <- NA
df_wide$sector <- NA

## Locate the relevant data from the site info list and insert it into the df
for (i in df_wide$sitecode) {
  df_wide[df_wide$`sitecode` == i, "site"] <- site_info[site_info$`Site Code` == i, "Laboratory"]
  df_wide[df_wide$`sitecode` == i, "type"] <- site_info[site_info$`Site Code` == i, "Type"]
  df_wide[df_wide$`sitecode` == i, "sector"] <- site_info[site_info$`Site Code` == i, "Sub-sector"]
}

## Rearrange the dataframe
df_wide <- df_wide %>%
  select(all_of(c("site", "sitecode", "type", "sector", "reporting.month", dataelements))) %>%
  mutate_at(all_of(dataelements), as.numeric)  ## And then convert the value columns to be numeric


## Rename the columns
colnames(df_wide) <- c("site", "sitecode", "type", "sector", "reporting.month",
  "Active large ruminant faecal", "Active large ruminant milk", "Active large ruminant other",
  "Active small ruminant faecal", "Active small ruminant milk", "Active small ruminant other",
  "Active pig faecal", "Active pig other",
  "Active poultry faecal", "Active poultry postmortem", "Active poultry other",
  "Active fish postmortem", "Active fish other",
  "Passive large ruminant faecal", "Passive large ruminant milk", "Passive large ruminant urine",
  "Passive large ruminant wound", "Passive large ruminant postmortem", "Passive large ruminant other",
  "Passive small ruminant faecal", "Passive small ruminant milk", "Passive small ruminant urine",
  "Passive small ruminant wound", "Passive small ruminant postmortem", "Passive small ruminant other",
  "Passive pig faecal", "Passive pig urine", "Passive pig wound", "Passive pig postmortem", "Passive pig other",
  "Passive poultry faecal", "Passive poultry postmortem", "Passive poultry other",
  "Passive horse faecal", "Passive horse urine", "Passive horse wound", "Passive horse postmortem", "Passive horse other",
  "Passive fish postmortem", "Passive fish other",
  "Other large ruminant faecal", "Other large ruminant milk", "Other large ruminant urine",
  "Other large ruminant postmortem", "Other large ruminant other",
  "Other small ruminant faecal", "Other small ruminant milk", "Other small ruminant urine",
  "Other small ruminant postmortem", "Other small ruminant other",
  "Other pig faecal", "Other pig urine", "Other pig postmortem", "Other pig other",
  "Other poultry faecal", "Other poultry postmortem", "Other poultry other",
  "Other horse faecal", "Other horse urine", "Other horse postmortem", "Other horse other",
  "Other fish postmortem", "Other fish other",
  "Active wildlife faecal", "Active wildlife postmortem", "Active wildlife other",
  "Passive wildlife faecal", "Passive wildlife postmortem",
  "Other wildlife faecal", "Other wildlife postmortem"
)

## Add a sum column which calculates the total number of samples processed in this
## reporting month
df_wide$`Total samples` <- rowSums(df_wide[, c(6:75)], na.rm = TRUE)

## Also add sum columns which calculate the total number of active, passive and
## other samples processed in this reporting month 
df_wide$`Total active surveillance samples` <- rowSums(df_wide[, c(6:18,69:71)], na.rm = TRUE)
df_wide$`Total passive surveillance samples` <- rowSums(df_wide[, c(19:45,72:73)], na.rm = TRUE)
df_wide$`Total other surveillance samples` <- rowSums(df_wide[, c(46:68,74:75)], na.rm = TRUE)


## Save as a masterlist.
write_xlsx(df_wide, "Output files/AH/3. AH sample processing.xlsx") ## Modify the output directory as required.



#################################### Producing sample processing graphs

## First, we will make a column containing the `End date` of the reporting period
df_wide <- df_wide %>%
  mutate(
    year = as.numeric(substr(reporting.month, 1, 4)),
    period = substr(reporting.month, 5, 6),
    `End date` = case_when(
      period == "Q1" ~ as.Date(paste0(year, "-03-31")),
      period == "Q2" ~ as.Date(paste0(year, "-06-30")),
      period == "Q3" ~ as.Date(paste0(year, "-09-30")),
      period == "Q4" ~ as.Date(paste0(year, "-12-31")),
      period == "S1" ~ as.Date(paste0(year, "-06-30")),
      period == "S2" ~ as.Date(paste0(year, "-12-31"))
    )
  )%>%
  select(!c("year", "period"))


## Next we will change the reporting.month into a factor, and set the levels to
## be in the appropriate date order.
df_wide$reporting.month <- factor(df_wide$reporting.month,
                                  dput(sort(unique(df_wide$reporting.month)))) 

df_wide$`End date` <- as.Date(df_wide$`End date`)




species_passive_plot <- df_wide %>%
  subset(type == "Surveillance") %>%
  subset(`End date` >= "2024-01-01") %>%
  subset(`End date` <= "2025-10-01") %>%
  select(c(1:75,80)) %>%
  pivot_longer(cols = c(6:75), names_to = "Sample type", values_to = "Samples processed") %>%
  mutate(`Surveillance type` = ifelse(`Sample type` %like% "%Passive%",
                       "Passive",
                       ifelse(
    `Sample type` %like% "%Active%", 
    "Active",
    "Other"
  )),
  Species = ifelse(`Sample type` %like% "%large%", "Large ruminant", ifelse(
    `Sample type` %like% "%small%", "Small ruminant", ifelse(
      `Sample type` %like% "%pig%", "Pig", ifelse(
        `Sample type` %like% "%horse%", "Horses/companion animals", ifelse(
          `Sample type` %like% "%poultry%", "Poultry", ifelse(
            `Sample type` %like% "%fish%", "Aquaculture", "Wildlife")
          ) 
        ) 
      )
    )
  ),
  `Sample type` = ifelse(`Sample type` %like% "%faecal%", "Faecal", ifelse(
    `Sample type` %like% "%milk%", "Milk", ifelse(
      `Sample type` %like% "%urine%", "Urine", ifelse(
        `Sample type` %like% "%wound%", "Wound", ifelse(
          `Sample type` %like% "%postmortem%", "Post-mortem", ifelse(
            `Sample type` %like% "%pond water%", "Pond water", "Other")
            )
          )
        )
      )
    )
  ) %>%
  subset(`Surveillance type` == "Passive" & Species == "Large ruminant") %>%
  arrange(reporting.month) %>%
  group_by(`Sample type`, `End date`) %>%
  summarize(`Samples processed` = sum(`Samples processed`)) %>%
  subset(!is.na(`Samples processed`))%>%
  mutate(`Cumulative samples` = cumsum(`Samples processed`))%>%
  ggplot(aes(x = as.Date(`End date`), y = as.numeric(`Cumulative samples`), 
             color = `Sample type`)) +
  theme_bw()+
  stat_summary(fun.data = mean_cl_normal, geom = "line") +
  geom_point()+
  xlab("Report date")+
  ylab("Cumulative samples processed")+
  scale_x_date(date_breaks = 'year', date_labels = "%Y") +
  scale_color_discrete(name = "Species")+
  theme(text=element_text(size=12), #change font size of all text
        axis.text=element_text(size=12), #change font size of axis text
        axis.title=element_text(size=12), #change font size of axis titles
        plot.title=element_text(size=12), #change font size of plot title
        legend.text=element_text(size=12), #change font size of legend text
        legend.title=element_text(size=12))




ui <- fluidPage(
  titlePanel("Interactive Sample Processing Dashboard"),
  
  tabsetPanel(
    tabPanel("By Site",
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput("date_range_sites", "Select Report Date Range:",
                                start = min(as.Date(df_wide$`End date`)),
                                end = max(as.Date(df_wide$`End date`)),
                                min = min(as.Date(df_wide$`End date`)),
                                max = max(as.Date(df_wide$`End date`)),
                                format = "yyyy-mm-dd"),
                 
                 radioButtons("view_sites", "Select View:",
                              choices = c("Cumulative" = "cumulative",
                                          "Per Quarter" = "quarter"),
                              selected = "cumulative"),
                 
                 selectInput("file_format_sites", "Download Format:",
                             choices = c("PNG" = "png", "PDF" = "pdf")),
                 
                 downloadButton("downloadSites", "Download Plot")
               )
               ,
               mainPanel(
                 plotlyOutput("sitesPlot"),
                 hr(),
                 DT::dataTableOutput("sitesTable")
               )
             )
    ),
    
    tabPanel("By Species",
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput("date_range_species", "Select Report Date Range:",
                                start = min(as.Date(df_wide$`End date`)),
                                end = max(as.Date(df_wide$`End date`)),
                                min = min(as.Date(df_wide$`End date`)),
                                max = max(as.Date(df_wide$`End date`)),
                                format = "yyyy-mm-dd"),
                 
                 radioButtons("view_species", "Select View:",
                              choices = c("Cumulative" = "cumulative",
                                          "Per Quarter" = "quarter"),
                              selected = "cumulative"),
                 
                 radioButtons("site_type", "Select Site Type:",
                              choices = c("Surveillance", "Reference"),
                              selected = "Surveillance"),
                 
                 radioButtons("sample_type", "Select Surveillance Type:",
                              choices = c("Passive", "Active", "Other"),
                              selected = "Passive"),
                 
                 checkboxGroupInput("species_filter", "Select Species:",
                                    choices = c("Large ruminant", "Small ruminant", "Pig",
                                                "Horses/companion animals", "Poultry",
                                                "Aquaculture", "Wildlife"),
                                    selected = c("Large ruminant", "Small ruminant", "Pig",
                                                 "Horses/companion animals", "Poultry",
                                                 "Aquaculture", "Wildlife")),
                 
                 selectInput("file_format_species", "Download Format:",
                             choices = c("PNG" = "png", "PDF" = "pdf")),
                 
                 downloadButton("downloadSpecies", "Download Plot")
               ),
               mainPanel(
                 plotlyOutput("speciesPlot"),
                 hr(),
                 DT::dataTableOutput("speciesTable")
               )
             )
    ),
    
    
    tabPanel("By Species and Sample Type",
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput("date_range_speciestype", "Select Report Date Range:",
                                start = min(as.Date(df_wide$`End date`)),
                                end = max(as.Date(df_wide$`End date`)),
                                min = min(as.Date(df_wide$`End date`)),
                                max = max(as.Date(df_wide$`End date`)),
                                format = "yyyy-mm-dd"
                 ),
                 
                 
                 radioButtons("view_speciestype", "Select View:",
                              choices = c("Cumulative" = "cumulative",
                                          "Per Quarter" = "quarter"),
                              selected = "cumulative"
                 ),
                 
                 
                 radioButtons("type_filter_speciestype", "Select Site Type:",
                              choices = c("Surveillance", "Reference"),
                              selected = "Surveillance"
                 ),
                 radioButtons("speciestype_filter", "Select Surveillance Type:",
                              choices = c("Passive", "Active", "Other"),
                              selected = "Passive"
                 ),
                 selectInput("species_filter_speciestype", "Select Species:",
                             choices = c("Large ruminant", "Small ruminant", "Pig",
                                         "Horses/companion animals", "Poultry",
                                         "Aquaculture", "Wildlife"),
                             selected = "Large ruminant"
                 ),
                 selectInput("file_format_speciestype", "Download Format:",
                             choices = c("PNG" = "png", "PDF" = "pdf")
                 ),
                 downloadButton("downloadSurvtypePlot", "Download Plot")
               ),
               mainPanel(
                 plotlyOutput("speciestypePlot"),
                 hr(),
                 DT::dataTableOutput("speciestypeTable")
               )
             )
    )
    
  )
)

server <- function(input, output) {
  
  # ---------------- Sites Tab ----------------
  sites_data <- reactive({
    df_wide %>%
      filter(`End date` >= input$date_range_sites[1],
             `End date` <= input$date_range_sites[2]) %>%
      arrange(sitecode, reporting.month)
  })
  
  sites_plot <- reactive({
    if (input$view_sites == "cumulative") {
      plot_data <- sites_data() %>%
        group_by(sitecode) %>%
        mutate(`Cumulative samples` = cumsum(replace_na(`Total samples`, 0))) %>%
        ungroup()
      
      ggplot(plot_data, aes(x = `End date`, y = `Cumulative samples`,
                            color = sitecode, shape = type)) +
        theme_bw() +
        stat_summary(fun.data = mean_cl_normal, geom = "line") +
        geom_point() +
        labs(x = "Report date", y = "Cumulative samples processed") +
        scale_x_date(date_breaks = 'year', date_labels = "%Y") +
        scale_color_discrete(name = "Site") +
        scale_shape_discrete(name = "Site type") +
        theme(text = element_text(size = 12))
      
    } else {
      ggplot(sites_data(), aes(x = `End date`, y = `Total samples`,
                               color = sitecode, shape = type)) +
        theme_bw() +
        stat_summary(fun.data = mean_cl_normal, geom = "line") +
        geom_point() +
        labs(x = "Report date", y = "Samples processed per quarter") +
        scale_x_date(date_breaks = 'year', date_labels = "%Y") +
        scale_color_discrete(name = "Site") +
        scale_shape_discrete(name = "Site type") +
        theme(text = element_text(size = 12))
    }
  })
  
  output$sitesPlot <- renderPlotly({
    ggplotly(sites_plot(), tooltip = c("x", "y", "color", "shape"))
  })
  
  output$downloadSites <- downloadHandler(
    filename = function() {
      paste("sites_plot_", Sys.Date(), ".", input$file_format_sites, sep = "")
    },
    content = function(file) {
      ggsave(file, plot = sites_plot(), width = 10, height = 6, device = input$file_format_sites)
    }
  )
  
  
  output$sitesTable <- DT::renderDataTable({
    table_data <- sites_data() %>%
      select(site, sitecode, type, `End date`, `Total samples`)
    
    if (input$view_sites == "cumulative") {
      table_data <- table_data %>%
        arrange(sitecode, `End date`) %>%
        group_by(sitecode) %>%
        mutate(`Cumulative samples` = cumsum(replace_na(`Total samples`, 0))) %>%
        ungroup()
    }
    
    table_data
  })
  
  
  # ---------------- Species Tab ----------------
  species_data <- reactive({
    df_wide %>%
      filter(type == input$site_type,
             `End date` >= input$date_range_species[1],
             `End date` <= input$date_range_species[2]) %>%
      select(c(1:75, 80)) %>%
      pivot_longer(cols = c(6:75), names_to = "Sample type", values_to = "Samples processed") %>%
      mutate(
        `Surveillance type` = case_when(
          str_detect(`Sample type`, "Passive") ~ "Passive",
          str_detect(`Sample type`, "Active") ~ "Active",
          TRUE ~ "Other"
        ),
        Species = case_when(
          str_detect(`Sample type`, "large") ~ "Large ruminant",
          str_detect(`Sample type`, "small") ~ "Small ruminant",
          str_detect(`Sample type`, "pig") ~ "Pig",
          str_detect(`Sample type`, "horse") ~ "Horses/companion animals",
          str_detect(`Sample type`, "poultry") ~ "Poultry",
          str_detect(`Sample type`, "fish") ~ "Aquaculture",
          TRUE ~ "Wildlife"
        )
        ,
        Species = factor(Species, levels = c(
          "Large ruminant", "Small ruminant", "Pig",
          "Horses/companion animals", "Poultry", "Aquaculture", "Wildlife"
        ))
      ) %>%
      filter(`Surveillance type` == input$sample_type,
             Species %in% input$species_filter) %>%
      group_by(Species, `End date`) %>%
      summarize(`Samples processed` = sum(`Samples processed`, na.rm = TRUE), .groups = "drop") %>%
      arrange(`End date`)
  })
  
  species_plot <- reactive({
    plot_data <- species_data()
    
    if (input$view_species == "cumulative") {
      plot_data <- plot_data %>%
        group_by(Species) %>%
        mutate(`Cumulative samples` = cumsum(`Samples processed`)) %>%
        ungroup()
      
      ggplot(plot_data, aes(x = `End date`, y = `Cumulative samples`, color = Species)) +
        theme_bw() +
        stat_summary(fun.data = mean_cl_normal, geom = "line") +
        geom_point() +
        labs(x = "Report date", y = "Cumulative samples processed") +
        scale_x_date(date_breaks = 'year', date_labels = "%Y") +
        scale_color_manual(name = "Species",values = c("Large ruminant" = "#31133a",
                                                       "Small ruminant" = "#4e84f6",
                                                       "Pig" = "#0ae1b7",
                                                       "Horses/companion animals" = "#9efa53",
                                                       "Poultry" = "#f9bc4c",
                                                       "Aquaculture" = "#e45122",
                                                       "Wildlife" = "#7a140c")) +
        theme(text = element_text(size = 12))
      
    } else {
      ggplot(plot_data, aes(x = `End date`, y = `Samples processed`, color = Species)) +
        theme_bw() +
        stat_summary(fun.data = mean_cl_normal, geom = "line") +
        geom_point() +
        labs(x = "Report date", y = "Samples processed per quarter") +
        scale_x_date(date_breaks = 'year', date_labels = "%Y") +
        scale_color_manual(name = "Species",values = c("Large ruminant" = "#31133a",
                                                       "Small ruminant" = "#4e84f6",
                                                       "Pig" = "#0ae1b7",
                                                       "Horses/companion animals" = "#9efa53",
                                                       "Poultry" = "#f9bc4c",
                                                       "Aquaculture" = "#e45122",
                                                       "Wildlife" = "#7a140c")) +
        theme(text = element_text(size = 12))
    }
  })
  
  output$speciesPlot <- renderPlotly({
    ggplotly(species_plot(), tooltip = c("x", "y", "color"))
  })
  
  output$downloadSpecies <- downloadHandler(
    filename = function() {
      paste("species_plot_", Sys.Date(), ".", input$file_format_species, sep = "")
    },
    content = function(file) {
      ggsave(file, plot = species_plot(), width = 10, height = 6, device = input$file_format_species)
    }
  )
  
  
  output$speciesTable <- DT::renderDataTable({
    table_data <- species_data()
    
    if (input$view_species == "cumulative") {
      table_data <- table_data %>%
        group_by(Species) %>%
        mutate(`Cumulative samples` = cumsum(`Samples processed`)) %>%
        ungroup()
    }
    
    table_data
  })
  
  
  # ---------------- Species Type Tab ----------------
  
  
  speciestype_data <- reactive({
    df_wide %>%
      filter(type == input$type_filter_speciestype,
             `End date` >= input$date_range_speciestype[1],
             `End date` <= input$date_range_speciestype[2]) %>%
      select(c(1:75, 80)) %>%
      pivot_longer(cols = c(6:75), names_to = "Sample type", values_to = "Samples processed") %>%
      mutate(
        `Surveillance type` = case_when(
          str_detect(`Sample type`, "Passive") ~ "Passive",
          str_detect(`Sample type`, "Active") ~ "Active",
          TRUE ~ "Other"
        ),
        Species = case_when(
          str_detect(`Sample type`, "large") ~ "Large ruminant",
          str_detect(`Sample type`, "small") ~ "Small ruminant",
          str_detect(`Sample type`, "pig") ~ "Pig",
          str_detect(`Sample type`, "horse") ~ "Horses/companion animals",
          str_detect(`Sample type`, "poultry") ~ "Poultry",
          str_detect(`Sample type`, "fish") ~ "Aquaculture",
          TRUE ~ "Wildlife"
        ),
        `Sample type` = case_when(
          str_detect(`Sample type`, "faecal") ~ "Faecal",
          str_detect(`Sample type`, "milk") ~ "Milk",
          str_detect(`Sample type`, "urine") ~ "Urine",
          str_detect(`Sample type`, "wound") ~ "Wound",
          str_detect(`Sample type`, "postmortem") ~ "Post-mortem",
          str_detect(`Sample type`, "pond water") ~ "Pond water",
          TRUE ~ "Other"
        )
      ) %>%
      filter(`Surveillance type` == input$speciestype_filter,
             Species == input$species_filter_speciestype) %>%
      group_by(`Sample type`, `End date`) %>%
      summarize(`Samples processed` = sum(`Samples processed`, na.rm = TRUE), .groups = "drop") %>%
      arrange(`End date`)%>%
      mutate(`Sample type` = factor(`Sample type`, levels = c("Faecal","Milk",
                                                              "Wound","Post-mortem",
                                                              "Urine","Other")))
    
  })
  
  
  speciestype_plot <- reactive({
    plot_data <- speciestype_data()
    
    if (input$view_speciestype == "cumulative") {
      
      plot_data <- plot_data %>%
        group_by(`Sample type`) %>%
        mutate(`Cumulative samples` = cumsum(`Samples processed`)) %>%
        ungroup()
      
      ggplot(plot_data, aes(x = `End date`, y = `Cumulative samples`, color = `Sample type`)) +
        theme_bw() +
        stat_summary(fun.data = mean_cl_normal, geom = "line") +
        geom_point() +
        labs(x = "Report date", y = "Cumulative samples processed") +
        scale_x_date(date_breaks = 'year', date_labels = "%Y") +
        scale_color_manual(name = "Sample type", 
                           values = c(
                             "Faecal" = "#170784",
                             "Milk" = "#6d0ca4",
                             "Urine" = "#fbaa48",        # yellow-orange
                             "Wound" = "#b2358e",
                             "Post-mortem" = "#e16b65",
                             "Other" = "#6baed6"         # changed to light blue
                           )
        ) +
        theme(text = element_text(size = 12),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 12),
              plot.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 12))
    } else {
      ggplot(plot_data, aes(x = `End date`, y = `Samples processed`, color = `Sample type`)) +
        theme_bw() +
        stat_summary(fun.data = mean_cl_normal, geom = "line") +
        geom_point() +
        labs(x = "Report date", y = "Samples processed per quarter") +
        scale_x_date(date_breaks = 'year', date_labels = "%Y") +
        scale_color_manual(name = "Sample type", 
                           values = c(
                             "Faecal" = "#170784",
                             "Milk" = "#6d0ca4",
                             "Urine" = "#fbaa48",        # yellow-orange
                             "Wound" = "#b2358e",
                             "Post-mortem" = "#e16b65",
                             "Other" = "#6baed6"         # changed to light blue
                           )
        ) +
        theme(text = element_text(size = 12),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 12),
              plot.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 12))
    }
  })
  
  
  output$speciestypePlot <- renderPlotly({
    ggplotly(speciestype_plot(), tooltip = c("x", "y", "color"))
  })
  
  output$downloadSurvtypePlot <- downloadHandler(
    filename = function() {
      paste("speciestype_plot_", Sys.Date(), ".", input$file_format_speciestype, sep = "")
    },
    content = function(file) {
      ggsave(file, plot = speciestype_plot(), width = 10, height = 6, device = input$file_format_speciestype)
    }
  )
  
  
  output$speciestypeTable <- DT::renderDataTable({
    table_data <- speciestype_data()
    
    if (input$view_speciestype == "cumulative") {
      table_data <- table_data %>%
        group_by(`Sample type`) %>%
        mutate(`Cumulative samples` = cumsum(`Samples processed`)) %>%
        ungroup()
    }
    
    table_data
  }, options = list(pageLength = 10, scrollX = TRUE))
  
}

shinyApp(ui = ui, server = server)
