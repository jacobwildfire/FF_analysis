############ HH Sample Processing Code
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
df <- read.csv("DHIS2 exported files/HH_sample_data_example.csv") # Replace with your DHIS2 HH sample data .csv filepath

## Load in site masterlist to obtain site information:
site_info <- read_xlsx("Resource files/Site masterlist template.xlsx", sheet = "Sentinel Sites") # Replace with your site masterlist .xlsx filepath 



#################################### Prepare relevant dataframes

## Specify the DHIS2 dataelements that should be isolated.
dataelements <- c("hh_8a1_cerebrospinalfluid_qtr",
                  "hh_8a2_cerebrospinalfluid_positive_qtr", 
                  "hh_8b1_urine_qtr", 
                  "hh_8b2_urine_positive_qtr",
                  "hh_8c1_stool_qtr", 
                  "hh_8c2_stool_positive_qtr",
                  "hh_8d1_pus_qtr", 
                  "hh_8d2_pus_positive_qtr", 
                  "hh_8e1_genitourinary_qtr", 
                  "hh_8e2_genitourinary_positive_qtr",
                  "hh_8f1_blood_qtr", 
                  "hh_8f2_blood_positive_qtr",
                  "hh_8g1_other_qtr", 
                  "hh_8g2_other_positive_qtr"
)

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

## Locate the relevant data from the site info list and insert it into the df
for (i in df_wide$sitecode) {
  df_wide[df_wide$`sitecode` == i, "site"] <- site_info[site_info$`Site Code` == i, "Laboratory"]
  df_wide[df_wide$`sitecode` == i, "type"] <- site_info[site_info$`Site Code` == i, "Type"]
}

## Rearrange the dataframe
df_wide <- df_wide %>%
  select(all_of(c("site", "sitecode", "type", "reporting.month", dataelements))) %>%
  mutate_at(all_of(dataelements), as.numeric)  ## And then convert the value columns to be numeric


## Rename the columns
colnames(df_wide) <- c("site", "sitecode", "type", "reporting.month", 
                       "Cerebrospinal fluid samples", "Positive cerebrospinal fluid samples",
                       "Urine samples", "Positive urine samples", "Stool samples",
                       "Positive stool samples", "Pus/pus swab samples", 
                       "Positive pus/pus swab samples", "Genitourinary samples",
                       "Positive genitourinary samples", "Blood samples", 
                       "Positive blood samples", "Other samples", "Positive other samples")


## Add a sum column which calculates the total number of samples processed in this
## reporting month
df_wide$`Total samples` <- rowSums(df_wide[, c(5,7,9,11,13,15,17)], na.rm = TRUE)

## Add a positive sum column which calculates the total number of positive samples 
## in this reporting month
df_wide$`Total positive samples` <- rowSums(df_wide[, c(6,8,10,12,14,16,18)], na.rm = TRUE)


## Save as a masterlist.
write_xlsx(df_wide, "Output files/HH/3. HH sample processing.xlsx") ## Modify the output directory as required.


#################################### Producing sample positivity percentage masterlist

## Positivity rates can be used as a proxy to identify laboratories experiencing
## contamination issues, particularly with blood cultures which should otherwise
## be sterile. As such, this code produces a blood culture positivity percentage
## masterlist to allow this examination.

## Produce a positivity dataframe using the site masterlist.
positivity_df <- df_wide

## Produce a column in which the percentage of positive blood cultures for each site
## in each quarter are calculated.
positivity_df$`Blood positivity percentage` <- (positivity_df$`Positive blood samples`/positivity_df$`Blood samples`)*100

## Reorganise the dataframe to select only the required columns
positivity_df <- positivity_df %>%
  select(c(site, sitecode, type, reporting.month, `Blood positivity percentage`))

## Convert Inf values, caused by division by zero, to be zero. This indicates there
## were no positive samples.
positivity_df[positivity_df$`Blood positivity percentage` == Inf &
                !is.na(positivity_df$`Blood positivity percentage`),
              "Blood positivity percentage"] <- 0

## Produce a wider version of the document, values over time to be more easily
## visualised by site.
wide_positivity_df <- positivity_df %>%
  pivot_wider(names_from = reporting.month, values_from = `Blood positivity percentage`, names_sort = TRUE)


## Save as a masterlist.
write_xlsx(wide_positivity_df, "Output files/HH/3a. Blood culture positivity percentage masterlist.xlsx")




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



okabe_ito <- c(
  "Blood samples" = "#D55E00",
  "Urine samples" = "#F0E442",
  "Pus/pus swab samples" = "#0072B2",
  "Stool samples" = "#E69F00",
  "Genitourinary samples" = "#CC79A7",
  "Cerebrospinal fluid samples" = "#56B4E9",
  "Other samples" = "#009E73"
  )


sample_types <- names(okabe_ito)  # All sample types for UI

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
               ),
               mainPanel(plotlyOutput("sitesPlot"),
                         br(),
                         h4(""),
                         dataTableOutput("sitesDataTable")
               )
             )
    ),
    
    tabPanel("By Sample Type",
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput("date_range_type", "Select Report Date Range:",
                                start = min(as.Date(df_wide$`End date`)),
                                end = max(as.Date(df_wide$`End date`)),
                                min = min(as.Date(df_wide$`End date`)),
                                max = max(as.Date(df_wide$`End date`)),
                                format = "yyyy-mm-dd"),
                 
                 radioButtons("view_type", "Select View:",
                              choices = c("Cumulative" = "cumulative",
                                          "Per Quarter" = "quarter"),
                              selected = "cumulative"),
                 
                 radioButtons("type_filter", "Select Site Type:",
                              choices = c("Surveillance", "Reference"),
                              selected = "Surveillance"),
                 
                 checkboxGroupInput("sample_type_select", "Select Sample Types:",
                                    choices = sample_types,
                                    selected = sample_types),
                 
                 checkboxGroupInput("include_values", "Include Values:",
                                    choices = c("Total", "Positive"),
                                    selected = c("Total")),
                 
                 selectInput("file_format_type", "Download Format:",
                             choices = c("PNG" = "png", "PDF" = "pdf")),
                 
                 downloadButton("downloadType", "Download Plot")
               ),
               mainPanel(plotlyOutput("typePlot"),
                         br(),
                         h4(""),
                         dataTableOutput("typeDataTable")
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
  
  
  
  output$sitesDataTable <- DT::renderDataTable({
    table_data <- sites_data() %>%
      select(site, sitecode, type, `End date`, `Total samples`)
    
    if (input$view_sites == "cumulative") {
      table_data <- table_data %>%
        arrange(sitecode, type, `End date`) %>%
        group_by(sitecode, type) %>%
        mutate(`Cumulative samples` = cumsum(replace_na(`Total samples`, 0))) %>%
        ungroup()
    }
    
    table_data
  })
  
  
  # ---------------- Sample Type Tab ----------------
  type_data <- reactive({
    df_wide %>%
      filter(type == input$type_filter) %>%
      filter(`End date` >= input$date_range_type[1],
             `End date` <= input$date_range_type[2]) %>%
      pivot_longer(cols = c(5:18), names_to = "Sample type", values_to = "Samples processed") %>%
      select(1:4, 7:9) %>%
      arrange(reporting.month) %>%
      group_by(`Sample type`, `End date`) %>%
      summarize(`Samples processed` = sum(`Samples processed`, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        positive = ifelse(substr(`Sample type`, 1, 8) == "Positive", "Positive", "Total"),
        `Sample type` = recode(`Sample type`,
                               "Positive blood samples" = "Blood samples",
                               "Positive cerebrospinal fluid samples" = "Cerebrospinal fluid samples",
                               "Positive stool samples" = "Stool samples",
                               "Positive genitourinary samples" = "Genitourinary samples",
                               "Positive pus/pus swab samples" = "Pus/pus swab samples",
                               "Positive urine samples" = "Urine samples",
                               "Positive other samples" = "Other samples",
                               "Blood samples" = "Blood samples",
                               "Cerebrospinal fluid samples" = "Cerebrospinal fluid samples",
                               "Stool samples" = "Stool samples",
                               "Genitourinary samples" = "Genitourinary samples",
                               "Pus/pus swab samples" = "Pus/pus swab samples",
                               "Urine samples" = "Urine samples",
                               "Other samples" = "Other samples"
        )
      ) %>%
      filter(positive %in% input$include_values,
             `Sample type` %in% input$sample_type_select) %>%
      mutate(`Sample type` = factor(`Sample type`, levels = sample_types))
  })
  
  type_plot <- reactive({
    if (input$view_type == "cumulative") {
      plot_data <- type_data() %>%
        filter(!is.na(`Samples processed`)) %>%
        group_by(`Sample type`, positive) %>%
        mutate(`Cumulative samples` = cumsum(replace_na(`Samples processed`, 0))) %>%
        ungroup()
      
      ggplot(plot_data, aes(x = `End date`, y = `Cumulative samples`,
                            color = `Sample type`, linetype = positive)) +
        theme_bw() +
        stat_summary(fun.data = mean_cl_normal, geom = "line") +
        geom_point(aes(color = `Sample type`), size = 2) +
        labs(x = "Report date", y = "Cumulative samples processed") +
        scale_x_date(date_breaks = 'year', date_labels = "%Y") +
        scale_color_manual(values = okabe_ito) +
        scale_linetype_manual(values = c("Total" = "solid", "Positive" = "dashed"), name = "") +
        theme(text = element_text(size = 12))
      
    } else {
      ggplot(type_data(), aes(x = `End date`, y = `Samples processed`,
                              color = `Sample type`, linetype = positive)) +
        theme_bw() +
        stat_summary(fun.data = mean_cl_normal, geom = "line") +
        geom_point(aes(color = `Sample type`), size = 2) +
        labs(x = "Report date", y = "Samples processed per quarter") +
        scale_x_date(date_breaks = 'year', date_labels = "%Y") +
        scale_color_manual(values = okabe_ito) +
        scale_linetype_manual(values = c("Total" = "solid", "Positive" = "dashed"), name = "") +
        theme(text = element_text(size = 12))
    }
  })
  
  output$typePlot <- renderPlotly({
    ggplotly(type_plot(), tooltip = c("x", "y", "color", "linetype"))
  })
  
  output$downloadType <- downloadHandler(
    filename = function() {
      paste("type_plot_", Sys.Date(), ".", input$file_format_type, sep = "")
    },
    content = function(file) {
      ggsave(file, plot = type_plot(), width = 10, height = 6, device = input$file_format_type)
    }
  )
  
  
  output$typeDataTable <- DT::renderDataTable({
    table_data <- type_data() %>%
      select(`Sample type`, `positive`, `End date`, `Samples processed`)
      
    if (input$view_type == "cumulative") {
      table_data <- table_data %>%
        arrange(`Sample type`, `positive`, `End date`) %>%
        group_by(`Sample type`, `positive`) %>%
        mutate(`Cumulative samples` = cumsum(replace_na(`Samples processed`, 0))) %>%
        ungroup()
    }
    
    table_data
  
    
  }, options = list(pageLength = 10, scrollX = TRUE))
  
}

shinyApp(ui = ui, server = server)
