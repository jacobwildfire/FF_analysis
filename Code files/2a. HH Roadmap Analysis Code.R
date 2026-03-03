############ HH Roadmap Analysis Code
#######
#######
####### Created by Dr Jacob Wildfire as part of the Fleming Fund



####### Having calculated the status for each LSHTM Roadmap subcomponent,
####### the following code produces visualisations showing the change in the 
####### proportions of sites performing each level of function over time.
#######



# Note: All path examples provide a default
#       that can be run using the files provided
#       in the "FF_analysis" GitHub repository,
#       provided appropriate modifications
#       are made (see README.txt).




#################################### Required packages

# List of required packages
required_packages <- c("ggplot2", "dplyr", "readxl", "writexl", "stringr", 
                       "tidyr", "DescTools", "shiny", "lubridate", "purrr",
                       "DT")

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

## Load up the LSHTM subcomponent source information files
df <- read_xlsx("Output files/HH/1. HH LSHTM Roadmap Status.xlsx")


## Load in site masterlist to obtain site information:
site_info <- read_xlsx("Resource files/Site masterlist template.xlsx", sheet = "Sentinel Sites") %>% # Replace with your site masterlist .xlsx filepath 
  mutate(`Support start date` = as.Date(`Support start date`),
         `Support end date` = as.Date(`Support end date`))



#################################### Graph output location
## Choose the location into which you would like your HH LSHTM Roadmap graphs
## to go into.
file_save <- "Output files/HH/"


#################################### Sector
## As we are looking at human health sites, we are going to set this to 
## "Human Health" so that we can isolate these sites.

sector <- "Human Health"

#################################### Prepare relevant dataframes

## convert each subcomponent "tier" into the subcomponent's actual name
custom_labels <- c("tier1a" = "1a. Clinical admission assessment", "tier1b" = "1b. Clinical data",
                   "tier1c" = "1c. Clinical investigation", "tier1d" = "1d. Clinical training & QA",
                   "tier2a" = "2a. Sample transport", "tier2b" = "2b. Sample registration",
                   "tier2c" = "2c. Culture & identification", "tier2d" = "2d. Susceptibility testing",
                   "tier2e" = "2e. Testing training & QA", "tier3a" = "3a. Storage of isolates",
                   "tier3b" = "3b. Transport to AMR laboratory", "tier3c" = "3c. Isolate storage training & QA", 
                   "tier4a" = "4a. Data use", "tier4b" = "4b. Data linkage", "tier4c" = "4c. Data governance")


## Convert to a long format
df_long <- df %>%
  select(!c("clinical_care","core", "extended", "advanced", "core or above")) %>%
  pivot_longer(
    cols = c("tier1a", "tier1b", "tier1c", "tier1d", "tier2a", "tier2b", "tier2c",
             "tier2d", "tier2e", "tier3a", "tier3b", "tier3c", "tier4a", "tier4b",
             "tier4c"), 
    names_to = "LSHTM subcomponent",
    values_to = "value"
  )

## Determine the different unique reporting periods and assign them a `Start date`
## that maps to the start of the specific reporting period, and an `End date`
## that maps to the end of the reporting period.

date_key <- data.frame(reporting.month = unique(df_long$reporting.month), `Start date` = NA, 
                       `End date` = NA,check.names = FALSE) %>%#
  mutate(
    year = as.numeric(substr(reporting.month, 1, 4)),
    period = substr(reporting.month, 5, 6),
    `Start date` = case_when(
      period == "Q1" ~ as.Date(paste0(year, "-01-01")),
      period == "Q2" ~ as.Date(paste0(year, "-04-01")),
      period == "Q3" ~ as.Date(paste0(year, "-07-01")),
      period == "Q4" ~ as.Date(paste0(year, "-10-01")),
      period == "S1" ~ as.Date(paste0(year, "-01-01")),
      period == "S2" ~ as.Date(paste0(year, "-07-01"))
    ),
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

## In the site_info list, if there is an empty "Support end date" value,
## replace it with the highest reporting end date. This indicates that the site is 
## still active.

site_info[is.na(site_info$`Support end date`),"Support end date"] <- max(unique(date_key$`End date`))

## Next, generate data rames in a list for surveillance and reference sites showing which 
## dates they were active, and therefore should have been reporting, during.
active_sites_list <- list()

for (type in c("Surveillance", "Reference")) {
  active_sites_list[[type]] <- site_info %>%
    filter(Type == type, Sector == sector) %>%
    rowwise() %>%
    mutate(months_active = list(seq(`Support start date`, `Support end date`, by = "month"))) %>%
    unnest(months_active) %>%
    mutate(reporting.month = floor_date(months_active, "month")) %>%
    select(`Site Code`, reporting.month) %>%
    `colnames<-`(c("sitecode", "Start date"))
}


active_surv_site_counts <- map_dfr(1:nrow(date_key), function(i) {
  period_start <- date_key$`Start date`[i]
  period_end <- date_key$`End date`[i]
  reporting_month <- date_key$reporting.month[i]
  
  count <- active_sites_list[["Surveillance"]] %>%
    filter(`Start date` >= period_start, `Start date` <= period_end) %>%
    distinct(sitecode) %>%
    nrow()
  
  tibble(reporting.month = reporting_month, active_site_count = count)
}) %>%
  subset(active_site_count > 0)

active_ref_site_counts <- map_dfr(1:nrow(date_key), function(i) {
  period_start <- date_key$`Start date`[i]
  period_end <- date_key$`End date`[i]
  reporting_month <- date_key$reporting.month[i]
  
  count <- active_sites_list[["Reference"]] %>%
    filter(`Start date` >= period_start, `Start date` <= period_end) %>%
    distinct(sitecode) %>%
    nrow()
  
  tibble(reporting.month = reporting_month, active_site_count = count)
}) %>%
  subset(active_site_count > 0)




# Generate summary dataframes, calculating the proportion of "Precore", "Core", 
## "Extended" and "Advanced" by date and type.
df_surv <- df_long %>%
  filter(type == "Surveillance") %>%
  group_by(reporting.month, `LSHTM subcomponent`) %>%
  summarise(
    at_least_core = sum(value %in% c("Core", "Extended", "Advanced")),
    core = sum(value == "Core"),
    extended = sum(value == "Extended"),
    advanced = sum(value == "Advanced"),
    precore = sum(value == "Precore"),
    not_applicable = sum(value == "Not applicable")
  ) %>%
  left_join(active_surv_site_counts, by = "reporting.month") %>%
  mutate(
    no_answer = active_site_count - at_least_core - precore - not_applicable,
    prop_core_above = at_least_core / active_site_count,
    prop_core = core / active_site_count,
    prop_extended = extended / active_site_count,
    prop_advanced = advanced / active_site_count,
    prop_precore = precore / active_site_count,
    prop_not_applicable = not_applicable / active_site_count,
    prop_no_answer = no_answer / active_site_count
  )


## Proportion of precore, core, extended and advanced by date, reference
df_ref <- df_long %>%
  filter(type == "Reference") %>%
  group_by(reporting.month, `LSHTM subcomponent`) %>%
  summarise(
    at_least_core = sum(value %in% c("Core", "Extended", "Advanced")),
    core = sum(value == "Core"),
    extended = sum(value == "Extended"),
    advanced = sum(value == "Advanced"),
    precore = sum(value == "Precore"),
    not_applicable = sum(value == "Not applicable")
  ) %>%
  left_join(active_ref_site_counts, by = "reporting.month") %>%
  mutate(
    no_answer = active_site_count - at_least_core - precore - not_applicable,
    prop_core_above = at_least_core / active_site_count,
    prop_core = core / active_site_count,
    prop_extended = extended / active_site_count,
    prop_advanced = advanced / active_site_count,
    prop_precore = precore / active_site_count,
    prop_not_applicable = not_applicable / active_site_count,
    prop_no_answer = no_answer / active_site_count
  )




sheets <- list("Prop. surv overall" = df_surv, "Prop. ref overall" = df_ref)

## Produce a new excel document which has the LSHTM Roadmap status numbers of sites and proportions.
write_xlsx(sheets, paste(file_save,"2. HH LSHTM Roadmap proportion masterlist.xlsx", sep=""))


## Produce a longer form of the datasets, producing a long version of the number of sites
## and then the proportion of sites
df_surv_long <- df_surv %>%
  pivot_longer(cols = c(prop_precore, prop_core, prop_extended, prop_advanced, prop_not_applicable, prop_no_answer), names_to = "Level", values_to = "Proportion") %>%
  select(1:2,12:13) %>%
  mutate(Level = ifelse(Level == 'prop_no_answer',"no_answer", ifelse(
    Level == "prop_not_applicable", "not_applicable", ifelse(
      Level == "prop_advanced", "advanced", ifelse(
        Level == "prop_extended", "extended", ifelse(
          Level == "prop_core", "core", "precore"
        )
      )
    )
  )))%>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable",
                                          "no_answer"))) %>%
  arrange(reporting.month,`LSHTM subcomponent`, Level)

df_surv_long_sites <- df_surv %>%
  pivot_longer(cols = c("precore",
                        "core",
                        "extended",
                        "advanced",
                        "not_applicable",
                        "no_answer"), names_to = "Level", values_to = "Sites") %>%
  select(1:2,12:13) %>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable",
                                          "no_answer"))) %>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable",
                                          "no_answer"))) %>%
  arrange(reporting.month,`LSHTM subcomponent`, Level)


## Then, combine these two datasets, ensuring that they are sorted the same way and
## therefore that the columns are being matched correctly.
if (all(df_surv_long$reporting.month == df_surv_long_sites$reporting.month) & 
    all(df_surv_long$`LSHTM subcomponent` == df_surv_long_sites$`LSHTM subcomponent`) & 
    all(df_surv_long$Level == df_surv_long_sites$Level)) {
df_surv_long <- data.frame(df_surv_long_sites, Proportion = df_surv_long$Proportion, check.names = FALSE)
} else {
  warning("Error: mismatch in the column order of df_surv_long and df_surv_long_sites. 
One or more of the columns of reporting.month, `LSHTM subcomponent` or Level are not in the same order between the datasets.")
}

## Repeat for reference sites.
df_ref_long <- df_ref %>%
  pivot_longer(cols = c(prop_precore, prop_core, prop_extended, prop_advanced, prop_not_applicable, prop_no_answer), names_to = "Level", values_to = "Proportion") %>%
  select(1:2,12:13) %>%
  mutate(Level = ifelse(Level == 'prop_no_answer',"no_answer", ifelse(
    Level == "prop_not_applicable", "not_applicable", ifelse(
      Level == "prop_advanced", "advanced", ifelse(
        Level == "prop_extended", "extended", ifelse(
          Level == "prop_core", "core", "precore"
        )
      )
    )
  )))%>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable",
                                          "no_answer"))) %>%
  arrange(reporting.month,`LSHTM subcomponent`, Level)

df_ref_long_sites <- df_ref %>%
  pivot_longer(cols = c("precore",
                        "core",
                        "extended",
                        "advanced",
                        "not_applicable",
                        "no_answer"), names_to = "Level", values_to = "Sites") %>%
  select(1:2,12:13) %>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable",
                                          "no_answer")))%>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable",
                                          "no_answer"))) %>%
  arrange(reporting.month,`LSHTM subcomponent`, Level)



if (all(df_ref_long$reporting.month == df_ref_long_sites$reporting.month) & 
    all(df_ref_long$`LSHTM subcomponent` == df_ref_long_sites$`LSHTM subcomponent`) & 
    all(df_ref_long$Level == df_ref_long_sites$Level)) {
  df_ref_long <- data.frame(df_ref_long_sites, Proportion = df_ref_long$Proportion, check.names = FALSE)
} else {
  warning("Error: mismatch in the column order of df_ref_long and df_surv_long_sites. 
One or more of the columns of reporting.month, `LSHTM subcomponent` or Level are not in the same order between the datasets.")
}

rm(df_surv_long_sites, df_ref_long_sites)

## Use the YYYYS# reporting.month date format to produce a function that produces
## date columns with the report date start and end:

date_conversion <- function(df) {
  df<-df %>% mutate(
    year = as.numeric(substr(reporting.month, 1, 4)),
    period = substr(reporting.month, 5, 6),
    `Start date` = case_when(
      period == "Q1" ~ as.Date(paste0(year, "-01-01")),
      period == "Q2" ~ as.Date(paste0(year, "-04-01")),
      period == "Q3" ~ as.Date(paste0(year, "-07-01")),
      period == "Q4" ~ as.Date(paste0(year, "-10-01")),
      period == "S1" ~ as.Date(paste0(year, "-01-01")),
      period == "S2" ~ as.Date(paste0(year, "-07-01"))
    ),
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

  return(df)
  
}

# Apply this function to the df_surv_long dataset.
df_surv_long <- date_conversion(df_surv_long)%>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable",
                                          "no_answer")))


# Conversion of ref dataset.
df_ref_long <- date_conversion(df_ref_long)%>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable",
                                          "no_answer")))

## Then, convert the dataframe to provide a ymin and ymax for each "Level"
## category at each reporting date, allowing the data to be plotted using
## geom_rect. This is important, as it then allows the data to be plotted 
## even if the reporting timeframe changes.

df_surv_long <- df_surv_long %>%
  group_by(reporting.month, `LSHTM subcomponent`) %>%
  arrange(Level) %>%
  mutate(
    ymin = cumsum(lag(Proportion, default = 0)),
    ymax = cumsum(Proportion),
    n.ymin = cumsum(lag(Sites, default = 0)),
    n.ymax = cumsum(Sites)
  )%>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable",
                                          "no_answer")))%>%
  arrange(reporting.month, `LSHTM subcomponent`, Level)




df_ref_long <- df_ref_long %>%
  group_by(reporting.month, `LSHTM subcomponent`) %>%
  arrange(Level) %>%
  mutate(
    ymin = cumsum(lag(Proportion, default = 0)),
    ymax = cumsum(Proportion),
    n.ymin = cumsum(lag(Sites, default = 0)),
    n.ymax = cumsum(Sites)
  )%>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable",
                                          "no_answer")))%>%
  arrange(reporting.month, `LSHTM subcomponent`, Level)








#################################### Producing plots

## The following Shiny app allows for you to produce data plots dynamically,
## allowing you to change the size of the plots however you like.


source("Code files/Shiny app/LSHTM_roadmap_plot_module.R")

# Build choiceNames as a list of HTML tags (to render symbols nicely)
choice_names <- lapply(
  # decode &amp; -> & for UI text
  gsub("&amp;", "&", unname(custom_labels), fixed = TRUE),
  htmltools::HTML
)
# Values (what the server receives): the tier codes
choice_values <- names(custom_labels)

# ✅ Plot facet labels (plain strings for ggplot)
custom_labels_plot <- gsub("&amp;", "&", custom_labels, fixed = TRUE)

# ---- UI ----
ui <- fluidPage(
  titlePanel("Subcomponent status over time"),
  
  tags$head(
    tags$script(HTML("
      Shiny.addCustomMessageHandler('updatePlotHeight', function(h) {
        var el1 = document.getElementById('plot_container');
        if (el1) el1.style.minHeight = h + 'px';
        var el2 = document.getElementById('plot_container_first_last');
        if (el2) el2.style.minHeight = h + 'px';
      });
    ")),
    tags$style(HTML("
      #plot_container, #plot_container_first_last { min-height: 400px; }
      #plot_container, #plot_container_first_last, #table_container_ot, #table_container_fl {
        display: block !important; width: 100% !important; clear: both !important;
      }
      #table_container_ot, #table_container_fl { margin-top: 20px !important; position: relative !important; z-index: 1 !important; }
      .dataTables_wrapper { clear: both !important; overflow: visible !important; position: relative !important; z-index: 1 !important; }
      .shiny-plot-output { display: block !important; clear: both !important; width: 100% !important; }
    "))
  ),
  
  tabsetPanel(
    # ---------------- Over time ----------------
    tabPanel(
      "Over time",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            "ot_site_type", "Site type",
            choices = c("Surveillance sites" = "surv", "Reference sites" = "ref"),
            selected = "surv"
          ),
          checkboxGroupInput(
            inputId = "ot_selected_tiers",
            label   = "Subcomponents (tiers)",
            choiceNames  = choice_names,
            choiceValues = choice_values,
            selected     = choice_values
          ),
          radioButtons(
            "ot_metric_type", "Metric Type",
            choices = c("Number of sites" = "count", "Proportion of sites" = "proportion"),
            selected = "count"
          ),
          conditionalPanel(
            condition = "input.ot_metric_type == 'proportion'",
            radioButtons(
              "ot_plot_type", "Plot Type",
              choices = c("Bar Chart" = "col", "Area Chart" = "area"),
              selected = "col"
            )
          ),
          sliderInput("ot_facet_rows",  "Number of Facet Rows", 1, 6, 4),
          sliderInput("ot_plot_width",  "Plot Width (inches)",   10, 25, 15),
          sliderInput("ot_plot_height", "Plot Height (inches)",   4, 15,  9),
          radioButtons(
            "ot_download_format", "Download format",
            choices = c("PNG" = "png", "PDF" = "pdf"),
            inline = TRUE, selected = "png"
          ),
          downloadButton("download_plot_ot", "Download plot")
        ),
        mainPanel(
          div(id = "plot_container", barChartUI("main_chart")),
          div(style = "height: 40px;"),
          div(id = "table_container_ot", DT::dataTableOutput("sitesDataTable"))
        )
      )
    ),
    
    # ------------- First vs Latest -------------
    tabPanel(
      "First vs last report",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            "fl_site_type", "Site type",
            choices = c("Surveillance sites" = "surv", "Reference sites" = "ref"),
            selected = "surv"
          ),
          checkboxGroupInput(
            inputId = "fl_selected_tiers",
            label   = "Subcomponents (tiers)",
            choiceNames  = choice_names,
            choiceValues = choice_values,
            selected     = choice_values
          ),
          radioButtons(
            "fl_plot_type", "Plot Type",
            choices = c("Connected Dot Chart" = "dot_plot",
                        "Bar Chart" = "col"),
            selected = "dot_plot"
          ),
          
          radioButtons(
            "fl_core_or_above_collection",
            "Combine Levels",
            choices = c("Yes" = "core_or_above", "No" = "core_extended_advanced"),
            selected = "core_or_above"
          ),
          
          sliderInput("fl_facet_rows",  "Number of Facet Rows", 1, 6, 3),
          sliderInput("fl_plot_width",  "Plot Width (inches)",  10, 25, 15),
          sliderInput("fl_plot_height", "Plot Height (inches)",  4, 15,  9),
          radioButtons(
            "fl_download_format", "Download format",
            choices = c("PNG" = "png", "PDF" = "pdf"),
            inline = TRUE, selected = "png"
          ),
          downloadButton("download_plot_fl", "Download plot")
        ),
        mainPanel(
          div(id = "plot_container_first_last", firstLastUI("first_last_chart")),
          div(style = "height: 40px;"),
          div(id = "table_container_fl", DT::dataTableOutput("firstLastDataTable"))
        )
      )
    )
  )
)


# ---- Server ----
server <- function(input, output, session) {
  
  # Keep both plots responsive to height slider(s)
  observe({
    # choose which slider to use: here we use max of both to be safe
    h <- max(input$ot_plot_height %||% 9, input$fl_plot_height %||% 9)
    session$sendCustomMessage("updatePlotHeight", h * 96)
  })
  
  # --- Load once ---
  sites_raw <- readxl::read_xlsx("Output files/HH/1b. HH site status - All questions.xlsx") %>%
    `colnames<-`(c(
      "site", "sitecode", "type", "reporting.month", "clinical_care",
      "tier1a1 (SP&M: 1a1)", "tier1a2 (SP&M: 1a2)", "tier1a3 (SP&M: 1a3)", "tier1a",
      "tier1b1 (SP&M: 1b1)", "tier1b2 (SP&M: 1b5)", "tier1b",
      "tier1c1 (SP&M: 1c2)", "tier1c2 (SP&M: 1c3)", "tier1c3 (SP&M: 1c1)", "tier1c",
      "tier1d1 (SP&M: 2a1)", "tier1d2 (SP&M: 2a2)", "tier1d3 (SP&M: 2a3)", "tier1d4 (SP&M: 2a12)", "tier1d",
      "tier2a1 (SP&M: 3a1)", "tier2a2 (SP&M: 3a2)", "tier2a3 (SP&M: 3a3)", "tier2a",
      "tier2b (SP&M: 3b1)",
      "tier2c1 (SP&M: 3c1)", "tier2c2csf (SP&M: 3c2)", "tier2c2urine (SP&M: 3c3)",
      "tier2c2stool (SP&M: 3c4)", "tier2c2swab (SP&M: 3c5)", "tier2c2genit (SP&M: 3c6)",
      "tier2c3strep (SP&M: 3c7)", "tier2c3staph (SP&M: 3c8)", "tier2c3ecoli (SP&M: 3c9)",
      "tier2c3kleb (SP&M: 3c10)", "tier2c3acine (SP&M: 3c11)", "tier2c3salmonella (SP&M: 3c12)",
      "tier2c3shigella (SP&M: 3c13)", "tier2c3ngonor (SP&M: 3c14)", "tier2c3pseudom (SP&M: 3c17)",
      "tier2c3styphi (SP&M: 3c18)", "tier2c3spara (SP&M: 3c19)", "tier2c3nmening (SP&M: 3c20)",
      "tier2c4 (SP&M: 3c15)", "tier2c",
      "tier2d1 (SP&M: 3d1)", "tier2d2 (SP&M: 3d2)", "tier2d3 (SP&M: 3d3)", "tier2d",
      "tier2e1 (SP&M: 3e1)", "tier2e2 (SP&M: 3e2)", "tier2e",
      "tier3a1 (SP&M: 4a1)", "tier3a2 (SP&M: 4a2)", "tier3a3 (SP&M: 4a3)", "tier3a4 (SP&M: 4a4)", "tier3a",
      "tier3b1 (SP&M: 4b1)", "tier3b2 (SP&M: 4b2)", "tier3b",
      "tier3c1 (SP&M: 4c1)", "tier3c2 (SP&M: 4c2)", "tier3c",
      "tier4a1 (SP&M: 5a1)", "tier4a2 (SP&M: 5a2)", "tier4a3local (SP&M: 5a6)", "tier4a3int (SP&M: 5a7)",
      "tier4a", "tier4b (SP&M: 5b)", "tier4c (SP&M: 5c)"
    ))
  
  # --- Helper: build list of selected tier columns for the DataTable (Over time tab only) ---
  selected_columns <- reactive({
    
    req(input$ot_selected_tiers)
    base_cols <- intersect(c("site", "sitecode", "type", "reporting.month"), names(sites_raw))
    pattern   <- paste0("^(", paste(input$ot_selected_tiers, collapse = "|"), ")(\\d+)?")
    tier_cols <- names(sites_raw)[grepl(pattern, names(sites_raw))]
    extra_cols <- character(0)
    if ("tier2e" %in% input$ot_selected_tiers || "tier3c" %in% input$ot_selected_tiers) {
      extra_cols <- c(extra_cols, names(sites_raw)[startsWith(names(sites_raw), "tier2a3")])
    }
    unique(c(base_cols, tier_cols, extra_cols))
  })
  
  # --- DataTable for Over time ---
  
  
  sites_data <- reactive({
    cols <- selected_columns()
    validate(need(length(cols) > 0, "No matching tier columns found in the Excel for the current selection."))
    
    
    filtered_sites <- if (input$ot_site_type == "surv") {
      sites_raw %>% dplyr::filter(type == "Surveillance")
    } else {
      sites_raw %>% dplyr::filter(type == "Reference")
    }
    
    # Then arrange + select the chosen columns
    filtered_sites %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(intersect(c("sitecode", "reporting.month"), names(sites_raw))))) %>%
      dplyr::select(dplyr::all_of(cols))
  })
  
  output$sitesDataTable <- DT::renderDataTable({
    DT::datatable(
      sites_data(),
      rownames = FALSE,
      options = list(pageLength = 25, scrollX = TRUE, dom = "Bfrtip")
    )
  })
  
  # --- Over time plot module ---
  observeEvent(input$ot_metric_type, {
    if (input$ot_metric_type == "count") {
      updateRadioButtons(session, "ot_plot_type", selected = "col")
    }
  }, ignoreInit = TRUE)
  
  plot_data_ot <- reactive({
    req(input$ot_selected_tiers)
    base_df <- if (input$ot_site_type == "surv") df_surv_long else df_ref_long
    base_df %>% dplyr::filter(`LSHTM subcomponent` %in% input$ot_selected_tiers)
  })
  
  current_plot_ot <- barChartServer(
    id            = "main_chart",
    data          = plot_data_ot,
    custom_labels = custom_labels_plot,
    filter_ref    = FALSE,
    plot_type     = reactive(input$ot_plot_type),
    metric_type   = reactive(input$ot_metric_type),
    plot_width    = reactive(input$ot_plot_width),
    plot_height   = reactive(input$ot_plot_height),
    facet_rows    = reactive(input$ot_facet_rows)
  )
  
  output$download_plot_ot <- downloadHandler(
    filename = function() {
      paste0(input$ot_site_type, "_sites_plot_", Sys.Date(), ".", input$ot_download_format)
    },
    content = function(file) {
      ggsave(
        filename = file,
        plot     = current_plot_ot(),
        width    = input$ot_plot_width,
        height   = input$ot_plot_height,
        device   = input$ot_download_format,
        dpi      = if (input$ot_download_format == "png") 96 else NULL
      )
    }
  )
  
  # --- First vs Latest: data + table ---
  first_last_data <- reactive({
    
    base <- if (input$fl_site_type == "surv") {
      df_long %>% dplyr::filter(type == "Surveillance")
    } else {
      df_long %>% dplyr::filter(type == "Reference")
    }
    req(input$fl_selected_tiers)
    base %>% dplyr::filter(`LSHTM subcomponent` %in% input$fl_selected_tiers)
  })
  
  # Optional simple table to show the counts at first/last by level & subcomponent
  output$firstLastDataTable <- DT::renderDataTable({
    df <- first_last_data()
    validate(need(nrow(df) > 0, "No data available for the current selection."))
    # Light summary view
    
    df2 <- date_conversion(df)
    
    df_start <- df2 %>%
      dplyr::arrange(`Start date`) %>%
      dplyr::group_by(sitecode, `LSHTM subcomponent`) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(timepoint = "First report")
    
    df_end <- df2 %>%
      dplyr::arrange(`Start date`) %>%
      dplyr::group_by(sitecode, `LSHTM subcomponent`) %>%
      dplyr::slice(dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(timepoint = "Final report")
    
    dplyr::bind_rows(df_start, df_end) %>%
      select(site,sitecode,type,`LSHTM subcomponent`,timepoint,value) %>%
      tidyr::pivot_wider(names_from = timepoint, values_from = value) %>%
      dplyr::arrange(`LSHTM subcomponent`) %>%
      DT::datatable(options = list(pageLength = 25, scrollX = TRUE))
  })
  
  # --- First vs Latest plot module ---
  
  current_plot_fl <- firstLastServer(
    id                        = "first_last_chart",
    data                      = first_last_data,
    custom_labels             = custom_labels_plot,
    plot_width                = reactive(input$fl_plot_width),
    plot_height               = reactive(input$fl_plot_height),
    facet_rows                = reactive(input$fl_facet_rows),
    plot_type                 = reactive(input$fl_plot_type),
    core_or_above_collection  = reactive(input$fl_core_or_above_collection)
  )
  
  output$download_plot_fl <- downloadHandler(
    filename = function() {
      paste0("first_last_plot_", input$fl_site_type, "_", Sys.Date(), ".", input$fl_download_format)
    },
    content = function(file) {
      ggsave(
        filename = file,
        plot     = current_plot_fl(),
        width    = input$fl_plot_width,
        height   = input$fl_plot_height,
        device   = input$fl_download_format,
        dpi      = if (input$fl_download_format == "png") 96 else NULL
      )
    }
  )
}


shinyApp(ui, server)




#################################### Plot of sites active over time

## The following code allows you to produce a plot of the number of sites that
## have been active over time. This allows for patterns in the data to be better
## explained.

## Graph to demonstrate how many surveillance sites are active over time.

## Surveillance site dataframe.
active_surv_site_counts_time <- active_surv_site_counts %>%
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
  select(!c("year", "period", "reporting.month"))

active_surv_site_counts_time$Type <- "Surveillance"
  

## Reference site dataframe.
active_ref_site_counts_time <- active_ref_site_counts %>%
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
  select(!c("year", "period", "reporting.month"))

active_ref_site_counts_time$Type <- "Reference"


## Binding of both site types together.
active_site_time <- rbind(active_surv_site_counts_time, active_ref_site_counts_time) 

## Production of the site over time graph.
sites_over_time <- ggplot(active_site_time, aes(x=`End date`, y = active_site_count, fill = Type, colour = Type)) +
  geom_point()+
  geom_line()+
  xlab("Report period end")+
  ylab("Number of laboratories")+
  scale_x_date(date_labels = "%Y", date_breaks = "year")+
  scale_y_continuous(limits = c(0,max(active_site_time$active_site_count)*1.2))+
  theme(axis.text = element_text(size = 12), strip.text = element_text(size = 12),
        legend.text = element_text(size = 12), axis.title.y = element_text(size = 12))


