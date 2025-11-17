############ AH Roadmap Analysis Code
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
                       "tidyr", "DescTools", "shiny", "lubridate", "purrr")

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
df <- read_xlsx("Output files/AH/1. AH LSHTM Roadmap Status.xlsx")

## Load in site masterlist to obtain site information:
site_info <- read_xlsx("Resource files/Site masterlist template.xlsx", sheet = "Sentinel Sites") %>% # Replace with your site masterlist .xlsx filepath 
  mutate(`Support start date` = as.Date(`Support start date`),
         `Support end date` = as.Date(`Support end date`))



#################################### Graph output location
## Choose the location into which you would like your HH LSHTM Roadmap graphs
## to go into.
file_save <- "Output files/AH/"

#################################### Sector
## As we are looking at animal health sites, we are going to set this to 
## "Animal Health Plus" so that we can isolate these sites.

sector <- "Animal Health Plus"



#################################### Prepare relevant dataframes

## convert each subcomponent "tier" into the subcomponent's actual name
custom_labels <- c("tier1a" = "1a. Clinical & passive sampling", "tier1b" = "1b. Sampling training & QA",
                   "tier2a" = "2a. Sample transport", "tier2b" = "2b. Sample registration",
                   "tier2c" = "2c. Culture & identification", "tier2d" = "2d. Susceptibility testing",
                   "tier3a" = "3a. Storage of isolates", "tier3b" = "3b. Transport to AMR laboratory", 
                   "tier3c" = "3c. Storage/referral training & QA", 
                   "tier4a" = "4a. Data use", "tier4b" = "4b. Data linkage", "tier4c" = "4c. Data governance")


## Convert to a long format
df_long <- df %>%
  select(!c("core", "extended", "advanced", "core or above")) %>%
  pivot_longer(
    cols = c("tier1a", "tier1b", "tier2a", "tier2b", "tier2c", "tier2d",
             "tier3a", "tier3b", "tier3c", "tier4a", "tier4b", "tier4c"), 
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

write_xlsx(sheets, paste(file_save,"2. AH LSHTM Roadmap proportion masterlist.xlsx", sep=""))



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

# Load module
source("Code files/Shiny app/LSHTM_roadmap_plot_module.R")


# UI
ui <- fluidPage(
  titlePanel("LSHTM roadmap subcomponent status over time"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("metric_type", "Metric Type",
                   choices = c("Number of sites" = "count","Proportion of sites" = "proportion"),
                   selected = "count"),
      
      conditionalPanel(
        condition = "input.metric_type == 'proportion'",
        radioButtons("plot_type", "Plot Type",
                     choices = c("Bar Chart" = "col", "Area Chart" = "area"),
                     selected = "col")
      ),
      
      sliderInput("facet_rows", "Number of Facet Rows", min = 1, max = 6, value = 4),
      sliderInput("plot_width", "Plot Width (inches)", min = 10, max = 25, value = 15),
      sliderInput("plot_height", "Plot Height (inches)", min = 4, max = 15, value = 9),
      downloadButton("download_surv_chart", "Download surveillance sites plot"),
      downloadButton("download_ref_chart", "Download reference sites plot")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Surveillance sites", barChartUI("surv_chart", "Surveillance sites")),
        tabPanel("Reference sites", barChartUI("ref_chart", "Reference sites"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Force plot type to "col" when metric is "count"
  observeEvent(input$metric_type, {
    if (input$metric_type == "count") {
      updateRadioButtons(session, "plot_type", selected = "col")
    }
  })
  
  surv_plot <- barChartServer("surv_chart", df_surv_long, custom_labels,
                              plot_type = reactive(input$plot_type),
                              metric_type = reactive(input$metric_type),
                              plot_width = reactive(input$plot_width),
                              plot_height = reactive(input$plot_height),
                              facet_rows = reactive(input$facet_rows)
  )
  
  ref_plot <- barChartServer("ref_chart", df_ref_long, custom_labels,
                             filter_ref = FALSE,
                             plot_type = reactive(input$plot_type),
                             metric_type = reactive(input$metric_type),
                             plot_width = reactive(input$plot_width),
                             plot_height = reactive(input$plot_height),
                             facet_rows = reactive(input$facet_rows)
  )
  
  output$download_surv_chart <- downloadHandler(
    filename = function() {
      paste0("surv_sites_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(surv_plot())
      ggsave(file, plot = surv_plot(), width = input$plot_width, height = input$plot_height, dpi = 96)
    }
  )
  
  output$download_ref_chart <- downloadHandler(
    filename = function() {
      paste0("ref_sites_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(ref_plot())
      ggsave(file, plot = ref_plot(), width = input$plot_width, height = input$plot_height, dpi = 96)
    }
  )
}

shinyApp(ui, server)












## Graph to demonstrate how many sites are active over time.

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

active_site_time <- rbind(active_surv_site_counts_time, active_ref_site_counts_time) 

sites_over_time <- ggplot(active_site_time, aes(x=`End date`, y = active_site_count, fill = Type, colour = Type)) +
  geom_point()+
  geom_line()+
  xlab("Report period end")+
  ylab("Number of laboratories")+
  scale_x_date(date_labels = "%Y", date_breaks = "year")+
  scale_y_continuous(limits = c(0,max(active_site_time$active_site_count)*1.2))+
  theme(axis.text = element_text(size = 12), strip.text = element_text(size = 12),
        legend.text = element_text(size = 12), axis.title.y = element_text(size = 12))


