barChartUI <- function(id, title = "Bar Chart") {
  ns <- NS(id)
  tagList(
    h3(title),
    plotOutput(ns("bar_plot"))
  )
}

barChartServer <- function(id, data, custom_labels, filter_ref = FALSE,
                           plot_width, plot_height, facet_rows, plot_type, metric_type) {
  moduleServer(id, function(input, output, session) {
    
    plot_data <- reactive({
      if (filter_ref) {
        data <- subset(data, !(`LSHTM subcomponent` %in% c("tier1a", "tier1b", "tier1c")))
      }
      
      
      # Reverse factor levels only if using geom_area
      if (plot_type() == "area") {
        data$Level <- factor(data$Level, levels = c(
          "no_answer",
          "not_applicable",
          "advanced",
          "extended",
          "core",
          "precore"
        ))
      } else {
        data$Level <- factor(data$Level, levels = c(
          "precore",
          "core",
          "extended",
          "advanced",
          "not_applicable",
          "no_answer"
        ))
      }
      data
    })
    
    plot_reactive <- reactive({
      df <- plot_data()
      
      geom_layer <- if (plot_type() == "col") {
        if (metric_type() == "count") {
          geom_rect(aes(xmin = as.Date(`Start date`) - 0.5,
                        xmax = as.Date(`End date`) + 0.5,
                        ymin = n.ymin, ymax = n.ymax, fill = Level))
        } else {
          geom_rect(aes(xmin = as.Date(`Start date`) - 0.5,
                        xmax = as.Date(`End date`) + 0.5,
                        ymin = ymin, ymax = ymax, fill = Level))
        }
      } else {
        geom_area(aes(x = as.Date(`End date`),
                      y = Proportion, fill = Level))
      }
      
      y_label <- if (metric_type() == "count" && plot_type() == "col") {
        "Number of sites"
      } else {
        "Proportion of sites"
      }
      
      ggplot(df) +
        geom_layer +
        facet_wrap(~ `LSHTM subcomponent`,
                   labeller = labeller(`LSHTM subcomponent` = custom_labels),
                   nrow = facet_rows()) +
        scale_fill_manual(name = "Functional level",
          values = c(
            no_answer = "white",
            not_applicable = "grey",
            advanced = "#FDE725",
            extended = "#B6D443",
            core = "#70C261",
            precore = "#440154"
          ),
          breaks = c('no_answer','not_applicable', 'advanced', 'extended', 'core', 'precore'),
          labels = c(
            no_answer = "No answer",
            not_applicable = "Not applicable",
            advanced = "Advanced",
            extended = "Extended",
            core = "Core",
            precore = "Precore"
          )
        ) +
        xlab("") +
        ylab(y_label) +
        scale_y_continuous(labels = if (metric_type() == "proportion") scales::percent_format() else waiver()) +
        scale_x_date(date_labels = "%Y", date_breaks = "year") +
        theme(
          axis.text = element_text(size = 12),
          strip.text = element_text(size = 12),
          legend.text = element_text(size = 12),
          axis.title.y = element_text(size = 12)
        )
    })
    
    output$bar_plot <- renderPlot({
      plot_reactive()
    }, width = function() { plot_width() * 96 },
    height = function() { plot_height() * 96 })
    
    return(plot_reactive)
  })
}
