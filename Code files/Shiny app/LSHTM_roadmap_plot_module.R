barChartUI <- function(id, title = NULL) {
  ns <- NS(id)
  tagList(
    if (!is.null(title)) h3(title),
    plotOutput(ns("bar_plot"))
  )
}


barChartServer <- function(
    id,
    data,                   # reactive(df)
    custom_labels,          # named character vector: names=tier codes, values=labels (plain text)
    filter_ref = FALSE,
    plot_width,
    plot_height,
    facet_rows,
    plot_type,
    metric_type
) {
  moduleServer(id, function(input, output, session) {
    
    plot_data <- reactive({
      
      df <- data()    # dereference reactive
      
      if (filter_ref) {
        df <- subset(
          df,
          !(`LSHTM subcomponent` %in% c("tier1a", "tier1b", "tier1c"))
        )
      }
      
      # Determine plot type safely (handle NULL when input is hidden)
      ptype <- plot_type()
      if (is.null(ptype) || !ptype %in% c("col", "area")) ptype <- "col"
      
      # Control stacking order based on plot type
      if (ptype == "area") {
        df$Level <- factor(df$Level, levels = c(
          "no_answer",
          "not_applicable",
          "advanced",
          "extended",
          "core",
          "precore"
        ))
      } else {
        df$Level <- factor(df$Level, levels = c(
          "precore",
          "core",
          "extended",
          "advanced",
          "not_applicable",
          "no_answer"
        ))
      }
      
      df
    })
    
    plot_reactive <- reactive({
      
      df <- plot_data()
      
      # Friendly validation to avoid ggplot facet crash on empty data
      validate(
        need(nrow(df) > 0, "No data available for the selected tiers.")
      )
      
      # Safe plot/metric type handling
      ptype <- plot_type()
      if (is.null(ptype) || !ptype %in% c("col", "area")) ptype <- "col"
      
      mtype <- metric_type()
      if (is.null(mtype) || !mtype %in% c("count", "proportion")) mtype <- "count"
      
      # Choose layer
      geom_layer <- if (ptype == "col") {
        if (mtype == "count") {
          geom_rect(aes(
            xmin = as.Date(`Start date`) - 0.5,
            xmax = as.Date(`End date`) + 0.5,
            ymin = n.ymin,
            ymax = n.ymax,
            fill = Level
          ))
        } else {
          geom_rect(aes(
            xmin = as.Date(`Start date`) - 0.5,
            xmax = as.Date(`End date`) + 0.5,
            ymin = ymin,
            ymax = ymax,
            fill = Level
          ))
        }
      } else {
        geom_area(aes(
          x = as.Date(`End date`),
          y = Proportion,
          fill = Level
        ))
      }
      
      y_label <- if (mtype == "count" && ptype == "col") {
        "Number of sites"
      } else {
        "Proportion of sites"
      }
      
      ggplot(df) +
        geom_layer +
        facet_wrap(
          ~ `LSHTM subcomponent`,
          labeller = labeller(`LSHTM subcomponent` = custom_labels),
          nrow = facet_rows()
        ) +
        scale_fill_manual(
          name = "Functional level",
          values = c(
            no_answer = "white",
            not_applicable = "grey",
            advanced = "#FDE725",
            extended = "#B6D443",
            core = "#70C261",
            precore = "#440154"
          ),
          breaks = c(
            "no_answer",
            "not_applicable",
            "advanced",
            "extended",
            "core",
            "precore"
          ),
          labels = c(
            "No answer",
            "Not applicable",
            "Advanced",
            "Extended",
            "Core",
            "Precore"
          )
        ) +
        xlab("") +
        ylab(y_label) +
        scale_y_continuous(
          labels = if (mtype == "proportion") scales::percent_format()
          else waiver()
        ) +
        scale_x_date(
          date_labels = "%Y",
          date_breaks = "year"
        ) +
        theme(
          axis.text = element_text(size = 12),
          strip.text = element_text(size = 12),
          legend.text = element_text(size = 12),
          axis.title.y = element_text(size = 12)
        )
    })
    
    output$bar_plot <- renderPlot(
      plot_reactive(),
      width  = function() plot_width() * 96,
      height = function() plot_height() * 96
    )
    
    return(plot_reactive)
  })
}
