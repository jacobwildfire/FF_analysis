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


firstLastUI <- function(id, title = NULL) {
  ns <- NS(id)
  tagList(
    if (!is.null(title)) h3(title),
    plotOutput(ns("first_last_plot"))
  )
}
firstLastServer <- function(
    id,
    data,                   # reactive(df_long subset)
    custom_labels,          # named vector for labeller
    plot_width,
    plot_height,
    facet_rows,
    plot_type,              # reactive('dot_plot'|'col')
    core_or_above_collection  # reactive('core_or_above'|'core_extended_advanced')
) {
  moduleServer(id, function(input, output, session) {
    
    # --- Helpers ---
    date_conversion <- function(df) {
      df %>%
        dplyr::mutate(
          year   = as.numeric(substr(reporting.month, 1, 4)),
          period = substr(reporting.month, 5, 6),
          `Start date` = dplyr::case_when(
            period == "Q1" ~ as.Date(paste0(year, "-01-01")),
            period == "Q2" ~ as.Date(paste0(year, "-04-01")),
            period == "Q3" ~ as.Date(paste0(year, "-07-01")),
            period == "Q4" ~ as.Date(paste0(year, "-10-01")),
            period == "S1" ~ as.Date(paste0(year, "-01-01")),
            period == "S2" ~ as.Date(paste0(year, "-07-01"))
          ),
          `End date` = dplyr::case_when(
            period == "Q1" ~ as.Date(paste0(year, "-03-31")),
            period == "Q2" ~ as.Date(paste0(year, "-06-30")),
            period == "Q3" ~ as.Date(paste0(year, "-09-30")),
            period == "Q4" ~ as.Date(paste0(year, "-12-31")),
            period == "S1" ~ as.Date(paste0(year, "-06-30")),
            period == "S2" ~ as.Date(paste0(year, "-12-31"))
          )
        ) %>%
        dplyr::select(-year, -period)
    }
    
    build_start_end <- function(df) {
      df2 <- date_conversion(df)
      
      df_start <- df2 %>%
        dplyr::arrange(`Start date`) %>%
        dplyr::group_by(sitecode, `LSHTM subcomponent`) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(timepoint = "Baseline")
      
      df_end <- df2 %>%
        dplyr::arrange(`Start date`) %>%
        dplyr::group_by(sitecode, `LSHTM subcomponent`) %>%
        dplyr::slice(dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(timepoint = "End")
      
      dplyr::bind_rows(df_start, df_end)
    }
    
    # --- Plot reactive ---
    plot_reactive <- reactive({
      df <- data()
      validate(need(nrow(df) > 0, "No data available for the selected tiers."))
      
      # Read inputs with safe defaults
      ptype   <- plot_type()
      if (is.null(ptype)) ptype <- "dot_plot"
      
      combine <- core_or_above_collection()
      if (is.null(combine)) combine <- "core_or_above"
      
      # Compute first/last snapshot
      df_se <- build_start_end(df)
      
      
      # ---------- DOT PLOT DATA (exclude Not applicable) ----------
      df_se_segments <- df_se %>%
        dplyr::filter(value != "Not applicable") %>%
        dplyr::mutate(timepoint = factor(timepoint, levels = c("Baseline", "End")))
      
      if (combine == "core_or_above") {
        df_se_segments <- df_se_segments %>%
          dplyr::mutate(
            value = ifelse(value %in% c("Core", "Extended", "Advanced"),
                           "Core or above", "Precore"),
            value = factor(value, levels = c("Precore", "Core or above"))
          )
      } else { # show the four levels
        df_se_segments <- df_se_segments %>%
          dplyr::mutate(
            value = factor(value, levels = c("Precore", "Core", "Extended", "Advanced"))
          )
      }
      
      segments <- df_se_segments %>%
        dplyr::arrange(sitecode, timepoint) %>%
        dplyr::group_by(sitecode, `LSHTM subcomponent`) %>%
        dplyr::mutate(
          next_timepoint = dplyr::lead(timepoint),
          next_value     = dplyr::lead(value)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::filter(timepoint == "Baseline" & next_timepoint == "End") %>%
        dplyr::group_by(`LSHTM subcomponent`, timepoint, value, next_timepoint, next_value) %>%
        dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
        dplyr::mutate(
          direction = factor(
            dplyr::case_when(
              as.numeric(next_value) > as.numeric(value) ~ "Increase",
              as.numeric(next_value) < as.numeric(value) ~ "Decrease",
              TRUE                                       ~ "No change"
            ),
            levels = c("No change", "Decrease", "Increase")
          )
        )
      
      # ---------- BAR CHART DATA (include Not applicable) ----------
      text_df_bars <- df_se %>%
        dplyr::mutate(timepoint = factor(timepoint, levels = c("Baseline", "End")))
      
      if (combine == "core_or_above") {
        text_df_bars <- text_df_bars %>%
          dplyr::mutate(
            value = dplyr::case_when(
              value %in% c("Core", "Extended", "Advanced") ~ "Core or above",
              TRUE                                         ~ value
            ),
            # Include Not applicable in levels and bars
            value = factor(value, levels = c("Not applicable","Core or above","Precore"))
          )
        
        fill_scale <- scale_fill_manual(
          values = c(`Core or above` = "#70C261",
                     Precore         = "#440154FF",
                     `Not applicable`= "grey"),
          breaks = c("Not applicable","Core or above","Precore"),
          name   = "Levels"
        )
      } else {
        text_df_bars <- text_df_bars %>%
          dplyr::mutate(
            value = factor(value, levels = c("Not applicable","Advanced","Extended","Core","Precore"))
          )
        
        fill_scale <- scale_fill_manual(
          values = c(Advanced = "#FDE725FF",
                     Extended = "#B6D443",
                     Core     = "#70C261",
                     Precore  = "#440154FF",
                     `Not applicable` = "grey"),
          breaks = c("Not applicable","Advanced","Extended","Core","Precore"),
          name   = "Levels"
        )
      }
      
      # Aggregate counts for bars
      text_df_bars <- text_df_bars %>%
        dplyr::group_by(timepoint, `LSHTM subcomponent`, value) %>%
        dplyr::summarise(count = dplyr::n(), .groups = "drop")
      
      # ---------- Plot variants ----------
      if (ptype == "dot_plot") {
        
        ggplot() +
          facet_wrap(
            ~ `LSHTM subcomponent`,
            labeller = labeller(`LSHTM subcomponent` = custom_labels),
            nrow = facet_rows()
          ) +
          geom_segment(
            data = segments %>% dplyr::arrange(direction),
            aes(
              x = timepoint, y = value,
              xend = next_timepoint, yend = next_value,
              size = count, color = direction
            ),
            alpha = 0.6, lineend = "round"
          ) +
          geom_count(
            data = df_se_segments,
            aes(x = timepoint, y = value)
          ) +
          scale_size_continuous(range = c(1, 7), name = "Number\nof sites") +
          scale_color_manual(
            values = c("Increase" = "lightblue", "Decrease" = "lightpink", "No change" = "grey80"),
            name = "Change\ndirection"
          ) +
          theme_bw() +
          scale_y_discrete(name = "Level") +
          scale_x_discrete(
            limits = c("Baseline", "End"),
            expand = c(0.2, 0.1),
            labels = c("Baseline" = "First report", "End" = "Last report"),
            name = "Site reporting timepoint"
          ) +
          # Optional labels next to dots (uses df_se_segments which excludes NA)
          {
            text_df_segments <- df_se_segments %>%
              dplyr::group_by(timepoint, `LSHTM subcomponent`, value) %>%
              dplyr::summarise(count = dplyr::n(), .groups = "drop")
            list(
              geom_text(
                aes(label = count, y = value, x = timepoint),
                position = position_nudge(x = -0.21),
                data = subset(text_df_segments, timepoint == "Baseline"),
                hjust = 0, size = 4
              ),
              geom_text(
                aes(label = count, y = value, x = timepoint),
                position = position_nudge(x = +0.10),
                data = subset(text_df_segments, timepoint == "End"),
                hjust = 0, size = 4
              )
            )
          } +
          theme(
            axis.text   = element_text(size = 12),
            strip.text  = element_text(size = 12),
            legend.text = element_text(size = 12),
            axis.title  = element_text(size = 12),
            legend.title= element_text(size = 12)
          ) +
          guides(color = guide_legend(override.aes = list(linewidth = 3)))
        
      } else {
        # ptype == 'col' : stacked bars including Not applicable
        ggplot(text_df_bars, aes(x = timepoint, y = count, fill = value)) +
          geom_col(position = "stack") +
          facet_wrap(
            ~ `LSHTM subcomponent`,
            labeller = labeller(`LSHTM subcomponent` = custom_labels),
            nrow = facet_rows()
          ) +
          scale_x_discrete(
            labels = c("Baseline" = "First report", "End" = "Last report"),
            name = "Site reporting timepoint"
          ) +
          xlab("Site reporting timepoint") +
          ylab("Number of sites") +
          fill_scale +
          theme_bw() +
          theme(
            axis.text   = element_text(size = 12),
            strip.text  = element_text(size = 12),
            legend.text = element_text(size = 12),
            axis.title  = element_text(size = 12),
            legend.title= element_text(size = 12)
          )
      }
    })
    
    output$first_last_plot <- renderPlot(
      plot_reactive(),
      width  = function() plot_width()  * 96,
      height = function() plot_height() * 96
    )
    
    return(plot_reactive)
  })
}