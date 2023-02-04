#!/bin/R
library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(glue)
conflicted::conflict_prefer("filter", "dplyr")
source("utils.R")

time_of_day_rate <- function(df) {
  df %>%
    mutate(
      hour = as.integer(`Hour of Day`) / 3600,
      capacity_energy_charge = case_when(
        month(Day) %in% c(6:10) ~ if_else(
          is_weekday(Day) &
            between(hour, 11, 18),
          `Hourly Total` * 0.11841,  # 11.841¢ per kWh for all on-peak kWh June through October
          `Hourly Total` * 0.0160    # 1.160¢ per kWh for all off-peak kWh June through October
        ),
        TRUE ~ if_else(
          is_weekday(Day) &
            between(hour, 11, 18),
          `Hourly Total` * 0.09341, # 9.341¢ per kWh for all On-peak kWh November through May
          `Hourly Total` * 0.00948 # 0.948¢ per kWh for all Off-peak kWh November through May
        )
      )
    ) %>%
    summarize(
      capacity_energy_charge = sum(capacity_energy_charge, na.rm = TRUE),
      non_capacity_energy_charge = sum(`Hourly Total`) * 0.04261, # 4.261¢ per kWh for all kWh
      
    )
}

# Define server logic
shinyServer(function(input, output) {
  
  usage <-
    reactive({
      
      file <- input$file
      ext <- tools::file_ext(file$datapath)
      
      req(file)
      validate(need(ext == "csv", glue("Please upload a csv file. The uploaded file has extension '{ext}'.")))
      
      file_spec <- spec_csv(
        file = file$datapath
      )
      expected_columns <- c(
        "Account Number",
        "Meter Number",
        "Day",
        "Hour of Day",
        "Hourly Total",
        "Daily Total",
        "Unit of Measurement"
      )
      validate(
        need(
          expr = names(file_spec$cols) == expected_columns,
          message = glue("Unexpected columns. The csv should have columns: {toString(expected_columns)}.")
        )
      )
      
      usage <- 
        read_csv(
          file = input$file$datapath,
          col_types = cols(
            `Account Number` = col_character(),
            `Meter Number` = col_double(),
            Day = col_date(format = "%m/%d/%Y"),
            `Hour of Day` = col_time(format = ""),
            `Hourly Total` = col_double(),
            `Daily Total` = col_double(),
            `Unit of Measurement` = col_character()
          ),
          na = c("No Data")
        )
      
      return(usage)
    })

  output$placeholder <-
    renderText({
      req(is.null(input$file))
      
      return("Upload your energy usage data to get your personalized rate recommendation.")
    })
  
  output$recommendation <-
    renderText({
      # require
      req(usage)
      tribble(
        ~res_total, ~tod_total, ~dpp_total,
        sum(unlist(residential_service_rate_details(usage()))),
        sum(unlist(residential_time_of_day_rate_details(usage()))),
        sum(unlist(dynamic_peak_pricing_rate_details(usage())))
      ) %>%
        summarize(
          across(c(res_total, tod_total, dpp_total), sum, na.rm = TRUE),
          best_rate = factor(
            x = which.min(c(res_total, tod_total, dpp_total)),
            levels = 1:3,
            labels = c(
              "Residential Electricity Rate",
              "Time of Day",
              "Dynamic Peak Pricing"
            )
          ),
          tod_pct = abs((tod_total - res_total) / res_total),
          dpp_pct = abs((dpp_total - res_total) / res_total),
          tod_saving = tod_total < res_total,
          dpp_saving = dpp_total < res_total
        ) %>%
        mutate(text = str_squish(
          glue(
            "Your recommended rate is the {strong(best_rate)} rate.",
            "Your electricity usage costs would have been {strong(dollar(tod_total))}",
            "({strong(percent(tod_pct))} {strong(if_else(tod_saving, \"lower\", \"higher\"))}) with the Time of Day rate and",
            "{strong(dollar(dpp_total))} ({strong(percent(dpp_pct))} {strong(if_else(dpp_saving, \"lower\", \"higher\"))}) with the Dynamic Peak Pricing rate.",
            .sep = " "
          )
        )) %>%
        pull(text)
    })
  
  output$bill_details <- 
    renderTable({
      req(usage)

      res <- 
        residential_service_rate_details(usage()) %>%
        data.frame() %>%
        pivot_longer(
          cols = 1:6,
          names_to = "item",
          values_to = "amount"
        ) %>%
        add_column(rate = "Residential Electricity Service", .before = 1)
            
      tod <- 
        residential_time_of_day_rate_details(usage()) %>%
        data.frame() %>%
        pivot_longer(
          cols = 1:8,
          names_to = "item",
          values_to = "amount"
        ) %>%
        add_column(rate = "Time of Day", .before = 1)
      
      dpp <- 
        dynamic_peak_pricing_rate_details(usage()) %>%
        data.frame() %>%
        pivot_longer(
          cols = 1:8,
          names_to = "item",
          values_to = "amount"
        ) %>%
        add_column(rate = "Dynamic Peak Pricing", .before = 1)
      
      comparison <- 
        bind_rows(res, tod, dpp) %>%
        mutate(
          rate = factor(
            x = rate,
            levels = c(
              "Residential Electricity Service",
              "Time of Day",
              "Dynamic Peak Pricing"
            ),
            labels = c(
              "Residential Electricity Service",
              "Time of Day",
              "Dynamic Peak Pricing"
            ),
            ordered = TRUE
          ),
          item = case_when(
            startsWith(item, "power_supply_capacity_charge") ~ "Power Supply Capacity Charge",
            item == "power_supply_non_capacity_charge" ~ "Power Supply Non-Capacity Charge",
            item == "power_supply_cost_recovery" ~ "Power Supply Cost Recovery",
            item == "service_charge" ~ "Service Charge",
            item == "distribution_charge" ~ "Distribution Charge",
            TRUE ~ "Other"
          )
        )
      
      comparison %>%
        group_by(rate, item) %>%
        summarize(
          amount = dollar(sum(amount, na.rm = TRUE))
        ) %>%
        pivot_wider(
          id_cols = item,
          names_from = rate,
          values_from = amount
        )
    })
  
  output$usage_bars <-
    renderPlot({
      req(usage)
      tribble(
        ~key, ~value,
        "Residential Electricity Service", sum(unlist(residential_service_rate_details(usage()))),
        "Time of Day", sum(unlist(residential_time_of_day_rate_details(usage()))),
        "Dynamic Peak Pricing", sum(unlist(dynamic_peak_pricing_rate_details(usage())))
        ) %>%
        mutate(key = factor(
          x = key,
          levels = c(
            "Residential Electricity Service",
            "Time of Day",
            "Dynamic Peak Pricing"
          ),
          labels = c(
            "Residential Electricity Service",
            "Time of Day",
            "Dynamic Peak Pricing"
          ),
          ordered = TRUE
        )) %>%
        ggplot(aes(x = key, y = value, fill = key)) +
        geom_bar(stat = "identity", width = 0.5) +
        geom_text(
          aes(label = scales::dollar(value)),
          position = position_dodge(width = 1),
          vjust = -0.5,
          size = 5
        ) +
        scale_x_discrete(name = NULL) +
        scale_y_continuous(name = NULL,
                           labels = dollar_format()) +
        scale_fill_manual(
          name = "Rate Plan",
          values = c("#999999", "#66FF99", "#6699FF"),
          guide = "none"
        ) +
        labs(title = "Total Usage Cost by Rate Plan") +
        theme_minimal(base_size = 14) +
        theme(
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank()
        )
    })
  
  output$monthly_usage_bars <-
    renderPlot({
      req(usage)
      
      res <- 
        usage() %>%
        split(floor_date(usage()$Day, "month")) %>%
        map(residential_service_rate_details) %>%
        map(unlist) %>%
        map(sum, na.rm=TRUE) %>%
        flatten() %>%
        enframe() %>%
        unnest(cols = c(value)) %>%
        mutate(
          rate = "Residential Electricity Rate"
        )
      
      tod <- 
        usage() %>%
        split(floor_date(usage()$Day, "month")) %>%
        map(residential_time_of_day_rate_details) %>%
        map(unlist) %>%
        map(sum, na.rm=TRUE) %>%
        flatten() %>%
        enframe() %>%
        unnest(cols = c(value)) %>%
        mutate(
          rate = "Time of Day"
        )
      
      dpp <- 
        usage() %>%
        split(floor_date(usage()$Day, "month")) %>%
        map(dynamic_peak_pricing_rate_details) %>%
        map(unlist) %>%
        map(sum, na.rm=TRUE) %>%
        flatten() %>%
        enframe() %>%
        unnest(cols = c(value)) %>%
        mutate(
          rate = "Dynamic Peak Pricing"
        )

      bind_rows(
        res,
        tod,
        dpp
      ) %>%
        mutate(
          name = ymd(name),
          rate = factor(
            x = rate,
            levels = c(
              "Residential Electricity Service",
              "Time of Day",
              "Dynamic Peak Pricing"
            ),
            labels = c(
              "Residential Electricity Service",
              "Time of Day",
              "Dynamic Peak Pricing"
            ),
            ordered = TRUE
          )
        ) %>%
        ggplot(
          aes(
            x = name,
            y = value,
            fill = rate
          )
        ) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_x_date(name = NULL) +
        scale_y_continuous(name = NULL,
                           labels = dollar_format()) +
        scale_fill_manual(
          name = "Rate Plan",
          values = c("#999999", "#66FF99", "#6699FF"),
          labels = c(
            "Residential Electricity Service",
            "Time of Day",
            "Dynamic Peak Pricing"
          )
        ) +
        labs(title = "Monthly Usage Cost by Rate Plan") +
        theme_minimal(base_size = 14) +
        theme(
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position = "bottom"
        )

    })
  
  output$hourly_text <-
    renderText({
      # require usage data
      req(usage)
      
      daily_avg <-
        usage() %>%
        filter(is_weekday(Day),!Day %in% holidays_tbl$date) %>%
        group_by(`Hour of Day`) %>%
        summarize(avg = mean(`Hourly Total`, na.rm = TRUE)) %>%
        ungroup()
      
      x <-
        daily_avg %>%
        arrange(desc(avg)) %>%
        slice(1)
      
      glue(
        "On average you use the most electricity (about {round(x$`avg`, digits = 1)} kWh)",
        "during the {hour(x$`Hour of Day`) %% 12} {if_else(am(x$`Hour of Day`), 'AM', 'PM')} hour.",
        "By shifting electricity usage from on-peak times to mid-peak or off-peak times you may be able to reduce your electricity costs.",
        .sep = " "
      )
    })
  
  output$tod_hourly_usage <-
    renderPlot({
      # require usage data
      req(usage)
      
      usage() %>%
        filter(is_weekday(Day),!Day %in% holidays_tbl$date) %>%
        ggplot() +
        geom_rect(
          mapping = aes(
            xmin = start,
            xmax = end,
            ymin = -Inf,
            ymax = Inf,
            fill = fct_relevel(rate_tier, c("Off-peak", "On-peak")),
            group = i
          ),
          alpha = 0.25,
          data = tod_tbl
        ) +
        geom_smooth(
          mapping = aes(x = `Hour of Day` + 1800, y = `Hourly Total`),
          se = FALSE,
          color = "#6699FF",
          method = "gam",
          formula = y ~ s(x, bs = "cs")
        ) +
        scale_x_time(
          name = NULL,
          labels = c("Midnight", "4 AM", "8 AM", "Noon", "4 PM", "8 PM"),
          breaks = seq(0, 23, 4) * 3600,
          minor_breaks = seq(0, 23, 1) * 3600,
          expand = c(0, 0)
        ) +
        scale_y_continuous(name = "Electricity Usage (kWh)",
                           limits = c(0, NA)) +
        scale_fill_manual(name = NULL,
                          values = c("#3366FF", "#FF66CC")) +
        labs(title = "Avg. Hourly Electricity Usage",
             caption = "Excluding weekends and holidays") +
        theme_minimal(base_size = 14) +
        theme(legend.position = "bottom")
    })
  
  output$dpp_hourly_usage <-
    renderPlot({
      # require usage data
      req(usage)
      
      usage() %>%
        filter(is_weekday(Day),!Day %in% holidays_tbl$date) %>%
        ggplot() +
        geom_rect(
          mapping = aes(
            xmin = start,
            xmax = end,
            ymin = -Inf,
            ymax = Inf,
            fill = fct_relevel(rate_tier, c("Off-peak", "Mid-peak", "On-peak")),
            group = i
          ),
          alpha = 0.25,
          data = dpp_tbl
        ) +
        geom_smooth(
          mapping = aes(x = `Hour of Day` + 1800, y = `Hourly Total`),
          se = FALSE,
          color = "#6699FF",
          method = "gam",
          formula = y ~ s(x, bs = "cs")
        ) +
        scale_x_time(
          name = NULL,
          labels = c("Midnight", "4 AM", "8 AM", "Noon", "4 PM", "8 PM"),
          breaks = seq(0, 23, 4) * 3600,
          minor_breaks = seq(0, 23, 1) * 3600,
          expand = c(0, 0)
        ) +
        scale_y_continuous(name = "Electricity Usage (kWh)",
                           limits = c(0, NA)) +
        scale_fill_manual(name = NULL,
                          values = c("#3366FF", "#FFCC66", "#FF66CC")) +
        labs(title = "Avg. Hourly Electricity Usage",
             caption = "Excluding weekends and holidays") +
        theme_minimal(base_size = 14) +
        theme(legend.position = "bottom")
      
    })
  
  output$month_tod_breakdown <-
    renderPlot({
      # require usage data
      req(usage)
      
      usage() %>%
        group_by(month = floor_date(Day, unit = "month"), `Hour of Day`) %>%
        summarize(
          kwh = sum(`Hourly Total`, na.rm = TRUE)
        ) %>%
        ggplot(aes(y = month, x = `Hour of Day`, fill = kwh)) +
        geom_tile() +
        geom_vline(xintercept = hms("11:00:00")) +
        scale_y_date(name = NULL) +
        # scale_y_continuous(name = NULL,
        #                    labels = dollar_format()) +
        scale_fill_viridis_c(
          name = NULL,
          guide = "none"
        ) +
        labs(title = "Usage by Month and Hour of Day") +
        theme_minimal(base_size = 14) +
        theme(
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank()
        )
    })
  
  outputOptions(output, "tod_hourly_usage", suspendWhenHidden = FALSE)

})
