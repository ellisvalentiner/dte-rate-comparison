library(shiny)

library(tidyverse)
library(lubridate)
library(lubridateExtras)
library(scales)
library(glue)

library(sentryR)

conflicted::conflict_prefer("filter", "dplyr")

if (Sys.getenv("ENVIRONMENT", "local") == "production" &&
    Sys.getenv("SENTRY_DSN") != "") {
  configure_sentry(
    dsn = Sys.getenv("SENTRY_DSN"),
    app_name = "dte-rate-comparison",
    app_version = "0.0.0-pre"
  )
  
  error_handler <- function() {
    capture(
      exception = list(
        type = geterrmessage(),
        value = geterrmessage(),
        stacktrace = list(frames = sentryR:::calls_to_stacktrace(calls = sys.calls()))
      ),
      level = "error"
    )
  }
  options(shiny.error = error_handler)
}

res_rate <- function(kWh) {
  # Residential Energy Service
  # First 17 kWh per day
  #   Capacity Energy      4.500¢
  #   Non-Capacity Energy  4.176¢
  # Additional kWh
  #   Capacity Energy      6.484¢
  #   Non-Capacity Energy  4.176¢
  return(kWh * 0.08676 + pmax(kWh - 17, 0) * 0.1066)
}

# Define server logic
shinyServer(function(input, output) {
  dpp_rates <-
    tribble(
      ~ rate,
      ~ dpp_cost,
      # Dynamic Peak Pricing Rates
      "Off-Peak",
      0.048,
      "Mid-Peak",
      0.092,
      "On-Peak",
      0.166,
      "Critical Peak",
      0.95
    )
  
  tod_rates <-
    tribble(
      ~ rate,
      ~ tod_cost,
      # Time of Day Rates
      "Summer On-Peak",
      0.11841 + 0.04261,
      "Summer Off-Peak",
      0.0160 + 0.04261,
      "Winter On-Peak",
      0.09341 + 0.04261,
      "Winter Off-Peak",
      0.00948 + 0.04261,
    )
  
  holidays_tbl <-
    tribble(
      ~ holiday,
      ~ date,
      "New Year’s Day",
      "2020-01-01",
      "Good Friday",
      "2020-04-10",
      "Memorial Day",
      "2020-05-25",
      "Independence Day",
      "2020-07-04",
      "Labor Day",
      "2020-09-07",
      "Thanksgiving Day",
      "2020-11-26",
      "Christmas Day",
      "2020-12-25",
      
      "New Year’s Day",
      "2021-01-01",
      "Good Friday",
      "2021-04-02",
      "Memorial Day",
      "2021-05-31",
      "Independence Day",
      "2021-07-04",
      "Labor Day",
      "2021-09-06",
      "Thanksgiving Day",
      "2021-11-25",
      "Christmas Day",
      "2021-12-25",
    ) %>%
    type_convert(col_types = cols(holiday = col_character(),
                                  date = col_date(format = "%Y-%m-%d")))
  
  tod_tbl <-
    tribble(
      ~ i,
      ~ start,
      ~ end,
      ~ rate_tier,
      1,
      "00:00:00",
      "07:00:00",
      "Off-peak",
      2,
      "07:00:00",
      "19:00:00",
      "On-peak",
      3,
      "19:00:00",
      "24:00:00",
      "Off-peak"
    ) %>%
    type_convert(
      col_types = cols(
        i = col_integer(),
        start = col_time(),
        end = col_time(),
        rate_tier = col_character()
      )
    )
  
  dpp_tbl <-
    tribble(
      ~ i,
      ~ start,
      ~ end,
      ~ rate_tier,
      1,
      "00:00:00",
      "07:00:00",
      "Off-peak",
      2,
      "07:00:00",
      "15:00:00",
      "Mid-peak",
      3,
      "15:00:00",
      "19:00:00",
      "On-peak",
      4,
      "19:00:00",
      "23:00:00",
      "Mid-peak",
      5,
      "23:00:00",
      "24:00:00",
      "Off-peak"
    ) %>%
    type_convert(
      col_types = cols(
        i = col_integer(),
        start = col_time(),
        end = col_time(),
        rate_tier = col_character()
      )
    )
  
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
            Day = col_date(format = "%m/%d/%Y"),
            `Hour of Day` = col_time(format = ""),
            `Hourly Total` = col_double(),
            `Daily Total` = col_double(),
            `Unit of Measurement` = col_character()
          )
        )
      
      return(usage)
    })
  
  comparison_data <-
    reactive({
      req(usage)
      usage() %>%
        mutate(
          hour = as.integer(`Hour of Day`) / 3600,
          dpp_rate = case_when(
            Day %in% holidays_tbl$date ~ "Off-Peak",
            is_weekend(Day) ~ "Off-Peak",
            is_weekday(Day) & hour < 7 ~ "Off-Peak",
            is_weekday(Day) & hour >= 23 ~ "Off-Peak",
            is_weekday(Day) &
              hour >= 7 & hour < 15 ~ "Mid-Peak",
            is_weekday(Day) &
              hour >= 19 & hour < 23 ~ "Mid-Peak",
            is_weekday(Day) &
              hour >= 15 & hour < 19 ~ "On-Peak"
          ),
          tod_rate = case_when(
            month(Day) %in% c(6:10) ~ if_else(
              is_weekday(Day) &
                between(hour, 11, 18),
              "Summer On-Peak",
              "Summer Off-Peak"
            ),
            TRUE ~ if_else(
              is_weekday(Day) &
                between(hour, 11, 18),
              "Winter On-Peak",
              "Winter Off-Peak"
            )
          )
        ) %>%
        left_join(y = dpp_rates,
                  by = c("dpp_rate" = "rate")) %>%
        left_join(y = tod_rates,
                  by = c("tod_rate" = "rate")) %>%
        mutate(dpp_price = `Hourly Total` * dpp_cost,
               tod_price = `Hourly Total` * tod_cost) %>%
        group_by(Day) %>%
        summarize(
          daily_kwh = sum(`Hourly Total`),
          res_total = res_rate(daily_kwh),
          dpp_total = sum(dpp_price),
          tod_total = sum(tod_price),
          .groups = "drop"
        ) %>%
        mutate(dpp_savings = res_total - dpp_total,
               tod_savings = res_total - tod_total)
    })
  
  output$placeholder <-
    renderText({
      req(is.null(input$file))
      
      return("Upload your energy usage data to get your personalized rate recommendation.")
    })
  
  output$recommendation <-
    renderText({
      # require
      req(comparison_data)
      
      comparison_data() %>%
        # comparison_data %>%
        summarize(
          across(c(res_total, tod_total, dpp_total), sum),
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
  
  output$usage_bars <-
    renderPlot({
      req(comparison_data)
      
      comparison_data() %>%
        summarize(across(c(res_total, tod_total, dpp_total), sum)) %>%
        gather() %>%
        mutate(key = factor(
          x = key,
          levels = c("res_total", "tod_total", "dpp_total"),
          labels = c(
            "Residential Electricity Service",
            "Time of Day",
            "Dynamic Peak Pricing"
          ),
          ordered = TRUE
        )) %>%
        ggplot(aes(x = key, y = value, fill = key)) +
        geom_bar(stat = "identity", width = 0.5) +
        scale_x_discrete(name = NULL) +
        scale_y_continuous(name = NULL,
                           labels = dollar_format()) +
        scale_fill_manual(
          name = "Rate Plan",
          values = c("#999999", "#66FF99", "#6699FF"),
          guide = FALSE
        ) +
        labs(title = "Total Usage Cost by Rate Plan") +
        theme_minimal(base_size = 14) +
        theme(
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank()
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
        summarize(avg = mean(`Hourly Total`)) %>%
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
  
  outputOptions(output, "tod_hourly_usage", suspendWhenHidden = FALSE)

})
