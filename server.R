library(shiny)

library(tidyverse)
library(lubridate)
library(lubridateExtras)
library(scales)
library(glue)

library(sentryR)

if (Sys.getenv("SENTRY_DSN") != "") {
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
              stacktrace = list(
                  frames = sentryR:::calls_to_stacktrace(
                      calls = sys.calls()
                  )
              )
          ),
          level = "error"
      )
  }
  options(
    shiny.error = error_handler
  )
}

# Define server logic
shinyServer(function(input, output) {
    
    # Dynamic Peak Pricing Rates
    rates <- 
        tribble(
            ~rate, ~cost,
            "Off-Peak", 0.048,
            "Mid-Peak", 0.092,
            "On-Peak", 0.166,
            "Critical Peak", 0.95
        )
    
    holidays_tbl <- 
        tribble(
            ~holiday, ~date,
            "New Year’s Day", "2020-01-01",
            "Good Friday", "2020-04-10",
            "Memorial Day", "2020-05-25",
            "Independence Day", "2020-07-04",
            "Labor Day", "2020-09-07",
            "Thanksgiving Day", "2020-11-26",
            "Christmas Day", "2020-12-25",
            
            "New Year’s Day", "2021-01-01",
            "Good Friday", "2021-04-02",
            "Memorial Day", "2021-05-31",
            "Independence Day", "2021-07-04",
            "Labor Day", "2021-09-06",
            "Thanksgiving Day", "2021-11-25",
            "Christmas Day", "2021-12-25",
        ) %>%
        type_convert(
            col_types = cols(
                holiday = col_character(),
                date = col_date(format = "%Y-%m-%d")
            )
        )
    
    dpp_tbl <- 
        tribble(
            ~i, ~start, ~end, ~rate_tier,
            1, "00:00:00", "07:00:00", "Off-peak",
            2, "07:00:00", "15:00:00", "Mid-peak",
            3, "15:00:00", "19:00:00", "On-peak",
            4, "19:00:00", "23:00:00", "Mid-peak",
            5, "23:00:00", "24:00:00", "Off-peak"
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
            req(input$file)
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
        })
        
    comparison_data <-
        reactive({
            req(usage)
            
            usage() %>%
                mutate(
                    hour = as.integer(`Hour of Day`) / 3600,
                    rate = case_when(
                        Day %in% holidays_tbl$date ~ "Off-Peak",
                        is_weekend(Day) ~ "Off-Peak",
                        is_weekday(Day) & hour < 7 ~ "Off-Peak",
                        is_weekday(Day) & hour >= 23 ~ "Off-Peak",
                        is_weekday(Day) & hour >= 7 & hour < 15 ~ "Mid-Peak",
                        is_weekday(Day) & hour >= 19 & hour < 23 ~ "Mid-Peak",
                        is_weekday(Day) & hour >= 15 & hour < 19 ~ "On-Peak"
                    )
                ) %>%
                left_join(
                    y = rates,
                    by = "rate"
                ) %>%
                mutate(
                    price = `Hourly Total` * cost
                ) %>%
                group_by(Day) %>%
                summarize(
                    smart_currents_total = sum(price),
                    standard_total = first(pmax(`Daily Total` - 17, 0) * 0.1066 + `Daily Total` * 0.08676)
                ) %>%
                mutate(
                    savings = standard_total - smart_currents_total
                )
        })
    
    best_rate <-
        reactive({
            # require the comparison data
            req(comparison_data)
            
            comparison_data() %>%
                summarize(
                    across(c(smart_currents_total, standard_total), sum)
                ) %>%
                summarize(
                    condition = case_when(
                        smart_currents_total < standard_total ~ "dpp",
                        smart_currents_total > standard_total ~ "standard",
                        TRUE ~ "same"
                    )
                ) %>%
                pull(condition)
        })
    
    output$placeholder <- 
        renderText({
            req(is.null(input$file))
            
            "Upload your energy usage data to and get your personalized recommendation."
        })
    
    output$recommendation <-
        renderText({
            # require
            req(best_rate)
            
            directional <-
                case_when(
                    best_rate() == "dpp" ~ "lower",
                    best_rate() == "standard" ~ "higher",
                    TRUE ~ "about the same"
                )
            
            pct <- 
                comparison_data() %>%
                summarize(
                    across(c(smart_currents_total, standard_total), sum)
                ) %>%
                summarize(
                    pct = abs((smart_currents_total - standard_total) / standard_total)
                ) %>%
                pull(pct)
            
            paste(
                glue("Your electricity usage costs would have been {strong(percent(pct))} {strong(directional)} with Dynamic Peak Pricing."),
                glue("The cost of electricity usage would have been {strong(dollar(sum(comparison_data()$smart_currents_total)))} using the Dynamic Peak Pricing rate, compared to {strong(dollar(sum(comparison_data()$standard_total)))} at the standard rate.")
            )
            
        })
    
    output$usage_bars <-
        renderPlot({
            req(comparison_data)
            
            comparison_data() %>%
                summarize(
                    across(c(smart_currents_total, standard_total), sum)
                ) %>%
                gather() %>%
                mutate(
                    key = if_else(key == "standard_total", "Standard", "Dynamic Peak Pricing")
                ) %>%
                ggplot(aes(x = key, y = value, fill = key)) +
                geom_bar(
                    stat = "identity", width = 0.2
                ) +
                scale_x_discrete(
                    name = NULL
                ) +
                scale_y_continuous(
                    name = NULL,
                    labels = dollar_format()
                ) +
                scale_fill_manual(
                    name = "Rate Plan",
                    labels = c("Dynamic Peak Pricing", "Standard"),
                    values = c("#6699FF", "#999999"),
                    guide = FALSE
                ) +
                labs(
                    title = "Total Cost by Rate Plan",
                    subtitle = "Dynamic Peak Pricing vs Standard"
                ) +
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
                filter(
                    is_weekday(Day),
                    !Day %in% holidays_tbl$date
                ) %>%
                group_by(`Hour of Day`) %>%
                summarize(
                    avg = mean(`Hourly Total`)
                ) %>%
                ungroup()
            
            x <- 
                daily_avg %>%
                arrange(desc(avg)) %>%
                slice(1)
            
            glue("On average you use the most eletricity (about {round(x$`avg`, digits = 1)} kWh) during the {hour(x$`Hour of Day`) %% 12} {if_else(am(x$`Hour of Day`), 'AM', 'PM')} hour.")
        })

    output$hourly_usage <-
        renderPlot({
            # require usage data
            req(usage)
            
            usage() %>%
                filter(
                    is_weekday(Day),
                    !Day %in% holidays_tbl$date
                ) %>%
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
                scale_y_continuous(
                    name = "Electricity Usage (kWh)",
                    limits = c(0, NA)
                ) +
                scale_fill_manual(
                    name = NULL,
                    values = c("#3366FF", "#FFCC66", "#FF66CC")
                ) +
                labs(
                    title = "Avg. Hourly Electricity Usage",
                    caption = "Excluding weekends and holidays"
                ) +
                theme_minimal(base_size = 14) +
                theme(
                    legend.position = "bottom"
                )
            
        })

})
