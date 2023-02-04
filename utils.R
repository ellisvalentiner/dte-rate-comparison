
is_weekend <- function (x) 
{
  wday(x = as_date(x), label = FALSE, abbr = FALSE) %in% c(1, 7)
}

is_weekday <- function (x) 
{
  wday(x = as_date(x), label = FALSE, abbr = FALSE) %in% 2:6
}

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
    
    "New Year’s Day",
    "2022-01-01",
    "Good Friday",
    "2022-04-15",
    "Memorial Day",
    "2022-05-30",
    "Independence Day",
    "2022-07-04",
    "Labor Day",
    "2022-09-05",
    "Thanksgiving Day",
    "2022-11-24",
    "Christmas Day",
    "2022-12-25",
  ) %>%
  type_convert(
    col_types = cols(
      holiday = col_character(),
      date = col_date(format = "%Y-%m-%d")
    )
  )

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

residential_service_rate_details <- function(usage_df) {
  # Residential Energy Service
  daily_kWh <-
    usage_df %>%
    group_by(`Day`) %>%
    slice(1) %>%
    pull(`Daily Total`)
  total_kWh <- sum(daily_kWh, na.rm = TRUE)
  count_months <- usage_df %>% distinct(month=month(Day)) %>% count() %>% pull(n)
  details <-
    list(
      # Capacity Energy Charges: 4.500¢ per kWh for the first 17 kWh per day
      power_supply_capacity_charge = sum(pmin(daily_kWh, 17) * 0.04500, na.rm = TRUE),
      # Capacity Energy Charges: 6.484¢ per kWh for excess over 17 kWh per day
      power_supply_capacity_charge_excess = sum(pmax(daily_kWh-17, 0) * 0.06484, na.rm = TRUE),
      # Non-Capacity Energy Charge: 4.176¢ per kWh for all kWh
      power_supply_non_capacity_charge = total_kWh * 0.04176,
      # Power Supply Cost Recovery: 0.665¢ per kWh
      power_supply_cost_recovery = total_kWh * 0.006650,
      # Service Charge: $7.50 per month
      service_charge = count_months * 7.50,
      # Distribution Charge: 6.611¢ per kWh for all kWh
      distribution_charge = total_kWh * 0.06611
  )
  return(details)
}

residential_time_of_day_rate_details <- function(usage_df) {
  daily_kWh <-
    usage_df %>%
    group_by(`Day`) %>%
    slice(1) %>%
    pull(`Daily Total`)
  total_kWh <- sum(daily_kWh, na.rm = TRUE)
  count_months <- usage_df %>% distinct(month=month(Day)) %>% count() %>% pull(n)
  june_oct_on_peak_kWh <- 
    usage_df %>%
    filter(
      between(month(Day), 6, 10),  # June through October
      between(hour(`Hour of Day`), 11, 18),  # 11 AM to 7 PM
      between(wday(Day), 2, 5)  # Monday through Friday
    ) %>%
    pull(`Hourly Total`) %>%
    sum(na.rm = TRUE)
  june_oct_off_peak_kWh <- 
    usage_df %>%
    filter(
      between(month(Day), 6, 10),  # June through October
      !between(hour(`Hour of Day`), 11, 18) & !between(wday(Day), 2, 5)  # 11 AM to 7 PM Monday through Friday
    ) %>%
    pull(`Hourly Total`) %>%
    sum(na.rm = TRUE)
  nov_may_on_peak_kWh <- 
    usage_df %>%
    filter(
      !between(month(Day), 6, 10),  # June through October
      between(hour(`Hour of Day`), 11, 18),  # 11 AM to 7 PM
      between(wday(Day), 2, 5)  # Monday through Friday
    ) %>%
    pull(`Hourly Total`) %>%
    sum(na.rm = TRUE)
  nov_may_off_peak_kWh <- 
    usage_df %>%
    filter(
      between(month(Day), 6, 10),  # June through October
      !between(hour(`Hour of Day`), 11, 18) & !between(wday(Day), 2, 5)  # 11 AM to 7 PM Monday through Friday
    ) %>%
    pull(`Hourly Total`) %>%
    sum(na.rm = TRUE)
  details <- 
    list(
      power_supply_capacity_charge_june_oct_on_peak = june_oct_on_peak_kWh * 0.11841,
      power_supply_capacity_charge_june_oct_off_peak = june_oct_off_peak_kWh * 0.01160,
      power_supply_capacity_charge_nov_may_on_peak = nov_may_on_peak_kWh * 0.09341,
      power_supply_capacity_charge_nov_may_off_peak = nov_may_off_peak_kWh * 0.00948,
      power_supply_non_capacity_charge = total_kWh * 0.04261,
      # Power Supply Cost Recovery: 0.665¢ per kWh
      power_supply_cost_recovery = total_kWh * 0.006650,
      # Service Charge: $7.50 per month
      service_charge = count_months * 7.50,
      # Distribution Charge: 6.611¢ per kWh for all kWh
      distribution_charge = total_kWh * 0.06611
    )
  return(details)
}

dynamic_peak_pricing_rate_details <- function(usage_df) {
  daily_kWh <-
    usage_df %>%
    group_by(`Day`) %>%
    slice(1) %>%
    pull(`Daily Total`)

  total_kWh <-
    sum(
      daily_kWh,
      na.rm = TRUE
    )
  count_months <- usage_df %>% distinct(month=month(Day)) %>% count() %>% pull(n)
  on_peak_kWh <- 
    usage_df %>%
    filter(
      between(hour(`Hour of Day`), 15, 18),  # 3 PM to 7 PM
      between(wday(Day), 2, 5),  # Monday through Friday
      !Day %in% holidays_tbl$date  # excluding holidays
    ) %>%
    pull(`Hourly Total`) %>%
    sum(na.rm = TRUE)
  mid_peak_kWh <- 
    usage_df %>%
    filter(
      between(hour(`Hour of Day`), 7, 14) | between(hour(`Hour of Day`), 19, 22),  # 7 AM to 3 PM and 7 PM to 11 PM
      between(wday(Day), 2, 5),  # Monday through Friday
      !Day %in% holidays_tbl$date  # excluding holidays
    ) %>%
    pull(`Hourly Total`) %>%
    sum(na.rm = TRUE)
  off_peak_kWh <- 
    usage_df %>%
    filter(
      between(hour(`Hour of Day`), 0, 7) | between(hour(`Hour of Day`), 23, 24),  # 11 PM to 7 AM
      between(wday(Day), 2, 5) | Day %in% holidays_tbl$date,  # M-F, all weekends and holidays
    ) %>%
    pull(`Hourly Total`) %>%
    sum(na.rm = TRUE)
  critical_peak_kWh <- 0
  details <- 
    list(
      power_supply_capacity_charge_on_peak = on_peak_kWh * 0.13025,
      power_supply_capacity_charge_mid_peak = mid_peak_kWh * 0.05645,
      power_supply_capacity_charge_off_peak = off_peak_kWh * 0.01218,
      power_supply_capacity_charge_critical_peak = critical_peak_kWh * 0.91424,
      power_supply_non_capacity_charge = total_kWh * 0.03576,
      # Power Supply Cost Recovery: 0.665¢ per kWh
      power_supply_cost_recovery = total_kWh * 0.006650,
      # Service Charge: $7.50 per month
      service_charge = count_months * 7.50,
      # Distribution Charge: 6.611¢ per kWh for all kWh
      distribution_charge = total_kWh * 0.06611
    )
  return(details)
}

