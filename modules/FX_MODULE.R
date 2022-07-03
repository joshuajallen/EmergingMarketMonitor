opt <-
  c(
    "periodicitySelection" = "DAILY",
    "nonTradingDayFillOption" = "NON_TRADING_WEEKDAYS",
    "nonTradingDayFillMethod" = "PREVIOUS_VALUE"
  )

security_config <- readr::read_csv(
  "../config/securities_fx.csv",
  col_types = cols(
    Type = col_character(),
    Ticker = col_character(),
    Field = col_character(),
    Desc = col_character(),
    Ex = col_character(),
    Country = col_character(),
    Region = col_character()
  )
)

#

unique_fields <- security_config$Field %>% unique()

df <- bloomberg_query(
  security_config$Ticker,
  unique_fields,
  from_date = Sys.Date() - 3*365,
  to_date = Sys.Date(),options = opt
)

data_fx <- security_config %>%
  dplyr::left_join(df,
            by = c("Ticker" = "Security")) %>%
  dplyr::select(Date, Type, Ticker, Desc, Value, Country, Region, Ex)

ytd_daycount <- sum(!weekdays(seq(as.Date(paste0(year(Sys.Date()) -1, "-12-31")), lubridate::today(), "days")) %in% c("Saturday", "Sunday"))

get_fx_timeseries <- data_fx %>%
  dplyr::group_by(Desc) %>%
  dplyr::mutate(
    Value1D = (Value - lag(Value)) * 100,
    Value7D = (Value - lag(Value, 7)) * 100,
    Value30D = (Value - lag(Value, month_count)) * 100,
    ValueYTD = (Value - lag(Value, ytd_daycount)) * 100
    )  %>%
  dplyr::ungroup()



# ERI table  --------------------------------------------------------------

ERITable <- get_fx_timeseries %>%
  dplyr::group_by(Ticker) %>%
  dplyr::filter(Date == max(Date), Type %in% c("Exchange Rate", "ERI")) %>%
  dplyr::ungroup() %>%
  dplyr::select(c(Country, Region, Type, Desc, Value, Value7D, Value30D, ValueYTD)) %>%
  dplyr::rename('Exchange Rate' = Desc,
         'Latest' = Value,
         '1W Change (%)' = Value7D,
         '1M Change (%)' = Value30D,
         'YTD Change (%)' = ValueYTD
  )

