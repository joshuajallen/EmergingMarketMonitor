
opt <-
  c(
    "periodicitySelection" = "DAILY",
    "nonTradingDayFillOption" = "NON_TRADING_WEEKDAYS",
    "nonTradingDayFillMethod" = "PREVIOUS_VALUE"
  )

security_config <- readr::read_csv(
  "../config/securities_equities.csv",
  col_types = cols(
    Country = col_character(),
    Ticker = col_character(),
    Field = col_character(),
    Desc = col_character(),
    Main = col_character(),
    Region = col_character()
  )
)

#

unique_fields <- security_config$Field %>% unique()

df <- bloomberg_query(
  security_config$Ticker,
  unique_fields,
  from_date = Sys.Date() - 3*365,
  to_date = Sys.Date(), 
  options = opt
)

data_equity <- security_config %>%
  dplyr::left_join(df,
                   by = c("Ticker" = "Security")) %>%
  dplyr::select(Date, Type, Ticker, Desc, Value, Country, Region, Main)

ytd_daycount <- sum(!weekdays(seq(as.Date(paste0(year(Sys.Date()) -1, "-12-31")), lubridate::today(), "days")) %in% c("Saturday", "Sunday"))

get_equity_timeseries <- data_equity %>%
  dplyr::group_by(Desc) %>%
  dplyr::mutate(
    Value1D = (Value/lag(Value)-1)*100,
    Value7D = (Value/lag(Value, 7)-1)*100,
    Value30D = (Value - lag(Value, month_count)) * 100,
    ValueBps7D = (Value-lag(Value, 7))*100,
    ValueYTD = (Value/lag(Value,ytd_daycount )-1)*100
  )  %>%
  dplyr::ungroup()


# equity table  -----------------------------------------------------------

EquityTable <- get_equity_timeseries %>%
  dplyr::group_by(Ticker) %>%
  dplyr::filter(Date == max(Date), Type == "Equity") %>%
  dplyr::ungroup() %>%
  dplyr::select(c(Country, Region, Ticker, Desc, Value, Value7D, Value30D, ValueYTD)) %>%
  dplyr::rename('Exchange Rate' = Desc,
                'Latest' = Value,
                '1W Change (%)' = Value7D,
                '1M Change (%)' = Value30D,
                'YTD Change (%)' = ValueYTD
  )


# equity vols  ------------------------------------------------------------

blpConnect()

data_equity_vol <- bloomberg_query(
  securities = security_config$Ticker[which(security_config$Main == "Yes")],
  fields = "VOLATILITY_30D",
  from_date = Sys.Date() - 3 * 365,
  to_date = Sys.Date()
)

data_equity_vol <- security_config %>%
  dplyr::left_join(data_equity_vol,
                   by = c("Ticker" = "Security")) %>%
  dplyr::select(Date, Ticker, Desc, Value, Country, Region, Main) %>%
  dplyr::filter(Main == "Yes") %>%
  dplyr::mutate(Desc = paste0(Country, " - ", Desc)) %>% 
  dplyr::select(Date, Country, Region, Ticker, Desc, Value) 


