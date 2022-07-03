# 
# 
# checkpoint_date <- "2021-03-03"
# source(
#   "\\\\istdba/BuildArchive/ShinyApps/EnvironmentScript/EnvironmentScript_Latest/LOCAL/Artifactory/environment.R"
# )
# message(paste0("checkpoint_date: ", checkpoint_date))
# boeCheckpoint(checkpoint_date, scanForPackages = TRUE)
# 
# app_config <- new.env()
# 
# app_config$boe_palette <- rep(c(
#   "#4a7e8f", #  1 teal
#   "#cf395c", #  2 bright pink
#   "#a9c9d3", #  3 light blue
#   "#b25395", #  4 pink
#   "#3b855f", #  5 green
#   "#2f4f5b", #  6 very dark teal
#   "#b65e19", #  7 orange
#   "#0f7cbf", #  8 blue
#   "white",   #  9 white
#   "#555555"  # 10 dark grey
# ), 3)
# 
# app_config$boe_markers_scatter <-
#   c(rep("circle",  length(app_config$boe_palette)),
#     rep("square",  length(app_config$boe_palette)),
#     rep("diamond", length(app_config$boe_palette)))
# 
# app_config$cache_invalidation_seconds <- 5 * 60
# app_config$default_height <- "600px"
# app_config$font_style <- list(family = c("Cabin"))
# app_config$headroom <- "85px"
# app_config$headroom_0.75 <- "64px"
# 
# 
# 
# library(dplyr)
# library(DT)
# library(flock)
# library(lubridate)
# library(plotly)
# library(purrr)
# library(Rblpapi)
# library(rjson)
# library(readr)
# library(shiny)
# library(shinycssloaders)
# library(shinydashboard)
# library(shinyjs)
# library(sparkline)
# library(stringr)
# library(tidyr)
# library(zoo)
# library(plotly)
# #
# FIRVr_str <- "library(FIRVr, lib.loc = '\\\\\\\\markets-nwsrv/DATA/Offdata/RM/_R code repository/RMPackages/_R4.0')"
# eval(parse(text = FIRVr_str))
# 
# source("functions/plotly_config.R")

opt <-
  c(
    "periodicitySelection" = "DAILY",
    "nonTradingDayFillOption" = "NON_TRADING_WEEKDAYS",
    "nonTradingDayFillMethod" = "PREVIOUS_VALUE"
  )

security_config <- readr::read_csv(
  "../config/securities_rates.csv",
  col_types = cols(
    Type = col_character(),
    Ticker = col_character(),
    Field = col_character(),
    Desc = col_character(),
    Country = col_character(),
    Maturity = col_double()
  )
)


df <- FIRVr::bloomberg_query(
  security_config$Ticker,
  unique(security_config$Field),
  from_date = Sys.Date() - 365,
  to_date = Sys.Date(), options = opt
)

data_rates <- security_config %>%
  dplyr::left_join(
    df,
    by = c("Ticker" = "Security")
  ) %>%
  dplyr::select(
    Date, Type, Ticker, Maturity, Desc, Country, Region, Value
  )

us_rates <- FIRVr::bloomberg_query(
  security_config$Ticker[which(security_config$Country == "US")],
  unique(security_config$Field),
  from_date = Sys.Date() - 365,
  to_date = Sys.Date(), options = opt
) %>% 
  dplyr::left_join(
    security_config,
    by = c("Security" = "Ticker")
  ) %>%
  dplyr::rename("Rate" = "Value") %>% #"Tenor" = "Maturity"
  dplyr::select(Date, Security, Maturity, Rate)

summary_table <- data_rates %>%
  dplyr::filter(Type == "Summary") %>%
  dplyr::group_by(Ticker) %>%
  dplyr::filter(Date == max(Date)) 


ytd_daycount <- sum(!weekdays(seq(as.Date(paste0(year(Sys.Date()) -1, "-12-31")), lubridate::today(), "days")) %in% c("Saturday", "Sunday"))
month_count <- sum(!weekdays(seq(today() - 30, lubridate::today(), "days")) %in% c("Saturday", "Sunday"))

get_rate_timeseries <-
  data_rates %>%
  dplyr::group_by(Ticker) %>%
  dplyr::mutate(
    Value1D = (Value - lag(Value)) * 100,
    Value7D = (Value - lag(Value, 7)) * 100,
    Value30D = (Value - lag(Value, month_count)) * 100,
    ValueYTD = (Value - lag(Value, ytd_daycount)) * 100
  )  %>%
  dplyr::ungroup()


# rate table output  ------------------------------------------------------

rate_table <- get_rate_timeseries %>%
  dplyr::filter(Type == "Rate") %>%
  dplyr::group_by(Ticker) %>%
  dplyr::filter(Date == max(Date)) %>%
  dplyr::ungroup() %>%
  dplyr::select(c(Country, Region, Desc, Value, Value7D,Value30D, ValueYTD)) %>%
  dplyr::rename('Rate' = Desc,
                'Latest Yield (%)' = Value,
                '1W Change (Bps)' = Value7D,
                '1M Change (Bps)' = Value30D,
                'YTD Change (Bps)' = ValueYTD
  )

breakeven_table <- get_rate_timeseries %>%
  dplyr::filter(Type == "Breakeven") %>%
  dplyr::group_by(Ticker) %>%
  dplyr::filter(Date == max(Date)) %>%
  dplyr::ungroup() %>%
  dplyr::select(c(Country, Region, Desc, Value, Value7D, Value30D, ValueYTD)) %>%
  dplyr::rename('Rate' = Desc,
                'Latest Yield (%)' = Value,
                '1W Change (Bps)' = Value7D,
                '1M Change (Bps)' = Value30D,
                'YTD Change (Bps)' = ValueYTD
  )


bank_rate_table <- get_rate_timeseries %>%
  dplyr::filter(Type == "Bank Rate") %>%
  dplyr::group_by(Ticker) %>%
  dplyr::filter(Date == max(Date)) %>%
  dplyr::ungroup() %>%
  dplyr::select(c(Country, Region, Desc, Value, Value7D, Value30D, ValueYTD)) %>%
  dplyr::rename('Rate' = Desc,
                'Latest Yield (%)' = Value,
                '1W Change (Bps)' = Value7D,
                '1M Change (Bps)' = Value30D,
                'YTD Change (Bps)' = ValueYTD
  )

