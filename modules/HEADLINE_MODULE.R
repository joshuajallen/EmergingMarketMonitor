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
  "../config/securities_headline.csv",
  col_types = cols(
    Type = col_character(),
    Category = col_character(), 
    Ticker = col_character(),
    Field = col_character(),
    Desc = col_character(),
    Country = col_character(),
    Region  = col_character()
  )
)


df <- bloomberg_query(
  security_config[security_config$Type != "Flows",]$Ticker,
  unique(security_config$Field),
  from_date = Sys.Date() - 1*365,
  to_date = Sys.Date(), 
  options = opt
)

df_flows <- bloomberg_query(
  security_config[security_config$Type == "Flows",]$Ticker,
  "RQ005",
  from_date = Sys.Date() - 1*365,
  to_date = Sys.Date(), 
  options = opt
)


ytd_daycount <- sum(!weekdays(seq(as.Date(paste0(year(Sys.Date()) -1, "-12-31")), lubridate::today(), "days")) %in% c("Saturday", "Sunday"))
month_count <- sum(!weekdays(seq(today() - 30, lubridate::today(), "days")) %in% c("Saturday", "Sunday"))


summary_stats <- df %>%
  dplyr::arrange(Security, Date) %>%
  dplyr::group_by(Security) %>%
  dplyr::select(-Field) %>%
  dplyr::summarise(
    Current = last(Value),
    Value7D = last(Value) - head(tail(Value, 6 + 1),1),
    Value30D = last(Value) - head(tail(Value, month_count + 1),1),
    ValueYTD = last(Value) - head(tail(Value, ytd_daycount + 1),1),
    Average = mean(Value, na.rm = TRUE),
    Stdev = sd(Value, na.rm = TRUE)
  ) %>%
  dplyr::mutate(
    `Z-score` = (Current - Average) / Stdev
  ) 


summary_stats_flows <- df_flows %>%
  tidyr::drop_na() %>% 
  dplyr::filter(Date >= as.Date(paste0(year(Sys.Date()) - 1, "-12", "-30"))) %>% 
  dplyr::mutate(Value = cumsum(Value)) %>% 
  dplyr::arrange(Security, Date) %>%
  dplyr::group_by(Security) %>%
  dplyr::select(-Field) %>%
  dplyr::summarise(
    Current = last(Value),
    Value7D = last(Value) - head(tail(Value, 6 + 1),1),
    Value30D = last(Value) - head(tail(Value, month_count + 1),1),
    ValueYTD = last(Value) - head(tail(Value, ytd_daycount + 1),1),
    Average = mean(Value, na.rm = TRUE),
    Stdev = sd(Value, na.rm = TRUE)
  ) %>%
  dplyr::mutate(
    `Z-score` = (Current - Average) / Stdev
  ) 

#
# Generate some sparkline content for datatable output
#

sparkline_data <- df %>%
  dplyr::select(-Field) %>%
  dplyr::group_by(Security) %>%
  dplyr::summarize(
    Timeseries = spk_chr(
      Value,
      type ="line",
      chartRangeMin = min(Value),
      chartRangeMax = max(Value),
      lineColor = "#4a7e8f",
      spotColor = "#aa0b3c",
      minSpotColor = "#999999",
      maxSpotColor = "#999999",
      fillColor = "#c3d9e0"
    ),
    `Box plot` = spk_chr(
      Value,
      type ="box",
      chartRangeMin = min(Value),
      chartRangeMax = max(Value),
      target = last(Value),
      targetColor = "#aa0b3c",
      medianColor = "#7f7f7f",
      boxLineColor = "#ffffff",
      whiskerColor = "#7f7f7f",
      lineColor = "#4a7e8f",
      boxFillColor = "#c3d9e0"
    )
  )


sparkline_data_flows <- df_flows %>%
  tidyr::drop_na() %>% 
  dplyr::filter(Date >= as.Date(paste0(year(Sys.Date()) - 1, "-12", "-30"))) %>% 
  dplyr::mutate(Value = cumsum(Value)) %>% 
  dplyr::select(-Field) %>%
  dplyr::group_by(Security) %>%
  dplyr::summarize(
    Timeseries = spk_chr(
      Value,
      type ="line",
      chartRangeMin = min(Value),
      chartRangeMax = max(Value),
      lineColor = "#4a7e8f",
      spotColor = "#aa0b3c",
      minSpotColor = "#999999",
      maxSpotColor = "#999999",
      fillColor = "#c3d9e0"
    ),
    `Box plot` = spk_chr(
      Value,
      type ="box",
      chartRangeMin = min(Value),
      chartRangeMax = max(Value),
      target = last(Value),
      targetColor = "#aa0b3c",
      medianColor = "#7f7f7f",
      boxLineColor = "#ffffff",
      whiskerColor = "#7f7f7f",
      lineColor = "#4a7e8f",
      boxFillColor = "#c3d9e0"
    )
  )

#
# Join in the summary stats and sparkline data
#

headline <- security_config[security_config$Type != "Flows",] %>%
  dplyr::left_join(
    summary_stats,
    by = c("Ticker" = "Security")
  ) %>%
  dplyr::left_join(
    sparkline_data,
    by = c("Ticker" = "Security")
  )  %>%
  dplyr::arrange(Type, Category, Region) %>%
  dplyr::select(
    Type, Desc, Category, Region, Country,  Ticker, Timeseries, Current,
    Value7D, Value30D, ValueYTD, 
    Average, Stdev, `Z-score`, `Box plot`
  )

headline_flows <- security_config[security_config$Type == "Flows",] %>%
  dplyr::left_join(
    summary_stats_flows,
    by = c("Ticker" = "Security")
  ) %>%
  dplyr::left_join(
    sparkline_data_flows,
    by = c("Ticker" = "Security")
  )  %>%
  dplyr::arrange(Type, Category, Region) %>%
  dplyr::select(
    Type, Desc, Category, Region, Country,  Ticker, Timeseries, Current,
    Value7D, Value30D, ValueYTD, 
    Average, Stdev, `Z-score`, `Box plot`
  )

market_stress <- dplyr::bind_rows(headline, headline_flows)


# market_stress_create_datatable(market_stress,
#                                row_group_index  = 0,
#                                underline_tickers = TRUE)

