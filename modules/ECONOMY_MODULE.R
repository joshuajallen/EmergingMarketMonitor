# 
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
# app_config$boe_palette <- rep(
#   c(
#     "#4a7e8f",
#     #  1 teal
#     "#cf395c",
#     #  2 bright pink
#     "#a9c9d3",
#     #  3 light blue
#     "#b25395",
#     #  4 pink
#     "#3b855f",
#     #  5 green
#     "#2f4f5b",
#     #  6 very dark teal
#     "#b65e19",
#     #  7 orange
#     "#0f7cbf",
#     #  8 blue
#     "white",
#     #  9 white
#     "#555555"  # 10 dark grey
#   ),
#   3
# )
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
# 
# FIRVr_str <-
#   "library(FIRVr, lib.loc = '\\\\\\\\markets-nwsrv/DATA/Offdata/RM/_R code repository/RMPackages')"
# eval(parse(text = FIRVr_str))
# 
# source("functions/plotly_config.R")


opt <-
  c(
    "nonTradingDayFillOption" = "NON_TRADING_WEEKDAYS",
    "nonTradingDayFillMethod" = "PREVIOUS_VALUE"
  )

security_config <- readr::read_csv(
  "../config/securities_economy.csv",
  col_types = cols(
    Type = col_character(),
    Ticker = col_character(),
    Field = col_character(),
    Desc = col_character(),
    Country = col_character(),
    Region = col_character()
  )
)

base_year <- as.numeric(stringr::str_sub(lubridate::year(Sys.Date()), start = 3, end = 4))
year_start <- as.numeric(stringr::str_sub(lubridate::year(Sys.Date()), start = 1, end = 2))
forecast_years <- c(base_year, base_year + 1, base_year + 2, base_year + 3)
securities_forecast <- security_config$Ticker[which(security_config$Type == "Forecast")]
secus <- c()
for(i in forecast_years){
  secus <- c(secus, paste(securities_forecast, " ", i, " Index", sep = ""))
}

blpConnect()

df <- bloomberg_query(
  security_config$Ticker[which(security_config$Type == "Data")],
  unique(security_config$Field),
  from_date = Sys.Date() - 12*365,
  to_date = Sys.Date()
)

data_economy <- security_config %>%
  dplyr::left_join(
    df,
    by = c("Ticker" = "Security")
  ) %>%
  dplyr::select(
    Date, Type, Ticker, Desc, Country, Region, Value,
  )


df <- bloomberg_query(
  secus,
  unique(security_config$Field),
  from_date = Sys.Date(),
  to_date = Sys.Date()
) %>%
  dplyr::mutate(
    Year = paste0(year_start, stringr::str_extract(string = Security, pattern = "[0-9]{1,2}")),
    Security = gsub(' [0-9]*[[:space:]]*Index', '' , Security), 
    Date = format(Date, "%b-%y")
  ) %>% 
  dplyr::select(-Field)

data_forecast <- df %>%
  dplyr::left_join(
    security_config,
    by = c("Security" = "Ticker")
  ) %>%
  dplyr::select(
    Country, Region, Date, Year, Desc,Value
  )
