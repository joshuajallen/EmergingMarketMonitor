
#
# Some functions that relate to the market stress dashboard/
# html nightly report.
#

#
# Load a list of indicators from a CSV file
#

market_stress_config <- function() {

  read_csv(
    "Category,Region,Class,Indicator,Ticker
    Funding,JP,FX,USDJPY basis 3m,JYBSC Curncy
    Funding,EZ,FX,EURUSD basis 3m,EUBSC Curncy
    Funding,UK,FX,GBPUSD basis 3m,BPBSC Curncy
    Funding,JP,FX,USDJPY OIS basis 3m,.JYBSOIS Index
    Funding,EZ,FX,EURUSD OIS basis 3m,.EUBSOIS Index
    Funding,UK,FX,GBPUSD OIS basis 3m,.BPBSOIS Index
    Funding,US,MM,SOFR-FF,.SOFRFFBP Index
    Funding,US,MM,FRA-OIS USD,USFOSC2 Curncy
    Funding,EZ,MM,FRA-OIS EUR,EUFOSC2 Curncy
    Funding,UK,MM,FRA-OIS GBP,BPFOSC2 Curncy
    Funding,US,CR,CP-OIS AA financials 3m,.CPOISFIN Index
    Funding,US,CR,CP-OIS AA non-fin. 3m,.CPOISNON Index
    Funding,EM,FX,Funding stress (basis),.EMSTRESS Index
    Financial Conditions,US,Multi,BBG Financial Conditions,BFCIUS Index
    Efficiency,EZ,FI,Yield curve dispersion,GVLQDE Index
    Efficiency,US,FI,Yield curve dispersion,GVLQUSD Index
    Efficiency,UK,FI,Yield curve dispersion,GVLQGBP Index
    Dealer Intermediation,EZ,FI,10Y OIS swap spreads,RXAISPO Comdty
    Dealer Intermediation,US,FI,10Y OIS swap spreads,TYAISP Comdty
    Dealer Intermediation,UK,FI,10Y OIS swap spreads,GAISPO Comdty
    Periphery,EZ,FI,IT-DE 10Y yield spread,.ITGR10YR Index
    Periphery,EZ,FI,Italy 10Y bid-ask spread,.ITBIDASK Index"
  )

}


#
# Request the necessary data from Bloomberg
#

market_stress_data_query <- function(tickers,
                                     target_date,
                                     lookback_years) {
  bloomberg_query(
    tickers,
    "PX_LAST",
    target_date - (lookback_years * 365),
    target_date
  )

}


#
# Calculate all fields for the table and add sparkline plots
#

market_stress_generate_table <- function(config,
                                         bbg_series,
                                         change_period_days) {

  #
  # Calculate some summary statistics
  #

  summary_stats <- bbg_series %>%
    arrange(Security, Date) %>%
    group_by(Security) %>%
    select(-Field) %>%
    summarise(
      Current = last(Value),
      Change = last(Value) -
        head(tail(Value, change_period_days + 1),1),
      Min = min(Value, na.rm = TRUE),
      Max = max(Value, na.rm = TRUE),
      Average = mean(Value, na.rm = TRUE),
      Stdev = sd(Value, na.rm = TRUE)
    ) %>%
    mutate(
      `Z-score` = (Current - Average) / Stdev
    )


  #
  # Generate some sparkline content for datatable output
  #

  sparkline_data <- bbg_series %>%
    select(-Field) %>%
    group_by(Security) %>%
    summarize(
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

  config %>%
    left_join(
      summary_stats,
      by = c("Ticker" = "Security")
    ) %>%
    left_join(
      sparkline_data,
      by = c("Ticker" = "Security")
    )

}


#
# Create the datatable output
#

market_stress_create_datatable <- function(data_input,
                                           target_date,
                                           row_group_index,
                                           underline_tickers = FALSE) {

  datatable(
    data_input,
    extensions = "RowGroup",
    rownames = FALSE,
    escape = FALSE,
    selection = "none",
    options = list(
      ordering = FALSE,
      dom = "",
      paging = FALSE,
      rowGroup = list(
        dataSrc = row_group_index
      ),
      fnDrawCallback = htmlwidgets::JS(
        "function() { HTMLWidgets.staticRender(); }"
      ),
      columnDefs = list(list("targets" = row_group_index,
                             "visible" = FALSE))
    )
  ) %>%
    spk_add_deps() %>%
    formatCurrency(
      c(target_date, "Change", "Min", "Max",
        "Average", "Stdev", "Z-score"),
      digits = 1,
      currency = ""
    ) %>%
    formatStyle(
      "Ticker",
      textDecoration = if_else(underline_tickers, "underline", "none")
    ) %>%
    formatStyle(
      c("Z-score"),
      fontWeight = "bold"
    ) %>%
    formatStyle(
      "Z-score",
      color = styleInterval(c(-3, -2, 2, 3),
                            c("white", "white", "black", "white", "white")),
      backgroundColor = styleInterval(
        c(-3, -2, 2, 3),
        c("#aa0b3c", "#b65e19", NA, "#b65e19", "#aa0b3c")
      )
    )

}


