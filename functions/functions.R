

plot_sovereign_curves <- function(data, Region){
  
  df <- data %>%
    dplyr::filter(Type == "Rate", Region %in% paste0(Region)) %>%
    dplyr::group_by(Ticker) %>%
    dplyr::filter(Date == max(Date)) %>%
    dplyr::ungroup() %>%
    dplyr::select(c(Country, Desc, Value, Maturity)) %>%
    tidyr::pivot_wider(id_cols = Maturity, names_from = Country, values_from = Value) %>%
    dplyr::arrange(Maturity)
  
  dates <- data$Date - as.difftime(180, unit="days")
  dates <- dates[!(weekdays(as.Date(dates)) %in% c('Saturday','Sunday'))]

  df_lag <- data %>%
    dplyr::filter(Type == "Rate", Region %in% paste0(Region)) %>%
    dplyr::group_by(Ticker) %>%
    dplyr::filter(Date %in% max(dates[dates %in% unique(data$Date)], na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(c(Country, Desc, Value, Maturity)) %>%
    tidyr::pivot_wider(id_cols = Maturity, names_from = Country, values_from = Value) %>%
    dplyr::arrange(Maturity)
  
  
  p <- plotly::plot_ly(data = df)
  
  i <- 0
  
  for (j in colnames(df)[2:ncol(df)]) {
    i <- i + 1
    p <- p %>%
      add_trace(x = ~Maturity, y = df[[j]], name = j,
                type = "scatter", mode = "lines+markers",
                line = list(color = app_config$boe_palette[i]),
                marker = list(color = app_config$boe_palette[i]))
  }
  
  k <- 0
  
  for (j in colnames(df_lag)[2:ncol(df_lag)]) {
    k <- k + 1
    p <- p %>%
      add_trace(x = ~Maturity, y = df_lag[[j]], name = paste0(j, " - 30 days ago"),
                type = "scatter", mode = "lines+markers",
                opacity = 0.2,
                line = list(color = app_config$boe_palette[k], dash="dot"),
                marker = list(color = app_config$boe_palette[k]),
                visible = 'legendonly')
  }
  
  p <- p %>%
    plotly_std_config() %>%
    plotly_std_style() %>%
    layout(
      xaxis = list(title = 'Maturity', ticks = "outside", ticklen = 5, tickwidth = 2, tickcolor = toRGB("black"),
                   showgrid = T, autorange = T, showticklabels = TRUE, zeroline = F,  showline = T),
      yaxis = list(title = 'Yield (%)', ticks = "outside", ticklen = 5, tickwidth = 2, tickcolor = toRGB("black"),
                   showgrid = T, autorange = T, showticklabels = TRUE, zeroline = F,  showline = T),
      margin = list(b = 50)#,
      #legend = list(orientation = 'h')
    )
  
  return(p)
  
}

make_line_plot <- function(df, xlab, ylab, fill, name, visible = NULL){
  
  
  p <- plotly::plot_ly(data = df)
  tickers <- df[[fill]] %>% unique()
  
  for ( i in seq_len(length(tickers)) ) {
    
    current <- df %>%
      dplyr::filter(!! rlang::sym(fill) == tickers[i])
    
    p <- p %>%
      add_trace(
        x = current[[xlab]],
        y = current[[ylab]],
        name = current[[name]][1],
        type = "scatter",
        mode = "lines",
        line = list(color = app_config$boe_palette[i]), 
        visible = visible
      )
  }
  
  return(p)
  
}




create_data_table <- function(data, row_target = 0, row_group = "Country"){
  
  
  DT::datatable(
    data,
    extensions = 'RowGroup',
    options = list(
      pageLength = 20,
      dom = 't',
      rowGroup = row_group,
      columnDefs = list(list(
        visible = FALSE, targets = c(row_target)
      ))
    ),
    rownames = FALSE
  )
  
  
}

format_style_tables <- function(table){
  
  
  table %>% 
  formatStyle('1W Change (Bps)',
              color = styleInterval(0, c(
                app_config$boe_palette[2],
                app_config$boe_palette[5]
              )),) %>%
    formatStyle('1M Change (Bps)',
                color = styleInterval(0, c(
                  app_config$boe_palette[2],
                  app_config$boe_palette[5]
                )),) %>%
    formatStyle('YTD Change (Bps)',
                color = styleInterval(0, c(
                  app_config$boe_palette[2],
                  app_config$boe_palette[5]
                ))) %>%
    formatStyle(0, target = 'row', lineHeight = '75%') %>%
    formatRound(c('1W Change (Bps)', '1M Change (Bps)', 'YTD Change (Bps)'),
                1) %>%
    formatRound('Latest Yield (%)', 2)
  
  
}

format_style_bank_rate <- function(table){
  
  
  table %>% 
    formatStyle('1W Change (Bps)',
                color = styleInterval(0, c(
                  app_config$boe_palette[2],
                  app_config$boe_palette[5]
                )),) %>%
    formatStyle('1M Change (Bps)',
                color = styleInterval(0, c(
                  app_config$boe_palette[2],
                  app_config$boe_palette[5]
                )),) %>%
    formatStyle('YTD Change (Bps)',
                color = styleInterval(0, c(
                  app_config$boe_palette[2],
                  app_config$boe_palette[5]
                ))) %>%
    formatStyle(0, target = 'row', lineHeight = '75%') %>%
    formatRound(c('1W Change (Bps)', '1M Change (Bps)', 'YTD Change (Bps)'),
                1) %>%
    formatRound('Latest Yield (%)', 2)
  
  
}


format_style_fx <- function(table){
  
  
  table %>%
    formatStyle(
      '1W Change (%)',
      color = styleInterval(0, c(app_config$boe_palette[2],
                                 app_config$boe_palette[5])),
      fontWeight ='bold'
    ) %>%
    formatStyle(
      '1M Change (%)',
      color = styleInterval(0, c(app_config$boe_palette[2],
                                 app_config$boe_palette[5]))
    ) %>%
    formatStyle(
      'YTD Change (%)',
      color = styleInterval(0, c(app_config$boe_palette[2],
                                 app_config$boe_palette[5]))
    ) %>%
    formatStyle(
      0, target= 'row',lineHeight='55%'
    ) %>%
    formatStyle(
      'Exchange Rate', target = "row",
      backgroundColor = styleEqual(c("Sterling ERI", "US ERI", "Euro ERI", "Japan ERI"),
                                   c(app_config$boe_palette[3], app_config$boe_palette[3],
                                     app_config$boe_palette[3], app_config$boe_palette[3]))
    ) %>%
    formatRound(c('1W Change (%)', '1M Change (%)', 'YTD Change (%)'),1) %>%
    formatRound('Latest', 1)
  
  
}

make_latam_decomp <- function(data){
  
  dfr <- data %>%
    dplyr::filter(Type %in%  c("Breakeven", "Rate"), Maturity %in% c(10)) %>%
    dplyr::group_by(Ticker) %>% 
    dplyr::mutate(Desc = paste0(Country, " ", Desc)) %>%
    dplyr::select(Date, Ticker, Desc, Value, Country) %>%
    tidyr::pivot_wider(id_cols = Date, names_from = Desc, values_from = Value) %>% 
    dplyr::arrange(Date)
  
  dfr["Brazil Real"] = dfr["Brazil 10-year"] - dfr["Brazil 10-year BE"]
  dfr["Mexico Real"] = dfr["Mexico 10-year"] - dfr[ "Mexico 10-year BE"]
  dfr["Chile Real"] = dfr["Chile 10-year" ] - dfr["Chile 10-year BE"]
  
  df1 <- dfr
  df2 <- dfr
  df3 <- dfr
  
  dfrd <- dfr %>% 
    dplyr::mutate_if(is.numeric, list(~(. -lag(.,ytd_daycount))*100)) %>%
    dplyr::mutate_if(is.numeric, round, 1) %>%
    dplyr::filter(Date == max(Date)) %>%
    dplyr::select(-Date) %>%
    rbind(c(rep(c("Brazil", "Mexico", "Chile"), 3))) %>%
    rbind(c(rep("Nominal", 3), rep("Inflation", 3), rep("Real", 3))) %>% 
    as.data.frame()
  
  
  row.names(dfrd) <- c("ValueYTD", "Country", "Type")
  dfrd <- data.frame(t(dfrd))
  
  return(dfrd)
  
}

make_emea_decomp <- function(data){
  
  dfr <- data %>%
    dplyr::filter(Type %in%  c("Breakeven", "Rate"), Maturity %in% c(5)) %>%
    dplyr::group_by(Ticker) %>% 
    dplyr::mutate(Desc = paste0(Country, " ", Desc)) %>%
    dplyr::select(Date, Ticker, Desc, Value, Country) %>%
    tidyr::pivot_wider(id_cols = Date, names_from = Desc, values_from = Value) %>% 
    dplyr::arrange(Date)
  
  #dfr["Turkey Real"] = dfr["Turkey 10-year"] - dfr["Turkey 10-year BE"]
  dfr["Russia Real"] = dfr["Russia 5-year"] - dfr[ "Russia 5-year BE"]
  dfr["South Africa Real"] = dfr["South Africa 5-year" ] - dfr["South Africa 5-year BE"]
  
  df1 <- dfr
  df2 <- dfr
  df3 <- dfr
  
  dfrd <- dfr %>% 
    dplyr::mutate_if(is.numeric, list(~(. -lag(.,ytd_daycount))*100)) %>%
    dplyr::mutate_if(is.numeric, round, 1) %>%
    dplyr::filter(Date == max(Date)) %>%
    dplyr::select(-Date) %>%
    rbind(c(rep(c("Russia", "South Africa"), 3))) %>%
    rbind(c(rep("Nominal", 2), rep("Inflation", 2), rep("Real", 2)))
  
  
  row.names(dfrd) <- c("ValueYTD", "Country", "Type")
  dfrd <- data.frame(t(dfrd))
  
  return(dfrd)
  
}

get_correlation_series <- function(country, data){
  
  df_equity <- data %>%
    dplyr::filter(Date >= as.Date(paste0(year(Sys.Date()) - 3, "-12-31"))) %>%
    dplyr::filter(Main == "Yes") %>%
    dplyr::select(Country, Date, Ticker, Desc, Value7D) %>%
    dplyr::filter(grepl(x = Country, pattern = country, ignore.case = T)) %>%
    tidyr::pivot_wider(id_cols = Date, names_from = Desc, values_from = c(Value7D)) %>%
    dplyr::arrange(Date)
  
  df_rate <- data %>%
    dplyr::filter(Date >= as.Date(paste0(year(Sys.Date()) - 3, "-12-31"))) %>%
    dplyr::filter(grepl(x = Desc, pattern = "10-year", ignore.case = T)) %>%
    dplyr::select(Country, Date, Ticker, Desc, ValueBps7D) %>%
    dplyr::filter(grepl(x = Country, pattern = country, ignore.case = T)) %>%
    tidyr::pivot_wider(id_cols = Date, names_from = Desc, values_from = c(ValueBps7D)) %>%
    dplyr::arrange(Date)
  
  if(nrow(df_equity) < 1 | nrow(df_rate) < 1){return(data.frame())}
  
  df <- dplyr::left_join(df_equity, df_rate, by = c("Date"))
  df <- data.frame(df) %>% tidyr::drop_na()
  
  i = 1
  output = NULL
  while (i<=22) {
    correlation = NA
    output = rbind(output, correlation)
    i=i+1
  }
  while (i >22 & i<= nrow(df)) {
    correlation = cor(df[,2][(i-22):(i)], df[,3][(i-22):(i)])
    output = rbind(output, correlation)
    i=i+1
  }
  
  df <- df %>% dplyr::select(Date)
  df$Correlation = output[,1]
  df$country <- paste0(country)
  
  return(df)
  
}



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


market_stress_create_datatable <- function(data_input,
                                           row_group_index,
                                           underline_tickers = FALSE) {
  
  datatable(
    data_input,
    extensions = c("RowGroup"),
    rownames = FALSE,
    escape = FALSE,
    filter = 'top',
    selection = "none",
    options = list(
      scrollY = "800px",
      scrollCollapse = TRUE, 
      ordering = FALSE,
      # dom = "",
      paging = FALSE,
      # fillContainer = TRUE,
      #scroller = TRUE,
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
      c("Current","Value7D", "Value30D",  "ValueYTD",
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
