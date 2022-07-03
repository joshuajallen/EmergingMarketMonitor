
plotly_std_config <- function(x) {
  # Set a NULL element ID to avoid messages of the form:
  #  Ignoring explicitly provided widget ID "6c0e221658"; Shiny doesn't use them
  x$elementId <- NULL
  x
}


plotly_std_title <- function(x, title,
                             x_coord = 0.0,
                             y_coord = 1.1,
                             font = list(size = 12)) {
  add_annotations(x,
                  xref = "paper",
                  yref = "paper",
                  x = x_coord,
                  y = y_coord,
                  showarrow = FALSE,
                  text = title,
                  font = font
                  )
}


plotly_std_style <- function(x) {
  style(x,
    hoverlabel = list(
      bgcolor = "white",
      font = app_config$font_style
    )
  )
}


plotly_top_change_chart <- function(x) {

  axis_template <- list(
    showgrid = TRUE,
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    ticks = "",
    title = "",
    mirror = "all"
  )

  layout(
    x,
    xaxis = axis_template,
    yaxis = axis_template,
    margin = list(l = 30, r = 30, b = 15, t = 30, pad = 4)
  )

}


plotly_slope_chart <- function(x) {

  axis_template <- list(
    showgrid = FALSE,
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    ticks = "",
    title = "",
    mirror = "all"
  )

  layout(
    x,
    xaxis = axis_template,
    yaxis = axis_template,
    showlegend = FALSE,
    margin = list(l = 30, r = 30, b = 15, t = 30, pad = 4)
  )

}


plotly_heatmap_chart <- function(x) {

  axis_template <- list(
    showgrid = FALSE,
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    ticks = "",
    title = "",
    mirror = "all"
  )

  layout(
    x,
    xaxis = axis_template,
    yaxis = axis_template,
    margin = list(l = 30, r = 30, b = 25, t = 30, pad = 4)
  )

}


plotly_central_bank_pricing_chart <- function(x) {

  axis_template <- list(
    showgrid = FALSE,
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    ticks = "",
    title = "",
    mirror = "all"
  )

  layout(
    x,
    xaxis = list(title = ""),
    yaxis = axis_template,
    bargap = 0.8,
    margin = list(l = 30, r = 30, b = 15, t = 30, pad = 4)
  )

}

plotly_format_layout <- function(x, x_title = 'Maturity' , y_title = 'Yield (%)'){
  
  x %>%
    layout(
      xaxis = list(
        title = x_title,
        ticks = "outside",
        ticklen = 5,
        tickwidth = 2,
        tickcolor = toRGB("black"),
        showgrid = T,
        showticklabels = TRUE
      ),
      yaxis = list(
        title = y_title,
        ticks = "outside",
        ticklen = 5,
        tickwidth = 2,
        tickcolor = toRGB("black"),
        showgrid = T,
        autorange = T,
        showticklabels = TRUE,
        zeroline = T,
        showline = T #side = "right"
      ),
      margin = list(r = 50)#,
      #legend = list(orientation = 'h')
  )
  
  
}
