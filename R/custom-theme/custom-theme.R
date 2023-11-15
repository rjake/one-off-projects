#' Apply our theme to charts
#' @param p the plot object (ggplot, highchart, plotly)
#' @family Our themes and colors
#' @export
our_theme <- function(p, ...) {
  # deal with ggplot '+' operator
  if (missing(p)) {
    return(our_theme.missing(...))
  }
  
  if (inherits(p, "ggplot")) {
    stop("Please use the + operator", call. = FALSE)
  }
  
  # otherwise use appropriate method
  UseMethod("our_theme", p)
}


#' @inheritDotParams ggplot2::theme_grey
#' @importFrom ggplot2 update_geom_defaults theme_grey %+replace% theme
#' element_blank element_text rel element_line margin
#' @rdname our_theme
#' @export
#' @examples
#' suppressMessages(library(ggplot2))
#' 
#' ggplot(mpg, aes(cty, hwy, color = drv)) +
#'   geom_point() +
#'   scale_color_manual(values = hcl.colors(3)) +
#'   labs(
#'     title = "This is ggplot",
#'     caption = "This is the caption"
#'   ) +
#'   our_theme()
#'
our_theme.missing <- function(...) {
  update_geom_defaults("point", list(size = 2.5))
  
  font_style <- function(size = rel(1), ...) {
    element_text(
      size = size,
      color = "#665546",
      ...
    )
  }
  
  theme_grey(...) %+replace%
    theme(
      plot.background = element_blank(),
      plot.title.position = "plot",
      plot.title = font_style(
        size = rel(1.35),
        hjust = 0,
        margin = margin(t = 10, b = 15, unit = "pt")
      ),
      plot.subtitle = font_style(
        size = rel(1.1),
        hjust = 0,
        margin = margin(t = -5, b = 15, unit = "pt")
      ),
      plot.caption = font_style(size = rel(0.9), hjust = 1, vjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_line(color = "#e0ddda", linewidth = 0.7),
      axis.line = element_line(color = "#e0ddda", linewidth = 0.6),
      axis.text = font_style(size = rel(0.9)),
      axis.title.y = font_style(
        vjust = 1.5, angle = 90,
        margin = margin(r = 10, l = 5)
      ),
      axis.title.x = font_style(
        vjust = -0.5,
        margin = margin(t = 5, b = 5)
      ),
      legend.title = font_style(hjust = 0),
      legend.key = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = rel(0.9)),
      legend.justification = "center"
    )
}


#' @inherit highcharter::hc_theme details
#' @importFrom highcharter hc_theme hc_theme_merge
#' @inheritDotParams highcharter::hc_theme
#' @export
#' @rdname our_theme
#' @examples
#' suppressMessages(library(highcharter))
#' 
#' highcharter::hchart(
#'   mpg,
#'   type = "scatter",
#'   highcharter::hcaes(cty, hwy, group = drv)
#' ) |>
#'   hc_colors(hcl.colors(3)) |>
#'   hc_title(text = "This is highchart") |>
#'   hc_credits(enabled = TRUE, text = "This is the caption") |>
#'   our_theme()
#'
our_theme.highchart <- function(p, ...) {
  
  font_style <- function(size, ...) {
    list(
      style =
        list(
          fontSize = paste0(size, "px"),
          color = "#665546"
        ),
      ...
    )
  }
  
  axis_opts <-
    list(
      labels = font_style(size = 13),
      title = font_style(size = 14),
      startOnTick = FALSE,
      lineWidth = 0.75,
      tickWidth = 0.75,
      tickLength = 5,
      lineColor = "#e0ddda",
      gridLineColor = "#FFFFFF",
      tickColor = "#e0ddda",
      minorTickInterval = 0,
      minorGridLineColor = "#FFFFFF"
    )
  
  theme <- hc_theme(
    chart = list(plotBackgroundColor = "#FFFFFF"),
    title = font_style(size = 20, align = "left"),
    subtitle = font_style(size = 16, align = "left"),
    credits = font_style(size = 12),
    xAxis = axis_opts,
    yAxis = axis_opts,
    legendBackgroundColor = "rgba(0, 0, 0, 0.5)",
    background2 = "#505053",
    dataLabelsColor = "#B0B0B3",
    textColor = "#665546",
    contrastTextColor = "#F0F0F3",
    maskColor = "rgba(255,255,255,0.3)"
  )
  
  if (length(list(...)) > 0) {
    theme <- hc_theme_merge(
      theme,
      hc_theme(...)
    )
  }
  
  p |>
    hc_add_theme(theme)
}


#' @importFrom plotly layout
#' @importFrom stringr str_count
#' @export
#' @rdname our_theme
#' @examples
#'
#' suppressMessages(library(plotly))
#' suppressMessages(library(stringr))
#' 
#' plot_ly(
#'   data = mpg,
#'   x = ~cty,
#'   y = ~hwy,
#'   color = ~drv,
#'   colors = hcl.colors(3),
#'   type = "scatter",
#'   mode = "markers"
#' ) |>
#'   layout(
#'     title = list(text = "This is plotly"),
#'     legend = list(title = list(text = "drv"))
#'   ) |>
#'   our_theme()
our_theme.plotly <- function(p, ...) {
  font_style <- function(size) {
    list(
      size = size,
      color = "#665546"
    )
  }
  
  axis_opts <- function() {
    list(
      showgrid = FALSE,
      tickfont = font_style(13),
      title = list(font = font_style(14)),
      zerolinecolor = "#e0ddda",
      zerolinewidth = 0.75,
      tickcolor = "#e0ddda",
      ticklen = 5
    )
  }
  
  # find the title length for margin so title isn't displayed over chart
  plot_title <- p$x$layoutAttrs[[1]]$title$text
  
  if (is.null(plot_title)) {
    title_margin <- 25
  } else {
    title_margin <- (1 + stringr::str_count(plot_title, "<br>|\\\n")) * 45
  }
  
  template <-
    list(
      ...,
      plot_bgcolor = "#FFFFFF",
      title = list(
        x = 0.02,
        y = 0.97,
        pad = list(t = 5),
        font = font_style(20)
      ),
      xaxis = axis_opts(),
      yaxis = axis_opts(),
      legend = list(
        orientation = "h",
        xanchor = "center",
        x = 0.5,
        y = -0.2
      )
    )
  
  p |>
    layout(
      margin = list(t = title_margin),
      template = list(
        data = list(), # this needs to be here for template to work
        layout = template
      )
    )
}
