## S3 method for custom themes
The [`custom-theme.R`](custom-theme.R) script provides an example creating a custom theme that can be added to different chart types.

The basic structure looks like this:
```r
# S3 method
our_theme <- function(p, ...) {
  # error if the |> operator is used on ggplot object
  if (inherits(p, "ggplot")) {
    stop("Please use the + operator", call. = FALSE)
  }

  # object type will be missing when using the + operator for ggplot objects
  if (missing(p)) {
    return(our_theme.missing(...))
  }  
  # otherwise use appropriate method
  UseMethod("our_theme", p)
}


# used for ggplot objects
our_theme.missing <- function(...) {

}

# when inherits(p, "highchart")
our_theme.highchart <- function(p, ...) {

}

# when inherits(p, "plotly")
our_theme.plotly <- function(p, ...) {
   
}

```

## These are the results

### ggplot
```r
ggplot2::ggplot(mpg, aes(cty, hwy, color = drv)) +
  geom_point() +
  scale_color_manual(values = hcl.colors(3)) +
  our_theme()      # <----- our_theme.missing()
```
________ original _________________________________ ________ with theme _____________________________

<img src="img/ggplot-base.png" height="250"/>  <img src="img/ggplot-theme.png" height="250"/>


### highcharter
```r

highcharter::hchart(mpg, type = "scatter", hcaes(cty, hwy, group = drv)) |>
  hc_colors(hcl.colors(3)) |>
  our_theme()      # <----- our_theme.highchart()
```
________ original _________________________________ ________ with theme _____________________________

<img src="img/highcharter-base.png" height="250"/>  <img src="img/highcharter-theme.png" height="250"/>

### plotly
```r
plotly::plot_ly(
  data = mpg,
  x = ~cty,
  y = ~hwy,
  color = ~drv,
  colors = hcl.colors(3),
  type = "scatter",
  mode = "markers"
) |>
  our_theme()      # <----- our_theme.plotly()
```
________ original _________________________________ ________ with theme _____________________________

<img src="img/plotly-base.png" height="250"/>  <img src="img/plotly-theme.png" height="250"/>
