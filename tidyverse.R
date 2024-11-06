library(dplyr)

var_summary <- function(data, var) {
  data$min_var <- min(data[, substitute(var)])
  data$max_var <- max(data[, substitute(var)])
  data
}

var_summary <- function(data, var1, var2) {
  data |>
    summarise(
      min_var = min(.data[[var1]]),
      max_var = max(.data[[var2]])
    )
}

use_baser_var_summary <- function(data, var) {
  data$min_var <- min(data[, substitute(var)])
  data$max_var <- max(data[, substitute(var)])
  data
}

mtcars |>
  group_by(cyl) |>
  var_summary("mpg", "disp")
