
library(dplyr)
head(starwars)

#use [[]] for character vectors

star_summary <- function(data, var1, var2, var3){
  data |>
    summarise(
      mean_var = mean(.data[[var1]]),
      max_var = max(.data[[var1]]),
      min_var = min(.data[[var1]]),
      mean_var = mean(.data[[var2]]),
      max_var = max(.data[[var2]]),
      min_var = min(.data[[var2]]),
      mean_var = mean(.data[[var3]]),
      max_var = max(.data[[var3]]),
      min_var = min(.data[[var3]])
    )
}

starwars |>
  group_by(homeworld) |>
  star_summary("height", "mass", "birth_year")



starwars_mass_summary <- function(group_var) {
  starwars |>
    group_by({{group_var}}) |>
    summarize(
      mean_mass = mean(.data$mass, na.rm = TRUE),
      sd_mass = sd(.data$mass, na.rm = TRUE)
    )
}

#summarizes height
height_sum <- function(data, group_var) {
  data |>
    dplyr::group_by({{ group_var }}) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_height = mean(.data$height)
    )
}

#using elipses but this only works in place of group variables, doesn't handle character strings. can only be used in one place.
height_sum_e <- function(data, ...) {
  data |>
    dplyr::group_by(...) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_height = mean(.data$height)
    )
}

height_sum(starwars, hair_color)

#dynamic column naming with dots :=

var_summary<-function (data, var, var_name){
  data |>
    summarise(
      "{var_name}" := min({{var}})
    )
}
mtcars |>
  group_by(cyl) |>
var_summary(mpg, "min_mpg")


var_summary<-function (data, var){
  data |>
    summarise(
      "{{var}}_min" := min({{var}})
    )
}

mtcars |>
  group_by(cyl) |>
  var_summary(mpg)

#dynamic dots to starwars
sum_var <- function(data,group_var, var) {
  data |>
    dplyr::group_by({{group_var}}) |>
    dplyr::summarise(
      n = dplyr::n(),
      "mean_{{var}}" := mean({{var}}, na.rm=TRUE)
    )
}

starwars |>
  sum_var(eye_color, height)


homework1 <- function(var){
  starwars |>
    summarise("mean_{{var}}" := mean({{var}}, na.rm = T),
              "max_{{var}}" := max({{var}}, na.rm = T),
              count = n())
}
homework1(height)

##adapting homework1 for
homework1 <- function(var){
  starwars |>
    summarise("mean_{var}" := mean(.data[[var]], na.rm = T),
              "max_{var}" := max(.data[[var]], na.rm = T),
              count = n())
}

homework1("height")


# across(), all_of(), any_of()
# across(), inside data-masking verbs

summy<-function (df, group_var, cols){
  df |>
    group_by({{ group_var}}) |>
    summarise(
      across({{ cols}}, .fns = list(min = min, max = max))
    )
}
mtcars |>
  summy(cyl, c(mpg, disp))

mtcars |>
  summy (cyl, where(is.numeric))
