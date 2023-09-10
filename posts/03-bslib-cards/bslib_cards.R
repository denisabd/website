pop_card <- function(
    df,
    template = "{measure} {trend}d by {change_abs}, from {pytd} to {ytd}",
    bg_color = "white",
    positive_color = "#228b22",
    negative_color = "#e32636",
    current_year = NULL,
    py_date = NULL,
    total = TRUE,
    type = "yoy",
    title = NULL,
    return_data = FALSE,
    ...) {

  if (type == 1) {
    type <- "yoy"
  } else if (type == 2) {
    type <- "previous period"
  } else if(type == 3) {
    type <- "same period last year"
  }

  if (!type %in% c("yoy", "previous period", "same period last year")) {
    stop('Argument type must be "yoy", "previous period" or "same period last year"')
  }

  df <- df %>%
    dplyr::ungroup()

  # date field
  date_field <- df %>%
    dplyr::select_if(lubridate::is.timepoint) %>%
    names()

  if (length(date_field) < 1) return(NULL)

  date_field <- date_field[1]

  # measures available
  measure <- df %>%
    dplyr::select_if(is.numeric) %>%
    names()

  # getting the df frequency
  frequency <- narrator::get_frequency(df)

  max_date <- max(df[[date_field]])

  prev_date <- df %>%
    dplyr::filter(base::get(date_field) != max_date) %>%
    dplyr::select(dplyr::all_of(date_field)) %>%
    as.matrix() %>%
    as.Date(origin = "1970-01-01") %>%
    max()

  n_measures <- length(measure)

  if (n_measures != 1) return(NULL)

  # Leaving only last period and same period last year for analysis
  if (type == "same period last year") {
    df <- df %>%
      dplyr::filter(base::get(date_field) %in% c(max_date, narrator::get_py_date(df)))

    prev_date <- max_date - lubridate::years(1)
  } else if (type == "previous period") {
    # Current and previous periods are filtered and then previous gets transformed
    # in order to make it occur exactly a year before max_date and make
    # ytd_volume and pytd_volume work
    df <- df %>%
      dplyr::filter(base::get(date_field) %in% c(max_date, prev_date)) %>%
      dplyr::mutate(!!date_field := ifelse(
        base::get(date_field) == prev_date,
        max_date - lubridate::years(1),
        base::get(date_field))
      ) %>%
      dplyr::mutate(!!date_field := as.Date(base::get(date_field), origin = "1970-01-01"))
  }

  # Period name calculation
  if (type == "yoy") {
    period_name <- "YTD"
    prev_period_name <- "Prior YTD"
  } else {
    if (frequency == "month") {
      period_name <- paste(lubridate::month(max_date,label = TRUE, abbr = TRUE), lubridate::year(max_date))
      prev_period_name <- paste(lubridate::month(prev_date,label = TRUE, abbr = TRUE), lubridate::year(prev_date))
    } else if (frequency == "week") {
      period_name <- paste("Week", lubridate::week(max_date), lubridate::year(max_date))
      prev_period_name <- paste("Week", lubridate::week(prev_date), lubridate::year(prev_date))
    } else if (frequency == "quarter") {
      period_name <- paste0("Q", lubridate::quarter(max_date), " ", lubridate::year(max_date))
      prev_period_name <- paste0("Q", lubridate::quarter(prev_date), " ", lubridate::year(prev_date))
    } else if (frequency == "year") {
      period_name <- lubridate::year(max_date)
      prev_period_name <- lubridate::year(prev_date)
    } else {
      period_name <- paste("Day", lubridate::yday(max_date), lubridate::year(max_date))
      prev_period_name <- paste("Day", lubridate::yday(prev_date), lubridate::year(prev_date))
    }
  }

  period_year <- lubridate::year(max_date)

  # Create Title based on the type of table
  if (type == "yoy") {
    title <- "YTD vs Prior YTD"
  } else if (type == "same period last year") {
    title <- paste(period_name, "vs",
                   stringr::str_replace(
                     period_name, toString(period_year), toString(period_year - 1)
                   )
    )
  } else if (type == "previous period") {
    title <- glue::glue("{period_name} vs {prev_period_name}")
  }

  ytd <- narrator:::ytd_volume(df)
  pytd <- narrator:::pytd_volume(df)

  change_raw <- ytd - pytd
  change_abs <- narrator::format_num(abs(change_raw), decimals = 2)
  change <- narrator::format_num(change_raw, decimals = 2)
  change_p <- paste0(round((ytd/pytd - 1)*100, 1), "%")

  ytd <- narrator::format_num(ytd, decimals = 2)
  pytd <- narrator::format_num(pytd, decimals = 2)

  trend <- ifelse(change_raw < 0, "decrease", "increase")

  color <- ifelse(change_raw >= 0, positive_color, negative_color)

  card <- bslib::card(
    bslib::card_body(
      style = "display: inline; align-items: center;",

      htmltools::tags$h3(htmltools::tags$b(measure)),
      htmltools::tags$h6(title),
      htmltools::tags$br(),

      htmltools::div(
        style = paste0("color:", color, "; margin-left: 10px; display:inline; align-items: center;"),
        htmltools::tagList(
          phosphoricons::ph("trend-up", weight = "bold", height = "2em"),
          htmltools::HTML(
            paste0('<h2 style="display:inline;"><b>', change_p, '</b></h2>')
          )
        )
      ),

      htmltools::tags$br(), htmltools::tags$br(),
      glue::glue(template),
      style = paste0("background-color: ", bg_color, ";"),
      max_height = 200,
      min_height = 100
    ),
    full_screen = FALSE
  )

  if (return_data == TRUE) {
    variables <- list(
      measure = measure,
      trend = trend,
      period_name = period_name,
      prev_period_name = prev_period_name,
      title = title,
      ytd = ytd,
      pytd = pytd,
      change_raw = change_raw,
      change_abs = change_abs,
      change = change,
      change_p = change_p
    )
    return(variables)
  }

  return(card)
}

df <- sales %>%
  dplyr::mutate(Date = lubridate::floor_date(Date, unit = "months")) %>%
  dplyr::group_by(Region, Product, Date) %>%
  dplyr::summarise(Sales = sum(Sales, na.rm = TRUE))

pop_card(df)

template = "{measure} {trend}d by {change_abs}, from {pytd} to {ytd}"
bg_color = "white"
positive_color = "#228b22"
negative_color = "#e32636"
current_year = NULL
py_date = NULL
total = TRUE
type = "yoy"
title = NULL
return_data = FALSE

library(tidyverse)
pop_card(df)
