#' Create Event Labels
#'
#' @description Helper function that creates an HMTL-formatted information
#'     based on user-specified variables. By default uses 'location','mag' and '
#'     total_deaths' variables.
#'
#' @param data A data frame.
#' @param defaults Specifies whether the default variables 'location', 'mag'
#'     and 'total_deaths' are to be used. Defaults to `TRUE`. If `FALSE` and
#'     no additional variables are added via `...` the labell will be empty.
#' @param ... Additional variables to add to the label or instead of defaults.
#'
#' @returns A data frame with HTML-formatted label information.
#'
#' @importFrom rlang as_label
#' @importFrom stringr str_to_title
#'
#' @examples
#' library(dplyr)
#'
#' #create label column
#' southamerica %>%
#'   mutate(popup_text = eq_create_label(.)) %>%
#'   head()
#'
#' # use labels in a plot
#' southamerica %>%
#'   filter(country == "CHILE" & lubridate::year(date) >= 1970) %>%
#'   mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map(annot_col = "popup_text")
#'
#' @export
eq_create_label <- function(data, defaults = TRUE, ...) {

  if (defaults) {
    variables <- list("location", "mag", "total_deaths", ...)
  } else {
    variables <- list(...)
  }

  create_label <- function(row) {
    l <- ""

    for (i in seq_along(variables)) {
      var <- rlang::as_label(variables[[i]])
      var <- gsub('"', "", var)

      var_name <- gsub("_", " ", var)
      var_name <- stringr::str_to_title(var_name)

      var_value <- row[[var]]

      if (!is.na(var_value)) {
        l <- paste0(l, "<b>", var_name, ":</b> ", var_value)

        if (i != length(variables)) {
          l <- paste0(l, "<br/>")
        }
      }
    }

    l <- sub("<br/>$", "", l)
    l <- trimws(l)
  }

  labels <- apply(data, 1, create_label)
  labels
}
