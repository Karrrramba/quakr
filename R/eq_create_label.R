#' Create Event Labels
#'
#' @description
#' Helper function that creates a new information column composed of:
#' date (in %Y-%m-%d format), magnitude and total number of casualties.
#' The resulting information are newline separated to keep the labels
#' adequately small. The labels do not show missing values. Instead, the respecitve
#' information is ommitted.
#'
#' @param data description
#'
#' @examples
#' data(mexico)
#'
#' mexico %>%
#'   eq_clean_data(.) %>%
#'   filter(year(date) > 2000) %>%
#'   mutate(popup_text = eq_create_label(.))
#'   eq_map(annot_col = "popup_text")
#'
#' @export

eq_create_label <- function(data) {

  df <- paste(
      ifelse(!is.na(data$location), paste("<b>Location:</b>", data$location, "<br/>"), ""),
      ifelse(!is.na(data$mag), paste("<b>Magnitude:</b>", data$mag, "<br />"), ""),
      ifelse(!is.na(data$total_deaths), paste("<b>Total deaths</b>:", data$total_deaths), "")
      )

  df
}
