#' Create Event Labels
#'
#' @description
#' Dynamically creates labels
#'
#' @param data description
#'
#' @examples
#' data(mexico)
#'
#' mexico %>%
#'   eq_clean_data(.) %>%
#'   filter(year(DATE) > 2000) %>%
#'   mutate(popup_text = eq_create_label(.))
#'   eq_map(annot_col = "popup_text")
#'
#'
#'
#' @export

eq_create_label <- function(data) {

  df <- paste(
      ifelse(!is.na(data$LOCATION), paste("<b>Location:</b>", data$LOCATION, "<br/>"), ""),
      ifelse(!is.na(data$MAG), paste("<b>Magnitude:</b>", data$MAG, "<br />"), ""),
      ifelse(!is.na(data$TOTAL_DEATHS), paste("<b>Total deaths</b>:", data$TOTAL_DEATHS), "")
      )

  df
}
