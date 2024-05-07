eq_create_label <- function(data) {

  df <- paste(
      ifelse(!is.na(data$LOCATION), paste("<b>Location:</b>", data$LOCATION, "<br />"), ""),
      ifelse(!is.na(data$MAG), paste("<b>Magnitude:</b>", data$MAG, "<br />"), ""),
      ifelse(!is.na(data$TOTAL_DEATHS), paste("<b>Total deaths:</b>", data$TOTAL_DEATHS, "<br />"), "")
      )
  print(head(df))
  df
}


eq_clean %>%
  filter(COUNTRY == "MEXICO" & lubridate::year(DATE) > 2000) %>%
  mutate(popup_text = paste("<b>Location:</b>", LOCATION, "<br />",
         "<b>Magnitude:</b>", MAG, "<br />",
         "<b>Total Deaths:</b>", TOTAL_DEATHS, "<br />")) %>%
  eq_map(annot_col = "popup_text")
  mutate(popup_text = eq_create_label(.)) %>%
   mutate(popup_text = paste(
    ifelse(!is.na(LOCATION), '"<b>Location:</b>"', ""),
    ifelse(!is.na(LOCATION), paste(LOCATION, as.character('"<br />"')), ""),
    ifelse(!is.na(MAG), paste("<b>Magnitude:</b>"), ""),
    ifelse(!is.na(MAG), paste("<b>Magnitude:</b>", MAG, "<br />"), ""),
    ifelse(!is.na(TOTAL_DEATHS), paste("<b>Total deaths:</b>", TOTAL_DEATHS, "<br />"), ""),
    ifelse(!is.na(TOTAL_DEATHS), paste("<b>Total deaths:</b>", TOTAL_DEATHS, "<br />"), ""),
           collapse = ""
  )) %>%
  select(LOCATION, popup_text)
