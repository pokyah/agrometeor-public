#'---
#'author: "Thomas Goossens (CRA-W) - t.goossens@cra.wallonie.be"
#'output:
#'  html_document:
#'    theme: default
#'    toc: false
#'    toc_depth: 6
#'    toc_float:
#'      collapsed: false
#'      smooth_scroll: true
#'title: "R script to prepare data received from Agromet API data (from string to numeric and posix)"
#'date: \`r format(Sys.Date(), " %d-%m-%Y")`\
#'---


#+ ---------------------------------
#' ## Presentation
#' This document presents the R script that allows you to transform the data received from the [AGROMET API V1](https://app.pameseb.be/fr/pages/api_call_test/) into a [R tidy & friendly format](https://www.r-bloggers.com/data-manipulation-with-tidyr/).
#+ presentation,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE, results='asis'


#+ ---------------------------------
#' ## Function declaration
#+ function,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE, results='asis'

# Declaration of the function
prepare_agromet_API_data.fun  <- function(meta_and_records.l){
  
  # Create the vector of all the existing sensors in the Agromet db
  sensors.chr <- c["tsa", "tha", "hra", "tsf", "tss", "ens", "dvt", "vvt", "plu", "hct", "ts2", "th2", "hr2"]

  # Create the stations positions df
  stations_meta.df <- meta_and_records.l[[1]]

  # Create the records df
  records.df <- meta_and_records.l[[2]]

  # In stations_meta.df, tmy_period information are stored as df stored inside df. We need to extract these from this inner level and add as new columns
  tmy_period.df <- stations_meta.df$metadata$tmy_period
  stations_meta.df <- stations_meta.df %>% dplyr::select(-metadata)
  stations_meta.df <- bind_cols(stations_meta.df, tmy_period.df)

  # Join stations_meta and records by "id"
  records.df <- left_join(stations_meta.df, records.df, by=c("id"))

  # Transform sensors.chr columns from character to numeric values
  records.df <- records.df %>% mutate_at(vars(one_of(sensors.chr)), funs(as.numeric))

  # Transform mtime column to posix format for easier time handling
  records.df <- records.df %>% mutate_at("mtime", as.POSIXct, format = "%Y-%m-%dT%H:%M:%SZ")

  # Transform meta altitude, longitude, latitude columns from character to numeric
  records.df <- records.df %>% mutate_at(vars(c("altitude", "longitude", "latitude")), funs(as.numeric))

  # Return the preperly typed and structured records dataframe.
  return(records.df)
}


#+ ---------------------------------
#' ## Terms of service
#' To use the [AGROMET API](https://app.pameseb.be/fr/pages/api_call_test/) you need to provide your own user token.
#' The present script is available under the [GNU-GPL V3](https://www.gnu.org/licenses/gpl-3.0.en.html) license and comes with ABSOLUTELY NO WARRANTY.
#'
#' Copyright : Thomas Goossens - t.goossens@cra.wallonie.be 2018.
#'
#' *(This document was generated using [R software](https://www.r-project.org/) with the [knitr library](https://deanattali.com/2015/03/24/knitrs-best-hidden-gem-spin/))*.
#+ TOS,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE
