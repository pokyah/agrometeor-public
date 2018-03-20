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
#' ## function declaration
#+ function,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE, results='asis'

# Declaration of the function 
prepare_agromet_API_data.fun  <- function(meta_and_cleandata.l, sensors.chr){
  
  # Create the stations positions df
  stations_positions.df <- meta_and_cleandata.l[[1]]
  
  # Create cleandata df
  cleandata.df <- meta_and_cleandata.l[[2]]
  
  # Join stations_positions.df and cleandata.df 
  tmy_period.df <- stations_positions.df$metadata$tmy_period
  stations_positions.df <- stations_positions.df %>% dplyr::select(-metadata)
  stations_positions.df <- bind_cols(stations_positions.df, tmy_period.df)
  cleandata.df <- left_join(stations_positions.df, cleandata.df, by=c("id"))
  
  # Transform sensors.chr columns from character to numeric values
  cleandata.df <- cleandata.df %>% mutate_at(vars(sensors.chr), funs(as.numeric))
  
  # Transform mtime column to posix format for easier time handling
  cleandata.df <- cleandata.df %>% mutate_at("mtime", as.POSIXct, format = "%Y-%m-%dT%H:%M:%SZ")
  
  # Transform meta columns from character to numeric altitude, longitude, latitude
  cleandata.df <- cleandata.df %>% mutate_at(vars(build_positions_names.fun()), funs(as.numeric))
  
  # Remove the records from stations with no valid location information
  cleandata.df <- cleandata.df %>% filter(!is.na(latitude))

  return(cleandata.df)
  
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
