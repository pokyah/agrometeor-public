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
#'title: "R script to retrieve Agromet API data"
#'date: \`r format(Sys.Date(), " %d-%m-%Y")`\
#'---


#+ ---------------------------------
#' ## Presentation
#' This document presents the R script that allows you to retrieve data from the [AGROMET API V1](https://app.pameseb.be/fr/pages/api_call_test/).  
#' The function to get data from the official Agromet API is called `get_from_agromet_API.fun`.
#+ presentation,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE, results='asis'

#+ ---------------------------------
#' ## Function arguments definition
#' 
#' * `user_token.chr`  
#' Character vector containing your own token.  
#' 
#' * `table_name.chr`  
#' Character vector containing the name of the table you want to query.  
#' Available table names are : `station`, `cleandata`, `cleandatafio`, `rawdata`, `rawdatafio`, `get_daily`, `get_tmy`.
#' 
#' * `sensors.chr`  
#' Character vector containing the name of the sensors you want to query.  
#' Available sensor names are : `tsa`, `hra`, `plu`, `ens`, `vvt`, `sunset`, `sunrise`. (You can use `all` to get them all).  
#' 
#' * `stations_ids.chr`  
#' character vector containing the ids of the stations you want to query. (You can Use `all` to get them all).  
#' __TIP : to get the id corresponding to the station names, you need to call the `station` table__.  
#' 
#' * `dfrom.chr`  
#' Charachter vector specifying the date from which you want data.  
#' Formatted `yyyy-mm-dd` excepted for `get_tmy` where you must provide `mm-dd`
#' 
#' * `dto.chr`  
#' Charachter vector specifying the date to which you want data.  
#' Formatted `yyyy-mm-dd` excepted for `get_tmy` where you must provide `mm-dd`
#+ setup,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE

#+ ---------------------------------
#' ## function declaration
#+ function,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE, results='asis'

# Declaration of the function to get pameseb data from the Pameseb API 
get_from_agromet_API.fun <- function(user_token.chr, table_name.chr, sensors.chr=NULL, stations_ids.chr, dfrom.chr=NULL, dto.chr=NULL){
  
  # Load required libraries 
  library("jsonlite")
  library("httr")
  
  # Clean the eventual spaces in the sensors.chr string
  sensors.chr <- gsub(" ","",sensors.chr)
  
  # Build the proper table API call URL
  api_table_url.chr <- paste("https://app.pameseb.be/agromet/api/v1", table_name.chr, sensors.chr, stations_ids.chr, dfrom.chr, dto.chr, sep="/")
  
  # Add your user token into the HTTP authentication header and call API (https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)
  api_table_req.resp <- httr::GET(api_table_url.chr, httr::add_headers("Authorization" = paste("Token", user_token.chr, sep=" ")))
  
  if(api_table_req.resp$status_code!=200){
    stop(paste0("The API responded with an error ", api_table_req.resp$status_code, ". Function execution halted. \n Please check your token and the validity + order of the parameters you provided. API documentation available at https://app.pameseb.be/fr/pages/api_call_test/ " ))
  }
  cat(paste0("The API responded with a status code ", api_table_req.resp$status_code, ". Your requested data has been downloaded \n"))
  
  # Getting the JSON data from the API response
  api_results_json.chr <- httr::content(api_table_req.resp, as = "text")
  
  # Transform the JSON response to R-friendly list format
  results.l <- fromJSON(api_results_json.chr)
  
  # Remove the terms of service and version info to only keep the data
  results.df <- results.l$results
  
  # Rename the column "station" to "id" for later clarity of the code
  colnames(results.df)[which(names(results.df) == "station")] <- "id"
  
  # Create a dataframe for the stations meta-information
  stations_meta.df <- results.l$references$stations
  
  # Group in a list
  results_and_stations_meta.l <- list(stations_meta.df = stations_meta.df, records.df = results.df)

  # Present a quick overview of the results in the console
  cat("Overview of the queried results : \n")
  print.data.frame(head(results_and_stations_meta.l$records.df))
  
  # Return the results and the station_meta dataframes stored in as a list 
  return(results_and_stations_meta.l)
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
