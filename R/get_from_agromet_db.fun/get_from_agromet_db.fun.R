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
#'title: "R script to connect to Agromet db"
#'date: \`r format(Sys.Date(), " %d-%m-%Y")`\
#'---

#+ ---------------------------------
#' ## Presentation
#' This document presents the R script that allows you to retrieve data from a local installation of the Pameseb AWS network postgreSQL database (https://app.pameseb.be/fr/pages/api_call_test/).  
#' The function to get data from the databse is called `get_from_agromet_db.fun`.
#+ presentation,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE, results='asis'

#+ ---------------------------------
#' ## Prerequisites
#' In order to use this function, you need a local copy of the Agromet postgreSQL database.
#' You can install postreSQL using the Ansible's `postgresql_debian` role available at this [repository](https://framagit.org/agromet-apps/craw-ansible).
#' To restore the db on your machine, follow the instructions available on this [repository](https://framagit.org/agromet-apps/meteo_paille/blob/master/documentation/admin_db_gestion_base_de_donnee.rst)
#' If you work with Docker, you will need to make your POSTGRESQL db accessible from outside of localhost. For this purpose, you will need to edit the `pg_hba.conf` and `postgresql.conf` files located in `/etc/postregsql/9.6/`.
#' It is recommended to store your database connection settings (username, password, port, etc) in your `.Rsession` file and call these variables using `sys.getEnv("<variable_name>")` 
#' #+ prerequisites,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE, results='asis'

#+ ---------------------------------
#' ## Function arguments definition
#' 
#' * `sensors.chr`  
#' Character vector containing the name of the sensors you want to query.  
#' Available sensor names are : `tsa`, `hra`, `plu`, `ens`, `vvt`, `sunset`, `sunrise`. (You can use `all` to get them all).  
#' 
#' * `stations_sids.chr`  
#' character vector containing the ids of the stations you want to query. (You can Use `all` to get them all).  
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

# Declaration of the function to get pameseb data from the local pameseb database.
get_from_agromet_db.fun <- function(stations_sids.chr, dfrom.chr, dto.chr, sensors.chr, tsastate_id.bool){
  
  # loading required package for postgreSQL connection
  library(RPostgreSQL)
  library(dplyr)
  
  # forcing timezone to be UTC to avoid time errors -----
  Sys.setenv(TZ="UTC")
  
  # Cretating a connection with local pameseb db -----
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, host=Sys.getenv("DOCKER_PAMESEB_DB_HOST"), dbname=Sys.getenv("PAMESEB_DB_NAME"), user=Sys.getenv("PAMESEB_DB_USER"), password=Sys.getenv("PAMESEB_DB_PASSWORD"), port=Sys.getenv("PAMESEB_DB_PORT") )
  
  # Print a list of all the available tables
  print(dbListTables(con))
  
  # Create a df containing all the stations and their locations, etc.
  all_stations_meta.df <- dbReadTable(con, "agromet_station")
  
  # Get all the column names of the table containing the cleandata : agromet_cleandata
  colnames_cleandata.df <- dbGetQuery(con, statement = "SELECT * FROM agromet_cleandata WHERE 'false'")
  
  #https://www.r-bloggers.com/rpostgresql-and-schemas/
  #id | date_created | date_modified | bywho | mtime | sunrise | sunset | tsa | tha | hra | tsf | tss | ens | dvt | vvt | plu | hct | ts2 | th2 | hr2 | metadata | dvtstate_id | ensstate_id | hctstate_id | hr2state_id | hrastate_id | plustate_id | station_id | th2state_id | thastate_id | ts2state_id | tsastate_id | tsfstate_id | tssstate_id | vvtstate_id 

  
  # filter the all_stations_meta.df dataframe to only keep the stations corresponding to desired stations
  if(stations_sids.chr != "all"){
    requested_stations_meta.df <- dplyr::filter(all_stations_meta.df, sid %in% stations_sids.chr )
  }else{
    requested_stations_meta.df <- all_stations_meta.df
  }
  
  # Prepare the SQL queries statements -----
  
  # base columns
  begin_part.chr <- "SELECT mtime, station_id, "
  
  # sensors & sensor state
  sensors_part.chr <- sensors.chr
  
  if(sensors.chr == "all"){
    sensors_part.chr = "tsa,hra,plu,ens,vvt"
  }
  
  if(tsastate_id.bool==TRUE){
    sensors_part.chr = paste0(sensors_part.chr, ",", "tsastate_id")
  }
  
  # stations
  if(stations_sids.chr =="all"){
    stations_part.chr =""
  }else{
    stations_part.chr <- "WHERE "
    if(length(unlist(strsplit(stations_sids, ","))) > 1){
      stations_part.chr <- paste(stations_part.chr, "station_id IN (", stations_sids, ")"  )
    }else{
      stations_part.chr <- paste(stations_part.chr, "station_id = (", stations_sids, ")"  )
    }
  }
  
  # dates
  if(dfrom.chr=="1994-01-01 00:00" && dto.chr=="2016-12-31 23:59"){# Case : whole dates choosen
    datePart <- ""
  }else{# Case : subset of dates choosen
    datePart <- paste("mtime >= to_timestamp( '",dfrom.chr, "','YYYY-MM-DD HH24:MI') AND mtime <= to_timestamp( '",dto.chr,"','YYYY-MM-DD HH24:MI')")
  }
  
  # if we have at least one station but not all, paste "and" before dates, otherwise, paste "where"
  if(!stations_sids.chr=="all"){
    datePart <- paste("AND", datePart)
  }else{
    datePart <- paste("WHERE", datePart)
  }  
  
  # tables to select
  tablepartNonHist <- NULL
  tablepartHist <- NULL
  validdata <- FALSE
  validdata_history <- FALSE
  tables <- build_named_list.fun(validdata, validdata_history)
  
  if(dfrom.chr=="*"){
    dfrom.chr <- "1994-01-01 00:00"
    tables$validdata_history <- TRUE
  }
  if(dto.chr=="*"){
    dto.chr <- "2016-12-31 23:59"
    tables$validdata <- TRUE 
  }
  if(as.Date(dfrom.chr) < as.Date("2004-01-01")){
    tables$validdata_history <- TRUE
  }
  if(as.Date(dto.chr) >= as.Date("2004-01-01")){
    #tables <- ifelse(!is.null(tables[1]), paste(tables, ", validdata"), "validdata" )
    tables$validdata <- TRUE
  }
  if(tables$validdata==TRUE){
    tablepartNonHist <- paste("FROM", "validdata")
  }
  if(tables$validdata_history==TRUE){
    tablepartHist <- paste("FROM", "validdata_history")
  }

  # build SQL statements and execute the query -----
  statementNonHist <- paste(begin_part.chr, sensors_part.chr, tablepartNonHist, stations_part.chr, datePart)
  
  querySetNonHist <- dbGetQuery(con, statement = statementNonHist)
  cleandata.df <- querySetNonHist
  
  if(!is.null(tablepartHist)){
    statementHist <- paste(begin_part.chr, sensors_part.chr, tablepartHist, stations_part.chr, datePart)
    querySetHist <- dbGetQuery(con, statement = statementHist)
    cleandata.df <- bind_rows(querySetNonHist, querySetHist)
  }
  
  # Disconnect from postGres server -----
  dbDisconnect(con)
  
  # rename the station_id column
  cleandata.df <- rename(cleandata.df, sid= station_id)
  
  # clean filtered to only keep the positions info
  requested_stations_meta.df <- dplyr::select(requested_stations_meta.df,one_of(c("sid", "station_name", "altitude", "latitude", "longitude")))
  
  # return result
  result <- build_named_list.fun(requested_stations_meta.df, cleandata.df)
  return(result)
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
