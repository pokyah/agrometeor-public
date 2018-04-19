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
#'title: "R script to get a leaflet map base template"
#'date: \ 19-04-2018\
#'---

#+ ---------------------------------
#' ## Presentation
#' This document presents the R script that allows you to create an interactive and responsive leaflet map template.  
#' The function to create tge template map is called `build_leaflet_template.fun`.
#+ presentation,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE, results='asis'

#+ ---------------------------------
#' ## Function arguments definition
#' 
#' * `records.sf`
#' A spatial dataframe in the [sf package](https://cran.r-project.org/web/packages/sf/index.html) format  
#+ setup,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE

#+ ---------------------------------
#' ## function declaration
#+ function,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE, results='asis'

build_leaflet_template.fun <- function(records.sf){
  
  # load the leaflet library
  library(leaflet)
  
  responsiveness.chr = "\'<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\'"
  
  template.map <- leaflet() %>% 
    addProviderTiles(group = "Stamen",
                     providers$Stamen.Toner,
                     options = providerTileOptions(opacity = 0.25)
    ) %>% 
    addProviderTiles(group = "Satellite",
                     providers$Esri.WorldImagery,
                     options = providerTileOptions(opacity = 1)
    ) %>% 
    fitBounds(st_bbox(extent.sf)[[1]], 
              st_bbox(extent.sf)[[2]], 
              st_bbox(extent.sf)[[3]], 
              st_bbox(extent.sf)[[4]]
    ) %>%
    addLayersControl(baseGroups = c("Stamen", "Satellite"),
                     overlayGroups = c("KNMI rain radar", "stations", "MNT", "slope", "aspect"),
                     options = layersControlOptions(collapsed = TRUE)
    ) %>%
    addEasyButton(easyButton(
      icon="fa-crosshairs", title="Locate Me",
      onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
    htmlwidgets::onRender(paste0("
                                 function(el, x) {
                                 $('head').append(",responsiveness.chr,");
                                 }"))
    return(template.map)
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
