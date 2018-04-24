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
#'  revealjs::revealjs_presentation:
#'    theme: white
#'    highlight: pygments
#'    center: true
#'    incremental: true
#'    transition: fade
#'    self_contained: false
#'    reveal_plugins: ["notes", "search"]
#'  md_document:
#'    variant: markdown_github
#'    toc: false
#'    toc_depth: 6
#'  pdf_document: default  
#'title: "Collection of R Scripts of the Agromet project"
#'date: "24 April, 2018"
#'---


#+ ---------------------------------
#' 
#+ preparation, echo=FALSE, warning=FALSE, message=FALSE, error=FALSE, results='hide'

# Avoid interference with old variables by cleaning the Global Environment
rm(list=ls(all=TRUE))

# Automagically set the wd and the root of all projects 
if (!require("here")) install.packages("here")
library(here)
wd.chr <- here::here()


#+ ---------------------------------
#' ## Project presentation  
#' 
#' Welcome to the __agrometeor-public__ repository, the publicly available R scripts repository related to the [Agromet project](http://www.cra.wallonie.be/fr/agromet).
#' The present work is under major development phase. If you find a bug or want to suggest an idea, please open an issue.  
#' Want to contribute ? Feel free to create a pull request.  
#+ presentation, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

#+ ---------------------------------
#' ## Available functions 
#' 
#+ tree, echo=FALSE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

# list all the .R Files in the project (these must be knitr ready - YAML header + #' comments) and render to html
r_files <- list.files("./R", full.names = FALSE, pattern="*.R", recursive = TRUE)
lapply(seq_along(r_files), function(x) rmarkdown::render(paste0("./R/",r_files[[x]])))


# list all the documentation files in the project and print those as md links
html_files <- list.files("./R", full.names = FALSE, pattern="*.html", recursive = TRUE)
url <- sapply(strsplit(x = html_files,split = "/"), "[[", 2)
names <- sapply(strsplit(x = html_files,split = "/"), "[[", 1)
cat(paste0("* [",names, "]","(./R/",names,"/",url,") \n  "))


#+ ---------------------------------
#' ## How to use the scripts ? 
#' 3 ways :  
#' 1. __Download using this page__ and add to your R project folder (if you want to stay up-to-date, you will have to manually download the latest version of the scripts)  
#' 2. __From within R, source the up-to-date version from this github repository__. To do so, you will need the little snippet presented here below :  
#'To get the github_url of the script, right click on the download link and select "copy link address"  
#' 3. __If your are familiar with git__, fork this repository and source it in your R script. Doing so, allows you to eventually suggest pull request for code improvement.
#+ usage, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

# source_github <- function(github_url.chr) {
#   # load required package
#   library(RCurl)
# 
#   # read script lines from github and evaluate
#   script <- getURL(github_url.chr, ssl.verifypeer = FALSE)
#   eval(parse(text = script),envir=.GlobalEnv)
# }

#+ ---------------------------------
#' *The agrometeor-public repository website is powered by github pages.*  
#' *Want to know how to quickly and easily publish your R work ? Check the [tutorial](https://pokyah.github.io/howto/Quickly-publish-your-R-interactive-data-visualization-tools-with-github-pages/) available on my blog.*  
#'
#+ gh-pages, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

#+ ---------------------------------
#' ## Terms of service 
#' To use the [AGROMET API](https://app.pameseb.be/fr/pages/api_call_test/) you need to provide your own user token.  
#' The present script is available under the [GNU-GPL V3](https://www.gnu.org/licenses/gpl-3.0.en.html) license and comes with ABSOLUTELY NO WARRANTY.
#' 
#' Copyright : Thomas Goossens - t.goossens@cra.wallonie.be 2018.  
#' 
#' *(This document was generated using [R software](https://www.r-project.org/) with the [knitr library](https://deanattali.com/2015/03/24/knitrs-best-hidden-gem-spin/))*.  
#+ TOS,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE  
