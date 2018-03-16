Welcome to the agrometeor-public, the publicly available R scripts repository related to the [Agromet project](http://www.cra.wallonie.be/fr/agromet).

## How to use the R scripts ? 

2 ways : 

1. Download using this page and add to your R project folder (if you want to stay up-to-date, you will have to manually download the latest version of the scripts)
2. From within R, source the required script from this github repository using. To do so, you will need this little snippet : 

```R
source_github <- function(github_url.chr) {
  # load package
  library(RCurl)

  # read script lines from website and evaluate
  script <- getURL(github_url.chr, ssl.verifypeer = FALSE)
  eval(parse(text = script),envir=.GlobalEnv)
}  
```
To get the github_url of the script, right click on the download link and select "copy link address"

## R Script to get data from the Agromet API V1

`get_from_agromet_API.fun.R` - [doc](./get_from_agromet_API.fun/get_from_agromet_API.fun.html)
__[download](./get_from_agromet_API.fun/get_from_agromet_API.fun.R)__

---------------------

Maintained by Thomas Goossens (t.goossens@cra.wallonie.be).
*Copyright : Thomas Goossens 2018*

