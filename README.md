Welcome to the __agrometeor-public__ repository, the publicly available R scripts repository related to the [Agromet project](http://www.cra.wallonie.be/fr/agromet).

The live website is available @ [pokyah.github.io/agrometeor-public]

The present work is under major development phase.
If you find a bug or want to suggest an idea, please open an issue.  
Want to contribute ? Feel free to create a pull request.  

## How to use the R scripts ? 

2 ways : 

1. Download using this page and add to your R project folder (if you want to stay up-to-date, you will have to manually download the latest version of the scripts)
2. From within R, source the up-to-date version from this github repository. To do so, you will need this little snippet : 

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

## R Scripts 

| `get_from_agromet_API.fun.R` 	| get data from the Agromet API V1 	| __[>> download <<](./get_from_agromet_API.fun/get_from_agromet_API.fun.R)__ 	| [doc](./get_from_agromet_API.fun/get_from_agromet_API.fun.html) 	|   	|
|------------------------------	|----------------------------------	|-----------------------------------------------------------------------------	|-----------------------------------------------------------------	|---	|

---------------------

Maintained by Thomas Goossens (t.goossens@cra.wallonie.be)  
*Copyright : Thomas Goossens 2018*

