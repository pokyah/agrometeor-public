Welcome to the __agrometeor-public__ repository, the publicly available R scripts repository related to the [Agromet project](http://www.cra.wallonie.be/fr/agromet).


The present work is under major development phase.
If you find a bug or want to suggest an idea, please open an issue.  
Want to contribute ? Feel free to create a pull request.  

## Project tree

* [LICENSE](./LICENSE)
* [_config.yml](./_config.yml)
* [tree-md.sh](./tree-md.sh)
* [get_from_agromet_API.fun](./get_from_agromet_API.fun)
  * [get_from_agromet_API.fun.R](./get_from_agromet_API.fun/get_from_agromet_API.fun.R)
  * [get_from_agromet_API.fun.html](./get_from_agromet_API.fun/get_from_agromet_API.fun.html)
* [README.md](./README.md)
* [prepare_agromet_API_data.fun](./prepare_agromet_API_data.fun)
    * [prepare_agromet_API_data.fun.R](./prepare_agromet_API_data.fun/prepare_agromet_API_data.fun.R)
    * [prepare_agromet_API_data.fun.html](./prepare_agromet_API_data.fun/prepare_agromet_API_data.fun.html)

## How to use the R scripts ?

3 ways :

1. Download using this page and add to your R project folder (if you want to stay up-to-date, you will have to manually download the latest version of the scripts)
2. From within R, source the up-to-date version from this github repository. To do so, you will need this little snippet :

```R
source_github <- function(github_url.chr) {
  # load required package
  library(RCurl)

  # read script lines from github and evaluate
  script <- getURL(github_url.chr, ssl.verifypeer = FALSE)
  eval(parse(text = script),envir=.GlobalEnv)
}  
```
To get the github_url of the script, right click on the download link and select "copy link address"

3. __If your are familiar with git__, fork this repository and source it in your R script. Doing so, allows you to eventually suggest pull request for code improvment.

## R Scripts

* `get_from_agromet_API.fun.R` :  get data from the Agromet API V1  
(__[>> download <<](https://raw.githubusercontent.com/pokyah/agrometeor-public/master/get_from_agromet_API.fun/get_from_agromet_API.fun.R)__ - [doc](./get_from_agromet_API.fun/get_from_agromet_API.fun.html))

* `prepare_agromet_API_data.fun` :  Make the character data received from Agromet API V1 data R-friendly
(__[>> download <<](https://raw.githubusercontent.com/pokyah/agrometeor-public/master/prepare_agromet_API_data.fun/prepare_agromet_API_data.fun.R)__ - [doc](./prepare_agromet_API_data.fun/prepare_agromet_API_data.fun.html))


---------------------

*The agrometeor-public repository website is powered by github pages.*  
*Want to know how to quickly and easily publish your R work ? Check the [tutorial](https://pokyah.github.io/howto/Quickly-publish-your-R-interactive-data-visualization-tools-with-github-pages/) available on my blog.*  

Maintained by Thomas Goossens (t.goossens@cra.wallonie.be)  
*Copyright : Thomas Goossens 2018*
