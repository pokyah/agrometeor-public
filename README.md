Project presentation
--------------------

Welcome to the **agrometeor-public** repository, the publicly available
R scripts repository related to the [Agromet
project](http://www.cra.wallonie.be/fr/agromet). The present work is
under major development phase. If you find a bug or want to suggest an
idea, please open an issue.  
Want to contribute ? Feel free to create a pull request.

Available functions
-------------------

-   [build\_leaflet\_template.fun](./R/build_leaflet_template.fun/build_leaflet_template.fun.html)
-   [get\_from\_agromet\_API.fun](./R/get_from_agromet_API.fun/get_from_agromet_API.fun.html)
-   [get\_from\_agromet\_db.fun](./R/get_from_agromet_db.fun/get_from_agromet_db.fun.html)
-   [helpers\_functions](./R/helpers_functions/helpers.html)
-   [prepare\_agromet\_API\_data.fun](./R/prepare_agromet_API_data.fun/prepare_agromet_API_data.fun.html)

How to use the scripts ?
------------------------

3 ways : 1. **Download using this page** and add to your R project
folder (if you want to stay up-to-date, you will have to manually
download the latest version of the scripts) 2. **From within R, source
the up-to-date version from this github repository**. To do so, you will
need the little snippet presented here below : To get the github\_url of
the script, right click on the download link and select "copy link
address" 3. **If your are familiar with git**, fork this repository and
source it in your R script. Doing so, allows you to eventually suggest
pull request for code improvement.

    source_github <- function(github_url.chr) {
      # load required package
      library(RCurl)

      # read script lines from github and evaluate
      script <- getURL(github_url.chr, ssl.verifypeer = FALSE)
      eval(parse(text = script),envir=.GlobalEnv)
    }

*The agrometeor-public repository website is powered by github pages.*  
*Want to know how to quickly and easily publish your R work ? Check the
[tutorial](https://pokyah.github.io/howto/Quickly-publish-your-R-interactive-data-visualization-tools-with-github-pages/)
available on my blog.*

Terms of service
----------------

To use the [AGROMET API](https://app.pameseb.be/fr/pages/api_call_test/)
you need to provide your own user token.  
The present script is available under the [GNU-GPL
V3](https://www.gnu.org/licenses/gpl-3.0.en.html) license and comes with
ABSOLUTELY NO WARRANTY.

Copyright : Thomas Goossens - <t.goossens@cra.wallonie.be> 2018.

*(This document was generated using [R
software](https://www.r-project.org/) with the [knitr
library](https://deanattali.com/2015/03/24/knitrs-best-hidden-gem-spin/))*.
