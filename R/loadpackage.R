#' loadpackage Function
#'
#' This function allows you to load packages cleanly.  They will be downloaded from 
#' CRAN (if needed) and instantiated into memory. You will know this is successful
#' if the return value is TRUE.  FALSE indicates failure.
#' @param pname Name of the desired package. No default.
#' @param quiet if TRUE suppresses output from this function. Defaults to FALSE.

#' @keywords loadpackage
#' @export
#' @examples
#' loadpackage(pname="ggplot2", quiet=FALSE)
loadpackage <- function(pname, quiet=FALSE)
{   #installs and loads package, only if necessary
  installedpackages_m <- utils::installed.packages() #matrix of installed libraries
  installed <- (pname %in% installedpackagess_m[,1])
  if (!installed) # then install library
  {
    install.packages(pname,repos = 'http://cran.us.r-project.org')
  }
  searchstring <- paste("\"package:",pname,"\"",sep = "")
  loaded <- TRUE
  if (! (searchstring %in% search()))
  {# then require library
    loaded <- require(pname,character.only = TRUE,quietly = TRUE,warn.conflicts = FALSE)
    if (!loaded)
    {
      if(quiet) {cat("\nCan't load package ",pname,".\n",sep = "")}
    }
  } else
  {
    loaded <- TRUE
  }
  loaded
}