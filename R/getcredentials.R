#' getcredentials Function
#'
#' This function allows you to access credentials stored in a csv file of your chosing.
#' The benefit is that you can store your credentials in a separate location from your code.
#' The function returns a dataframe if successful, or FALSE if not.
#' @param system Name of the desired system for which you'd like to retrieve the credentials. No default.
#' @param filepathname Concatenated file and path name to the CSV. Defaults to "~/credentials/credentials.csv"
#' @param quiet If TRUE suppresses output from this function. Defaults to FALSE.
#' @keywords getcredentials
#' @export
#' @examples
#' getcredentials(system="masterdatabase", filepathname="C:/lockbox/passwords.txt")
getcredentials <- function(system=NA, filepathname = "~/credentials/credentials.csv", quiet=FALSE)
{
  if(is.na(system))
  {
    if(!quiet) {print("You must enter a system name when requesting credentials.")}
    credentials_out <- FALSE
  }else
  {
    system_lc <- tolower(system)
    credentials <- read.csv(filepathname)
    allsystems <- credentials$system
    credentials$system <- tolower(credentials$system)
    credentials <- credentials[credentials$system == system_lc,]
    if(nrow(credentials)== 0)
    {
      if(!quiet) {print(paste("Can't find ",system," when requesting credentials. Options:",sep=""))}
      if(!quiet) {print(as.character(allsystems))}
      credentials_out <- FALSE
    }else
    {
      credentials_out <- credentials
    }
  }
  credentials_out
}
