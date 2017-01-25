#' g_geocode Function
#'
#' This function allows you retrieve the latitude and longitude by using a Google API
#' @param address The street address.
#' @param city The city.
#' @param state The state.
#' @param api_key The Google api_key.  If NULL, a default value from getcredentials will be used.
#' @param retrytimes Number of times to try if you do not get a 200 return code
#' @keywords g_geocode
#' @export
#' @examples
#' g_geocode(addr="222 Malone St", city="Houston", state="TX")
g_geocode <- function(addr=NULL,city=NULL,state=NULL,api_key=NULL,retrytimes=5)
{
    KonostdlibR::loadpackage('httr')
    KonostdlibR::loadpackage('jsonlite')
    
    if(is.null(api_key))
    {
        api_key <- as.character(getcredentials(system="googlemaps")$api_key)
    }
    path <- "https://maps.googleapis.com/maps/api/geocode/json?"
    addr <- paste0("address=",gsub(" ","+",addr,fixed=TRUE),",")
    addr <- gsub(".","+",addr,fixed=TRUE)
    city <- paste0(gsub(" ","+",city,fixed=TRUE),",")
    city <- gsub(".","+",city,fixed=TRUE)
    state <- gsub(".","+",state,fixed=TRUE)
    path <- paste0(path,addr,"+",city,"+",state,"&key=",api_key)
    path <- gsub("\n","+",path,fixed=TRUE)
    path <- gsub("++","+",path,fixed=TRUE)
    
    goterror <- TRUE
    keeptrying <- TRUE    
    while(goterror & keeptrying)
    {
        result <- httr::GET(url=path)
        returncode <- result$status_code
        if (returncode == 200) {goterror <- FALSE}
        keeptrying <- (retrytimes > 0)
        retrytimes <- (retrytimes - 1)
        if(keeptrying & goterror) {Sys.sleep(1)}
    }
    
    if(result$status_code == 200)
    {
        df <- jsonlite::fromJSON(content(result,as="text"),flatten=TRUE)$results
        if(class(df) != "data.frame")
        {
            df<- data.frame()
        }
    }
    list(returncode=result$status_code,df=df)
}
