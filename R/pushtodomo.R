#' pushtodomo Function
#'
#' This function allows you to execute a mysql statement against a mysql database. The only required parameter is
#' the mysql statement.  All others may be NULL.
#' The function returns a dataframe.
#' @param df The data frame you would like to push  to Domo.
#' @param datasetname The name of the data set you would like to push to Domo.
#' @param api_key, The api key issued by Domo.
#' @param organization The organization as determined by Domo.
#' @param createdataset TRUE if you want to create a new data set, FALSE otherwise
#' @param dbname The name of the database.
#' @keywords pushtodomo
#' @export
#' @examples
#' pushtodomo(df=data.frame(a=c(1,2),b=c(3,4)), datasetname="Test Dataset", createdataset=TRUE)
pushtodomo <- function(df, datasetname, api_key=NULL, organization="KonoAnalytics", createdataset=FALSE)
{
    loadpackage("DomoR")
    
    if(is.null(api_key))
    {
        api_key <- as.character(KonostdlibR::getcredentials("Domo")$api_key)
    }
    DomoR::init(organization,api_key)
    if(createdataset)
    {
        DomoR::create(df, name=datasetname)
    }else
    {
        DataSetInfo <- DomoR::list_ds(name=datasetname)
        replace <- DomoR::replace_ds(DataSetInfo$id, df)
    }
}