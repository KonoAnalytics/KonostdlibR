#' runmysql Function
#'
#' This function allows you to execute a mysql statement against a mysql database. The only required parameter is
#' the mysql statement.  All others may be NULL.
#' The function returns a dataframe.
#' @param statement The MYSQL statement you would like to execute.
#' @param con A connection of type MySQLConnection as described in theRMySQL package. If NULL, one will be created.
#' @param user The user id of the database.
#' @param password The password of the dataase.
#' @param host Where the database resides.
#' @param dbname The name of the database.
#' @keywords runmysql
#' @export
#' @examples
#' runmysql(statement = "select * from tbl_main")
runmysql <- function(statement,con=NULL, user=NULL, password=NULL, host=NULL, dbname=NULL)
{
    library('RMySQL')
    library('KonostdlibR')
    if (is.null(con))
    {
        if (is.null(user)) {user <- as.character(KonostdlibR::getcredentials("AWSMySQLSandbox")$userid)}
        if (is.null(password)) {password <- as.character(KonostdlibR::getcredentials("AWSMySQLSandbox")$password)}
        if (is.null(host)) {host <- as.character(KonostdlibR::getcredentials("AWSMySQLSandbox")$host)}
        if (is.null(dbname)) {dbname <- as.character(KonostdlibR::getcredentials("AWSMySQLSandbox")$dbname)}
        con <- RMySQL::dbConnect(MySQL(),user=user, password=password, host=host, dbname=dbname)
    }
    dbGetQuery(con, statement = statement)
}
