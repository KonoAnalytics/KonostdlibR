#' changedfnames Function
#'
#' This function receives a dataframe, a list of current names, and a list of equal length
#' of new names.  It returns a dataframe with the new names, or stops with an error message
#' if it can not do so.
#' @param df Name of the input dataframe
#' @param namesin List of current names
#' @param namesout List of new names. Must be of equal length as namesin.
#' @keywords changedfnames
#' @export
#' @examples
#' changedfnames(df=data.frame(a=c(1,2),b=c(3,4)), namesin=c("a","b"), namesout=c("c","d"))
changedfnames <- function(df, namesin, namesout)
{
  if(length(namesin) != length(namesout)) {stop("length of namesin not equal to namesout")}
  if(length(namesin) == 0) {stop("length of namesin must be greater than zero")}
  if(length(which(names(df) %in% namesout)) > 0) {stop("a name or names in namesout already exists in df")}
  if(length(which(names(df) %in% namesin)) != length(namesin)) {stop("the names in namesin don't match the names in df")}
  for(i in 1:length(namesin))
  {
    names(df)[names(df) == namesin[i]] <- namesout[i]
  }
  df
}

