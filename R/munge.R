#' Munge
#'
#' Quick and dirty simple munging for data frames with mostly character data.
#'
#' Note - if this throws the error "Error in FUN(X[[i]], ...) : invalid multibyte string 203",
#' you've got some weird characters in your dataset. Add '  fileEncoding="latin1  ' to the end
#' of your read.csv line for the data frame that you read in.
#' @param df matrix or data table
#' @return lowercase data frame with white space trimmed. numeric, Date, and integer columns are left alone.
#' @keywords data munging
#' @export
#' @
#' @details many thanks to this stack overflow answer: http://stackoverflow.com/questions/17964513/subset-variables-in-data-frame-based-on-column-type
#' @examples
#' df = data.frame(number = numeric(c(1,2,3)),
#'      characternumber = c("one ", " TwO", "ThrEe"),
#'      city = c(" Memphis", "MEMPHIS", "memphis "))
#' munge(df)

munge = function(df){
  require(dplyr)
  require(lubridate)

  df[,!sapply(df, is.numeric) & !sapply(df,is.integer) &
               !sapply(df,is.Date)] =
    lapply((df[,!sapply(df, is.numeric) & !sapply(df,is.integer) &
         !sapply(df,is.Date)]), trimws) %>%
    lapply(tolower) %>%
    as.data.frame(stringsAsFactors = FALSE, fileEncoding="latin1")

  return(df)
}
