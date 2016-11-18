#' %=% 
#'
#' This function encodes a season variable based on dates of data collection in the study.
#' inspired by http://stackoverflow.com/questions/7519790/assign-multiple-new-variables-on-lhs-in-a-single-line-in-r. 
#' @param l left hand list 
#' @param r right hand list 

'%=%' = function(l, r, ...)
    UseMethod('%=%')



