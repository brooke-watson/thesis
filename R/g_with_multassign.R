#' g
#'
#' groups a list for use with the %=% operator. 
#' inspired by http://stackoverflow.com/questions/7519790/assign-multiple-new-variables-on-lhs-in-a-single-line-in-r. 
#' @param ... list  
#' @seealso %=% 
#' @keywords assign  

g = function(...) {
    List = as.list(substitute(list(...)))[-1L]
    class(List) = 'lbunch'
    return(List)
}