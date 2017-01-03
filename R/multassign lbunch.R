#' %=%.lbunch 
#'
#' groups the left hand side for use with the %=% operator. 
#' inspired by http://stackoverflow.com/questions/7519790/assign-multiple-new-variables-on-lhs-in-a-single-line-in-r. 
#' @param l left hand list 
#' @param r right hand list  
#' @keywords assign 
#' @export

# Binary Operator
'%=%.lbunch' = function(l, r, ...) {
    Envir = as.environment(-1)
    
    if (length(r) > length(l))
        warning("Right side has more args than left side. Only first ", length(l), " arguments used.")
    
    if (length(l) > length(r))  {
        warning("Left side has more arguments than right side. Right side will be repeated.")
        r <- extendToMatch(r, l)
    }
    
    for (n in 1:length(l)) {
        do.call('<-', list(l[[n]], r[[n]]), envir=Envir)
    }
}