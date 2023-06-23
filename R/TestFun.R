#' TestFun
#' @description test the package
#' @param x  numeric - a test value
#' @return if all wokrs the function returns a message
#' @author Andreas Schönberg
#' @export TestFun
#' @aliases TestFun
#' @examples
#' TestFun(2019)

TestFun <- function(x){
  cat("Start testing",sep="\n")
  Sys.sleep(1)
  cat("...",sep="\n")
  Sys.sleep(1)
  cat(paste0("Testing input variable '",x,"' "),sep="\n")
  Sys.sleep(1)
  cat("...",sep="\n")
  Sys.sleep(1)
  cat("done",sep="\n")
  }# end of function
