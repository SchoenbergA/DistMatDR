#' Get DistMat
#' @description Check if input GIDs are valid
#' @param GIDs  numeric - a vector of GIDs
#' @return Returns
#' @author Andreas Schönberg
#' @export get_DistMat
#' @aliases get_DistMat
#' @examples
#' # load data
#' data("valid_GIDs")
#'
#' # get vector of random GIDs
#' gids <- sample(valid_GIDs,102)
#' res <- get_DistMat(gids)


get_DistMat <- function(GIDs){
  # define path
  BASE = "https://distmat.dsa.info"

  SIZE = 99
  n = length(GIDs)
  if( n <= 100 ) return(get_traveltime(GIDs))
  m = ceiling(n/SIZE)-1

  acc_rows = data.frame()
  for( i in 0:m ){
    s_from = i*SIZE+1
    e_from = min((i+1)*SIZE, n)
    acc_cols = get_traveltime_from_to(GIDs[s_from:e_from], GIDs[1:SIZE])
    for( j in 1:m ){
      s_to = j*SIZE+1
      e_to = min((j+1)*SIZE, n)
      ij = get_traveltime_from_to(GIDs[s_from:e_from], GIDs[s_to:e_to])
      acc_cols = cbind(acc_cols, ij)
    }
    acc_rows = rbind(acc_rows, acc_cols)
  }
  return(acc_rows)
}
