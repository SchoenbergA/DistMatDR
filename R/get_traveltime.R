#' get_traveltime
#' @description Extract the traveltime distance matirx for a given set of GIDs
#' @param GIDs  numeric - an integer of GIDs
#' @return Returns the Distacne Matrix (traveltime on foot) for the given GIDs for both directions.
#' @author Andreas Schönberg
#' @export get_traveltime
#' @aliases get_traveltime
#' @examples
#'
#' # load data
#' data("valid_GIDs")
#'
#' # get vector of random GIDs
#' gid <- sample(valid_GIDs,100)
#' # get traveltime distanc matrix
#' df<-get_traveltime(gid)

get_traveltime = function(GIDs){

  # load list of valid GIDs
  data("valid_GIDs")

  # check input
  if(any(GIDs%in%valid_GIDs==F)){
    stop("Invalid GIDs detected!")}

  # define path
  IP = "distmat.dsa.info"  #"localhost"
  BASE = paste0("https://", IP)

  # compute query for all GIDs
  prefix = paste0(BASE, "/", "distance?gids=")
  suffix = paste0(GIDs, collapse = "&gids=")

  query = paste0(prefix, suffix)

  # request query
  as_list = fromJSON(query)

  # convert to df
  as_df = data.frame(as_list$data)
  colnames(as_df) = as_list$columns
  rownames(as_df) = as_list$index

  # return
  return(as_df)
}

