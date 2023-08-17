#' Get knn and traveltime for GID
#' @description sdf
#' @param GID  numeric - a vector of GIDs
#' @param GIDs_to  numeric - a vector of GIDs
#' @return Returns
#' @author Andreas Schönberg
#' @export get_knn
#' @aliases get_knn
#' @examples
#' # get the 5 nearest neighbours for GID 112019
#' res <-get_knn(112019,5)
#' res


get_knn = function(GID, k){

  ### check GIDs
  # load list of valid GIDs
  data("valid_GIDs")
  # check
  if(any(GID%in%valid_GIDs==F)){
    stop("Invalid GID detected!")}

  BASE = "https://distmat.dsa.info"
  query = paste0(BASE, "/", "knn?gid=", GID, "&k=", k)
  as_list = fromJSON(query)
  as_df = data.frame(knn = as_list$knn, dist = as_list$dist)
  return(as_df)
}
