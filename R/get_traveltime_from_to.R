#' Get traveltime from source GIDs to target Gids
#' @description sdf
#' @param GIDs_from  numeric - a vector of GIDs
#' @param GIDs_to  numeric - a vector of GIDs
#' @return Returns
#' @author Andreas Schönberg
#' @export get_traveltime_from_to
#' @aliases get_traveltime_from_to
#' @examples
#' # load data
#' data("valid_GIDs")
#' gids_from <-sample(valid_GIDs,20)
#' gids_to <-sample(valid_GIDs,5)
#'
#' res <-get_traveltime_from_to(GIDs_from = gids_from,GIDs_to = gids_to)
#' res


get_traveltime_from_to = function(GIDs_from, GIDs_to) {
  ### check GIDs
  # load list of valid GIDs
  data("valid_GIDs")
  # check
  if(any(GIDs_from%in%valid_GIDs==F)){
    stop("Invalid GIDs detected!")}
  if(any(GIDs_to%in%valid_GIDs==F)){
    stop("Invalid GIDs detected!")}

  BASE = "https://distmat.dsa.info"
  prefix = paste0(BASE, "/", "distance_from_to?src=")
  src_string = paste0(GIDs_from, collapse = "&src=")
  dst_string = paste0(GIDs_to, collapse = "&dest=")

  query = paste0(prefix, src_string, "&dest=", dst_string)
  query = if (R.version$major < 4) base::url(query) else
    base::url(query, headers = c(Accept = "application/json, text/*, */*"))

  as_list = fromJSON(query)

  as_df = data.frame(as_list$data)
  colnames(as_df) = as_list$columns
  rownames(as_df) = as_list$index
  return(as_df)
}
