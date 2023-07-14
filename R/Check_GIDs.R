#' Check GIDs
#' @description Check if input GIDs are valid
#' @param GIDs  numeric - a vector of GIDs
#' @param crop boolean - If TRUE invalid GIDs are cropped
#' @return Returns a message if all GIDs are valid or which are invalid. If 'crop=TRUE' returns a vector of all valid input GIDs
#' @author Andreas Schönberg
#' @export Check_GIDs
#' @aliases Check_GIDs
#' @examples
#' # get vector of GIDs
#' gids <- c(522647, 522710)
#' # check
#' Check_GIDs(gids)
#'
#' # get vector of GIDs with invalid GID
#' gids2 <- c(522647, 522710,1)
#' # check
#' Check_GIDs(gids2)
#' # crop invalid GIDS
#' gids_new <- Check_GIDs(gids2,crop=T)
#' # check
#' Check_GIDs(gids_new)
#' gids_new


Check_GIDs <- function(GIDs,crop=F){

  # load list of valid GIDs
  data("valid_GIDs")

  # check
  if(any(GIDs%in%valid_GIDs==F)){
    cat("Invalid GIDs detected!",sep="\n")
    print(GIDs[which(GIDs%in%valid_GIDs==F)])

    if(crop==T){
      cat("Cropping invalid GIDs",sep="\n")
      GIDs_c <- GIDs[-which(GIDs%in%valid_GIDs==F)]
      return(GIDs_c)
    }
  } else {
    cat("No invalid GIDs detected!",sep="\n")
  }

} # end of function


