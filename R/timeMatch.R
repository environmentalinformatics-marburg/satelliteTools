#' Match time stamps of two vectors
#'
#' @description
#' Match time stamps of two vectors in order to select the closest time in 
#' a second vector based on the time information in the first vector.
#'
#' @param a Vector with time information
#' @param b Vector with time information
#'
#' @return
#' A data frame with corresponding elements.
#'
#' @author
#' Thomas Nauss
#'
#' @references
#' NONE
#'
#' @examples
#' \notrun{
#' }
#'
#' @export timeMatch
#' @name timeMatch
#'
timeMatch <- function(a, b){
  a[1:10]
  b[1:10]
  
  time_match <- lapply(a, function(ai){
    dt <- ai - b
    id <- which(abs(dt) == min(abs(dt)))
    data.frame(a = ai,
               b = b[id],
               diff = dt[id],
               id = id)
  })
  time_match <- do.call("rbind", time_match)
  return(time_match)
}