#' Extract the BLUP
#'
#' @description Extract the BLUP of all model
#'
#' @param object a listofMermod object.
#' @return a list containing the the BLUP for all models.
#' @importFrom lme4 ranef
#' @export
ranef.listofMermod <- function(object){
  lapply(object, ranef)
}
