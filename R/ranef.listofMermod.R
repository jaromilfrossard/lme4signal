#' Extract the BLUP
#'
#' @description Extract the BLUP of all model
#'
#' @param object a listofmerMod object.
#' @return a list containing the the BLUP for all models.
#' @importFrom lme4 ranef
#' @export
ranef.listofmerMod <- function(object){
  lapply(object, ranef)
}
