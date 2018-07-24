#' Extract component from list of Mermod
#'
#' @description Method to extract component of all the the models produce by lmersignal
#'
#' @param object a listofMermod object.
#' @param name the name of the component.
#' @param ... additional argument like SIMPLIFY
#' @details see \link{getME} for more info about the component available.
#' @export
#' @importFrom lme4 getME
getME.listofmerMod = function(object, name, ...){
  dotargs = list(...)
  if(is.null(dotargs$SIMPLIFY)){dotargs$SIMPLIFY = F}
  if(dotargs$SIMPLIFY==T){
  sapply(object,function(o){lme4::getME(o,name)})}
  else{
    lapply(object,function(o){lme4::getME(o,name)})}
}
