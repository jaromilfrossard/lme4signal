#' Anova method for listofmerMod object
#'
#' @description Apply the anova function from lmerTest at each model of the list.
#' @param object a listofmerMod object.
#' @param type numerical indicating the type of SS. Default is 3.
#' @param ddf character indicating the computation of the degree of freedom. Default is "Satterthwaite".
#' @param ... orther arguments.
#' @details see anova from lmerTest for more informations.
#' @export
anova.listofmerMod <- function(object, ..., type = 3, ddf = "Satterthwaite"){
  dotargs=list(...)
  lapply(object,function(obji)lmerTest:::anova.lmerModLmerTest(obji,type = type,ddf = ddf,dotargs))}
