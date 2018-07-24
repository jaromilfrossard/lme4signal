#' Linear Mixed Models for signal
#'
#' @description This function return a list mixed model fitted using the same design on each column of a matrix response variable. Theta parameters are assume to be smooth.
#'
#' @param formula a \code{lme4} formula where the response is a matrix.
#' @param data a data frame. See \code{\link{lmer}} for more details.
#' @param REML a logical that indicate which criterion to optimize. See \code{\link{lmer}} for more details.
#' @param control Some parameters. See \link{lmerControl} or \code{\link{lmer}} for more details.
#' @param start starting values for the parameters.
#' @param verbose See \code{\link{lmer}} for more details.
#' @param subset an expression to selecte a subset of the data. See \code{\link{lmer}} for more details.
#' @param weights an optional vector of weights. See \code{\link{lmer}} for more details.
#' @param na.action a function that handle \code{NA}'s. See \code{\link{lmer}} for more details.
#' @param offset specify a priori component in the predictor. See \code{\link{lmer}} for more details.
#' @param contrasts a list of coFntrasts. See \code{\link{lmer}} for more details.
#' @param devFunOnly a logical set by default to \code{FALSE}. See \code{\link{lmer}} for more details.
#' @param ... addition arguments. See \code{\link{lmer}} for more details.
#' @return A listofmerMod object which is a list of merMod object.
#' @seealso \code{\link{lmer}}.
#' @importFrom lme4 lmerControl glmerControl mkLmerDevfun optimizeLmer checkConv mkMerMod nobars
#' @importFrom stats df model.frame model.response update.formula
#' @export
lmersignal <- function (formula, data = NULL, REML = TRUE, control = lmerControl(),
          start = NULL, verbose = 0L, subset, weights, na.action, offset,
          contrasts = NULL, devFunOnly = FALSE, ...) {
  mc <- mcout <- match.call()
  missCtrl <- missing(control)
  if (!missCtrl && !inherits(control, "lmerControl")) {
    if (!is.list(control))
      stop("'control' is not a list; use lmerControl()")
    warning("passing control as list is deprecated: please use lmerControl() instead",
            immediate. = TRUE)
    control <- do.call(lmerControl, control)
  }
  if (!is.null(list(...)[["family"]])) {
    warning("calling lmer with 'family' is deprecated; please use glmer() instead")
    mc[[1]] <- quote(lme4::glmer)
    if (missCtrl)
      mc$control <- glmerControl()
    return(eval(mc, parent.frame(1L)))
  }
  fr = model.frame(nobars(formula),df)
  signal = model.response(fr)

  #signal <- model.response(lmod$fr)

  mc$control <- control
  mc[[1]] <- quote(lme4::lmer)
  mc$formula = update.formula(old= formula(mc),new = eval(parse(text=paste(paste(nobars(formula)[[2]]),"[,1]~.",sep=""))))
  print(dim(signal))
  output=list()
  output[[1]] <- eval(mc)
  for(i in 2:ncol(signal)){
    newy = paste(paste(nobars(formula)[[2]]),"[,",eval(parse(text="i")),"]~.",sep="")
    mc$formula = update.formula(old= formula(mc),new = eval(parse(text=newy)))
    mc$start= getME(output[[i-1]],"theta")
    output[[i]] = eval(mc)
    print(i)
  }
  class(output) <- "listofmerMod"
  return(output)
}
