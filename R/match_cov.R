#' Match
#' 
#' @param dt  a list of \code{matrix}
#' @param match A vector of covariates name to match on
#' @example
#' # match on internal and external trial data using covariates 1 and 2
#' match_cov(sample_cov, c("cov1", "cov2"))
#' 
#' @export
#' @keywords constructor
match_cov <- function(dt, match) {
  if (missing(dt)) stop("Please provide a list of simulated data (dt).")
  if (missing(match)) {
    stop("Please provide covariates name to match on.")
  } else if (sum(match %notin% colnames(dt[[1]])[colnames(dt[[1]]) %notin% c("driftHR", "HR", "trt", "ext")]) > 0) {
    stop("Please make sure the trial data contains the correct variable names.")
  }
  match.formula <- paste("int ~", paste0(match, collapse=' + '))
  print(match.formula)
  res_list <- lapply(seq(1, length(dt), by = 1), function(i){
    m_cov(dt[[i]], match.formula)
  }) #loop foreach
  
  res_list
}

