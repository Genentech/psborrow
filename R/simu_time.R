#' Simulate time-to-events for multiple scenarios
#' 
#' @param dt a list of \code{matrix} generated in \code{\link{simu_cov}} containing simulated covariates information
#' @param eventObj an object of class \code{.eventClass} generated in \code{\link{set_event}} including event information
#' @param clinInt an object of class \code{.clinClass} generated in \code{\link{set_clin}} including internal trial information
#' @param clinExt an object of class \code{.clinClass} generated in \code{\link{set_clin}} including external trial information
#' @param seed the seed of random number generator. Default is 47
#' @param path file name for saving the output including folder path
#' @return a list of \code{matrix} containing simulated time-to-events information
#'
#' @example
#' # simulate patient-level data without covariates and simulate survival time
#' sample = set_n(ssC = 140, ssE = 275, ssExt = 100)
#' sample_cov <- simu_cov(ssObj = sample, HR = c(0.67, 1), driftHR=c(1,1.2), nsim = 100)

#' # Enrollment pattern, drop-out, analysis start time
#'c_int = set_clin(gamma = 10, e_itv = 415/10, etaC = -log(1-0.04/12),  CCOD = "fixed-first", CCOD_t = 64)
#'c_ext = set_clin(gamma = 100/18, e_itv = 18, etaC = -log(1-0.01/12),  CCOD = "fixed-first", CCOD_t = 64)
#'
#' # Time-to-event distribution
#' evt <- set_event(event = "weibull", shape = 0.9, lambdaC = 0.0135)
#' simu_time(dt = sample_cov, eventObj = evt, clinInt = c_int, clinExt = c_ext)
#' 
#' 
#' @export
#' @keywords simulator
simu_time = function(dt, eventObj, clinInt, clinExt, seed, path){
  
  if (missing(dt)) stop("Please provide a list of simulated data (dt).")
  if (missing(eventObj)) stop("Please provide eventObj.")
  if (missing(clinInt)) stop("Please provide clinInt.")
  if (missing(clinExt)) stop("Please provide clinExt.")
  if (missing(seed)){
    message("Set.seed(47)")
    seed = 47
  }
  
  seed_list <- seq(seed, seed + length(dt), by = 1)
  
  res_list <- lapply(seq(1, length(dt), by = 1), function(i){
    seed_i = seed_list[i]
    print(paste("seed =", seed_i))
    add_time(dt = dt[[i]], eventObj = eventObj, clinInt = clinInt, clinExt = clinExt, seed = seed_i)
  }) #loop foreach
  
  if (missing(path)) print("Simulated time are not saved.") else {
    save(res_list, file = path)
    print(paste("Simulated time are saved as", path))
  }
  res_list
}

