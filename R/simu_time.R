
#' Simulate time-to-events for multiple scenarios
#' @name simu_time
#' @aliases simu_time,matrix,.eventClass-method
#' 
#' @param dt a list of \code{matrix} generated in \code{\link{simu_cov}} containing simulated covariates information
#' @param eventObj an object of class \code{.eventClass} generated in \code{\link{set_event}} including event information
#' @param clinInt an object of class \code{.clinClass} generated in \code{\link{set_clin}} including internal trial information
#' @param clinExt an object of class \code{.clinClass} generated in \code{\link{set_clin}} including external trial information
#' @param seed the seed of random number generator. Default is 47
#' @param path file name for saving the output including folder path
#' @return a list of \code{matrix} containing simulated time-to-events information
#'
#' @export
#' @keywords simulator
setGeneric(name="simu_time", def=function(dt, eventObj, clinInt, clinExt, seed, path){standardGeneric("simu_time")})
setMethod(f="simu_time", signature(dt = "matrix", eventObj = ".eventClass"),
          definition=function(dt, eventObj, clinInt, clinExt, seed, path){

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
          })

