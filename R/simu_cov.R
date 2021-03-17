# S4 class, constructors and methods for running simulations for multiple scenarios
# Last update on 082320

`%notin%` <- Negate(`%in%`)

#' Simulate covariates
#'
#' This function generates continuous and binary covariates through simulating from a multivariate normal distribution. Outcomes are further converted to binary variables using quantiles of the normal distribution calculated from the probability provided. Then the covariates are added to the external trial and treatment arm indicators.
#'
#' @param ssObj an object of class \code{.covClass} generated in \code{\link{set_n}}
#' @param covObj an object of class \code{.covClass} generated in \code{\link{set_cov}}
#' @param driftHR hazard ratio of external control and internal control arms
#' @param HR a list of hazard ratio of treatment and control arms
#' @param nsim number of simulation. Default is 5
#' @param seed the seed of R‘s random number generator. Default is 47
#' @param path file name for saving the output including folder path
#' @return a list of \code{matrix} containing simulated covariates information
#'
#' @export
#' @keywords simulator
setGeneric(name="simu_cov", def=function(ssObj, covObj, driftHR, HR, nsim, seed, path){standardGeneric("simu_cov")})
setMethod(f="simu_cov", signature(ssObj = "matrix"),
          definition=function(ssObj, covObj, driftHR, HR, nsim, seed, path){

            if (missing(ssObj)) stop("Please provide ssObj.")
            if (missing(covObj)) {
              message("No covObj is provided.")
              covObj = NULL
            }
            if (missing(HR)){
              HR = 1
              message("HR values (HR) not provided. Default value 1 is used.")
            }
            if (missing(driftHR)){
              driftHR = 1
              message("driftHR values (driftHR) not provided. Default value 1 is used.")
            }
            if (missing(nsim)) {
              message("Number of simulation is not provided. Default value 5 is used")
              nsim = 5
            }

            dt0 <- ssObj # before the sample size may change

            ssC <- sum(dt0[,'ext'] ==0 & dt0[,'trt'] ==0)
            ssE <- sum(dt0[,'ext'] ==0 & dt0[,'trt'] ==1)
            ssExt <- sum(dt0[,'ext'] ==1)

            message(paste0("The sample size for internal control, internal treatment, and external control arms are ",
                           ssC, ", ", ssE, ", and ", ssExt,", respectively."))

            if (missing(seed)){
              message("Set.seed(47)")
              seed = 47
            }

            seed_list <- array(seq(seed, length(driftHR) * length(HR) * nsim + seed, by = 1),
                               dim = c(nsim, length(HR), length(driftHR)))
            flog.debug(cat("[simu_cov] seed_list:", seed_list, "\n"))

            res_list <- sapply(1:length(driftHR), function(k){
              dr <- driftHR[k]

              sapply(1:length(HR), function(j) {
                hr <- HR[j]

                nsim_res <-  lapply(seq(1, nsim, by = 1), function(i){
                  seed_i = seed_list[i, j, k]

                  print(paste("-------------------", i, "of ", nsim, "simulation:",
                              j, "of HR =", hr, ",",
                              k, "of driftHR =", dr, "seed =", seed_i))

                  samp = set_n(ssC = ssC, ssE = ssE, ssExt = ssExt)
                  samp_cov = samp

                  if (!is.null(covObj)) {
                    samp_cov = add_cov(dt = samp, covObj = covObj, seed = seed_i)
                  }

                  flog.debug(cat("[simu_cov] seed_i:", seed_i, "\n"))
                  cbind("driftHR" = dr, "HR" = hr, samp_cov)
                })
              })
            })


            if (missing(path)) print("Simulated covariates are not saved.") else {
              save(res_list, file = path)
              print(paste("Simulated covariates are saved as", path))
            }
            res_list
          })

