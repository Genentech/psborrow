# Methods for calculating summary statistics for MCMC
# Last update on 120320

#' @import ggplot2
#' @import survival
`%notin%` <- Negate(`%in%`)

thm <-  theme(axis.title.x = element_blank(),
              legend.position = "none",
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_line(size=.1, color="grey"),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              text = element_text(size=15))


#' Generate summary statistics for the MCMC chains
#'
#' @param samples an object of class \code{mcmc.list}
#' @return a vector containing the mean, median, sd, reject rate for the MCMC chains
#'
#' @keywords method
rej_est = function(samples){
  summ = summary(samples)[[1]]
  quantiles <- summary(samples)[[2]]
  reject = quantiles["HR_trt_cc","97.5%"] < 1
  mean_HR  =  summ["HR_trt_cc","Mean"]
  sd_HR = summ["HR_trt_cc","SD"]
  # post_median_hr_trt_cc = quantiles["HR_trt_cc","50%"]
  mean_driftHR  =  summ["HR_cc_hc", "Mean"]
  sd_driftHR = summ["HR_cc_hc","SD"]
  print(paste("reject", reject,
              "mean_HR", mean_HR, "sd_HR", sd_HR,
              "mean_driftHR", mean_driftHR,  "sd_driftHR", sd_driftHR))
  c("reject" = reject,
    "mean_HR" = mean_HR, "sd_HR" = sd_HR,
    "mean_driftHR"  = mean_driftHR,  "sd_driftHR"  = sd_driftHR)
}


#' Generate summary statistics of a simulation scenario
#'
#' @param dt a \code{data.frame} containing summary statistics for the posterior samples from each simulation
#' @return a \code{data.frame} containing the mean and sd of posterior HR between treatment and control arm, the posterior mean and sd of HR between internal control and external control arm, reject rate, variance, bias and mse of the simulation set
#'
#' @export
#' @keywords method
get_summary = function(dt){
  dt %>% group_by(HR, driftHR, prior, pred) %>%
    summarize(nsim = n(),
              reject = mean(reject),
              mean_HR_trt_cc = mean(mean_HR),
              sd_HR_trt_cc = mean(sd_HR),
              bias =  mean_HR_trt_cc - mean(HR),
              var = (1/(nsim - 1))*sum((mean_HR - mean_HR_trt_cc)^2),
              sd = sqrt(var),
              mse = var + bias^2,
              mean_HR_cc_hc = mean(mean_driftHR),
              sd_HR_cc_hc = mean(sd_driftHR))
}

#' Plot type 1 error
#'
#' @param dt a \code{data.frame} containing summary statistics for the posterior samples from each simulation generated with \code{\link{get_summary}}
#' @param driftHR pre-specified HR between external control arm and internal control arm
#' @param pred predictors to use when fitting exponential distribution in MCMC
#'
#' @return a \code{ggplot} which is a bar plot containing type 1 error implications corresponding to each prior, the pre-specified HR between internal treatment and control arms is 1
#'
#' @export
#' @keywords method
plot_type1error <- function(dt, driftHR = 1, pred = "none"){
  dt$prior <- factor(dt$prior, levels = c("no_ext", "cauchy", "gamma", "full_ext"))

  dt2 = dt[dt$HR == 1 & dt$driftHR == driftHR & dt$pred == pred & dt$prior != "no_ext",]
  ref = dt[dt$HR == 1 & dt$driftHR == driftHR & dt$pred == pred & dt$prior == "no_ext",
           "reject", drop = TRUE]

  p1 <- ggplot(dt2, aes(y = reject, x = prior, fill = prior)) +
    geom_bar(position = position_dodge2(width = 1.2, preserve = "single"), stat="identity") +
    labs(title = "Summarizing posterior distributions: Type 1 Error",
         subtitle = paste0("pre-specified HR = 1, driftHR =", driftHR)) +
    ylab("Type 1 error") +
    geom_text(aes(y = reject, label = round(reject, 4)), vjust = -0.8) +
    scale_x_discrete(breaks=c("no_ext", "cauchy", "gamma", "full_ext"),
                     labels=c("No borrow", "Half-Cauchy", "Gamma", "Full borrow")) +

    scale_linetype_manual(name = "No Borrowing", values = "solid", labels = "") +
    scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + thm

  if(length(ref) == 0){
    p1
  } else {
    p1 + geom_hline(aes(yintercept = ref, linetype=factor(ref)), colour = "purple")
  }
}

#' Plot power
#'
#' @param dt a \code{data.frame} containing summary statistics for the posterior samples from each simulation generated with \code{\link{get_summary}}
#' @param HR pre-specified HR between treatment and control arm in the internal trial
#' @param driftHR pre-specified HR between external control arm and internal control arm
#' @param pred predictors to use when fitting exponential distribution in MCMC
#' @return a \code{ggplot} which is a bar plot containing power implications corresponding to each prior
#'
#' @export
#' @keywords method
plot_power <- function(dt, HR = 0.67, driftHR = 1, pred = "none") {
  dt$prior <- factor(dt$prior, levels = c("no_ext", "cauchy", "gamma", "full_ext"))
  dt2 = dt[dt$HR == HR & dt$driftHR == driftHR & dt$pred == pred & dt$prior != "no_ext",]
  ref = dt[dt$HR == HR & dt$driftHR == driftHR & dt$pred == pred & dt$prior == "no_ext",
           "reject", drop = TRUE]

  p1 <- ggplot(dt2, aes(y = reject, x = prior, fill = prior)) +
    geom_bar(position = position_dodge2(width = 1.2, preserve = "single"), stat="identity") +
    labs(title = "Summarizing posterior distributions: Power",
         subtitle = paste0("HR = ", HR, ", driftHR = ", driftHR)) +
    ylab("Power") +
    geom_text(aes(y = reject, label = round(reject, 4)), vjust = -0.8) +
    scale_x_discrete(breaks=c("cauchy", "gamma", "full_ext"),
                     labels=c("Half-Cauchy", "Gamma", "Full borrow")) +
    scale_linetype_manual(name = "No Borrowing", values = "solid", labels = "") +
    scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + thm

  if(length(ref) == 0){
    p1
  } else {
    p1 + geom_hline(aes(yintercept= ref, linetype= factor(ref)), colour = "purple")
  }
}

#' Plot posterior hazard ratio between treatment and control
#'
#' @param dt a \code{data.frame} containing summary statistics for the posterior samples from each simulation generated with \code{\link{get_summary}}
#' @param HR pre-specified HR between treatment and control arm in the internal trial
#' @param driftHR pre-specified HR between external control arm and internal control arm
#' @param pred predictors to use when fitting exponential distribution in MCMC
#' @return a \code{ggplot} which is a point plot containing posterior hazard ratio between treatment and control arms corresponding to each prior
#' @export
#' @keywords method
plot_hr <- function(dt, HR = 0.67, driftHR = 1, pred = "none") {
  dt$prior <- factor(dt$prior, levels = c("no_ext", "cauchy", "gamma", "full_ext"))
  dt2 = dt[dt$HR == HR & dt$driftHR == driftHR & dt$pred == pred & dt$prior != "no_ext",]
  ref = dt[dt$HR == HR & dt$driftHR == driftHR & dt$pred == pred & dt$prior == "no_ext", "mean_HR_trt_cc", drop = TRUE]

  p1 <- ggplot(dt2, aes(y = mean_HR_trt_cc, x = prior, fill = prior)) +
    geom_point() +
    geom_errorbar(dt2, mapping=aes(x = prior,
                                   ymin = mean_HR_trt_cc - sd_HR_trt_cc,
                                   ymax = mean_HR_trt_cc + sd_HR_trt_cc), width=0.2, color="orange") +
    labs(title = "Summarizing posterior distributions: hazard ratio",
         subtitle = paste0("Pre-specified HR = ", HR, ", driftHR = ", driftHR)) +
    ylab("Posterior HR") +
    geom_text(aes(y = mean_HR_trt_cc, label = round(mean_HR_trt_cc, 4)), vjust = -0.8) +
    scale_x_discrete(breaks=c("cauchy", "gamma", "full_ext"),
                     labels=c("Half-Cauchy", "Gamma", "Full borrow")) +

    scale_linetype_manual(name = "No Borrowing", values = "solid", labels = "") +
    scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + thm

  if(length(ref) == 0){
    p1
  } else {
    p1 + geom_hline(aes(yintercept = ref, linetype=factor(ref)), colour = "purple")
  }
}


#' Plot bias
#'
#' @param dt a \code{data.frame} containing summary statistics for the posterior samples from each simulation generated with \code{\link{get_summary}}
#' @param driftHR pre-specified HR between external control arm and internal control arm
#' @param pred predictors to use when fitting exponential distribution in MCMC
#'
#' @return a \code{ggplot} which is a bar plot containing bias implications corresponding to each prior, the pre-specified HR between internal treatment and control arms is 1
#'
#' @export
#' @keywords method
plot_bias <- function(dt, HR = 1, driftHR = 1, pred = "none"){
  dt$prior <- factor(dt$prior, levels = c("no_ext", "cauchy", "gamma", "full_ext"))
  dt2 = dt[dt$HR == HR & dt$driftHR == driftHR & dt$pred == pred & dt$prior != "no_ext",]
  ref = dt[dt$HR == HR & dt$driftHR == driftHR & dt$pred == pred & dt$prior == "no_ext",
           "bias", drop = TRUE]

  p1 <- ggplot(dt2, aes(y = bias, x = prior, fill = prior)) +
    geom_bar(position = position_dodge2(width = 1.2, preserve = "single"), stat="identity") +
    labs(title = "Summarizing posterior distributions: Bias",
         subtitle = paste0("Pre-specified HR = ", HR, ", driftHR = ", driftHR)) +
    ylab("Bias") +
    geom_text(aes(y = bias, label = round(bias, 4)), vjust = -0.8) +
    scale_x_discrete(breaks=c("cauchy", "gamma", "full_ext"),
                     labels=c("Half-Cauchy", "Gamma", "Full borrow")) +

    scale_linetype_manual(name = "No Borrowing", values = "solid", labels = "") +
    scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + thm

  if(length(ref) == 0){
    p1
  } else {
    p1 + geom_hline(aes(yintercept = ref, linetype=factor(ref)), colour = "purple")
  }
}

#' Plot MSE
#'
#' @param dt a \code{data.frame} containing summary statistics for the posterior samples from each simulation generated with \code{\link{get_summary}}
#' @param driftHR pre-specified HR between external control arm and internal control arm
#' @param pred predictors to use when fitting exponential distribution in MCMC
#'
#' @return a \code{ggplot} which is a bar plot containing MSE implications corresponding to each prior, the pre-specified HR between internal treatment and control arms is 1
#'
#' @export
#' @keywords method
plot_mse <- function(dt, HR = 1, driftHR = 1, pred = "none"){
  dt$prior <- factor(dt$prior, levels = c("no_ext", "cauchy", "gamma", "full_ext"))
  dt2 = dt[dt$HR == HR & dt$driftHR == driftHR & dt$pred == pred & dt$prior != "no_ext",]
  ref = dt[dt$HR == HR & dt$driftHR == driftHR & dt$pred == pred & dt$prior == "no_ext",
           "mse", drop = TRUE]
  p1 <- ggplot(dt2, aes(y = mse, x = prior, fill = prior)) +
    geom_bar(position = position_dodge2(width = 1.2, preserve = "single"), stat="identity") +
    labs(title = "Summarizing posterior distributions: MSE",
         subtitle = paste0("Pre-specified HR = ", HR, ", driftHR = ", driftHR)) +
    ylab("MSE") +
    geom_text(aes(y = mse, label = round(mse, 4)), vjust = -0.8) +
    scale_x_discrete(breaks=c("cauchy", "gamma", "full_ext"),
                     labels=c("Half-Cauchy", "Gamma", "Full borrow")) +
    scale_linetype_manual(name = "No Borrowing", values = "solid", labels = "") +
    scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + thm

  if(length(ref) == 0){
    p1
  } else {
    p1 + geom_hline(aes(yintercept = ref, linetype=factor(ref)), colour = "purple")
  }
}
