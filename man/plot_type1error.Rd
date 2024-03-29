% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_summary.R
\name{plot_type1error}
\alias{plot_type1error}
\title{Plot type 1 error}
\usage{
plot_type1error(dt, driftHR = 1, pred = "none")
}
\arguments{
\item{dt}{a \code{data.frame} containing summary statistics for the posterior samples from each simulation generated with \code{get_summary()}. Must contain simulations for HR = 1.0.}

\item{driftHR}{the driftHR between the external and internal control arms for which the type 1 error should be plotted. Must be within \code{unique(dt$driftHR)}.}

\item{pred}{the predictors used when fitting the exponential distribution in MCMC for which the type 1 error should be plotted. Must be within \code{unique(dt$pred)}.}
}
\value{
a bar plot of class \code{ggplot} containing type 1 error proportions for each prior distribution.
}
\description{
Plot type 1 error for each prior distribution according to selected simulation parameters
}
\keyword{method}
