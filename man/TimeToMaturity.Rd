% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TimeToMaturity.R
\name{TimeToMaturity}
\alias{TimeToMaturity}
\alias{time_to_maturity}
\title{Time to Crop Maturation}
\usage{
time_to_maturity(
  growth_probability,
  stages = 1,
  p = 0.5,
  method = c("median", "mean", "quantile"),
  unit = "minute"
)
}
\arguments{
\item{growth_probability}{\code{numeric(1)} on the interval (0, 1). The
probability that a crop will advance a growth stage when assigned a
random tick.}

\item{stages}{\code{integerish(1)}. The number of growth stages a crop requires
before becoming mature.}

\item{p}{\code{numeric(1)} on the interval (0, 1). The probability used for
determining the quartile of the probability distribution. Used only
when \code{unit = "minute"}.}

\item{method}{\code{character(1)}. One of \code{c("median", "mean", "quantile")}.
The method of calculation to be based on either the median, mean, or
quantile of the probability distribution.}

\item{unit}{\code{character(1)}. One of \code{c("tick", "second", "minute", "hour")}.
The unit of time for the metric.}
}
\description{
Time to Maturity (TTM) is the duration of time between when
it is planted and when it reaches a mature state and is ready to harvest.
Crops may encounter several stages of growth before maturity–such as wheat,
which encounters eight growth stages.
}
