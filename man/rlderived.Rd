% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{rlderived}
\alias{rlderived}
\title{Log-Distribution derived by comparable circumstances}
\usage{
rlderived(n, min, max)
}
\arguments{
\item{n}{Number of samples to be drawn}

\item{min}{Minimum value of normal comparables}

\item{max}{Maximum value of normal comparables}
}
\value{
numeric vector of length n with randomly drawn values according to
the distribution
}
\description{
This function draws random values from a uniform distribution (minimum to
maximum) combined with the upper and lower tail of a log normal
distribution. Minimum and maximum values are transformed to a log10 scale
first.
}
\details{
95\%, 2.5\% and 2.5\% of all drawn values originate from the log-uniform
distribution, the lower tail, and the upper tail of a log-normal distribution,
respectively. The log-normal distribution is characterized by the mean of
log-minimum and log-maximum. The standard deviation is derived from the 2.5\% and
97.5\% quantile defined by the minimum and maximum values.
sd = (log10(max) - log10(mean)) / 1.959963

The number of Fields is multiplied by 38 for the uniform distribution, while
it is multiplied by 1 for the tails of the log-normal distribution
(38 / 40 = 95\%)
Finally, the defined number of fields is randomly drawn from the vector.
}
