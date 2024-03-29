% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fcr_PEC.R
\name{get_PEC_human}
\alias{get_PEC_human}
\title{Predicted human consumption}
\usage{
get_PEC_human(p, d, food_only)
}
\arguments{
\item{p}{Parameter table created with \code{\link[=oneYear_matrix]{oneYear_matrix()}} and extended with
\code{\link[=add_variables]{add_variables()}}}

\item{d}{The number of days of the growing period.}

\item{food_only}{If TRUE, the predicted human consumption via food is
multiplied by 2 to compensate the disregard of water consumption}
}
\value{
Numeric vector of human consumption via crops in µg/d
}
\description{
Predicted human consumption via crop consumption. The calculation is based
on the average predicted soil concentration within the growing period.
}
