% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R, R/CAB.ks.R
\name{ks}
\alias{ks}
\alias{ks}
\title{Kolmogorow-Smirnov statistic}
\usage{
ks(sample_1, sample_2)

CAB.ks( sample1, sample2 )
}
\arguments{
\item{sample1}{Numeric vector of samples}

\item{sample2}{Numeric vector of samples}
}
\description{
Calculates the two-sample Kolmogorov-Smirnov statistic
}
\details{
The KS statistic is computed the same way as it is in the \code{ks.test} function in the \code{stats} package. We just stripped away all the other stuff so that it is faster.
}
