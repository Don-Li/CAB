#### Kolmogorov-Smirnov statistic

#' Kolmogorow-Smirnov statistic
#'
#' Calculates the two-sample Kolmogorov-Smirnov statistic
#'
#' @param sample1 Numeric vector of samples
#' @param sample2 Numeric vector of samples
#'
#' @usage CAB.ks( sample1, sample2 )
#'
#' @details
#' The KS statistic is computed the same way as it is in the \code{ks.test} function in the \code{stats} package. We just stripped away all the other stuff so that it is faster.
#'
#' @name ks
NULL
