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
#' @export CAB.ks

CAB.ks = function( sample1, sample2 ){
    len_sample_1 = length(sample1)
    len_sample_2 = length(sample2)
    ecdf_len = len_sample_1 * len_sample_2 / ( len_sample_1 + len_sample_2 )
    all_samples = c( sample1, sample2 )
    ordering = order( all_samples ) <= len_sample_1
    differences = cumsum( ordering * 1/len_sample_1 + ( 1 - ordering ) * -1/len_sample_2 )
    max(abs(differences))
}
