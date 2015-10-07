##' this function calculates the number of sample sizes needed for
##' generating a sample from a multinomial distribution with num_bins
##'
##' @param num_bins
##' @return num_samples
##' @export
##'

sim_calculate_nsamples <- function(num_bins=-1)
{
    num_samples <- -999

    if (num_bins > 0)
    {
        # AIEEEEEE!!! magic numbers selected from the aether
        num_samples <- 8 * num_bins
    }

    return(num_samples)
}
