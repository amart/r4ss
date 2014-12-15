##' this function generates a length composition observation for the fleet in the data structure for the SS DAT file
##'
##' @param dat_struct - DAT structure
##' @param rep_struct - structure returned by SS_output()
##' @param gen_year - year for the generated composition observation
##' @param gen_seas - season number
##' @param gen_fleet - fleet number
##' @param apply_error - apply error to the generated observation
##' @return generated length composition matrix with sample size
##' @export
##'

sim_generate_length_comp <- function(dat_struct=NULL,rep_struct=NULL,gen_year=-1,gen_seas=-1,gen_fleet=-1,apply_error=FALSE)
{
    new_len_comp <- NULL

    if (!is.null(dat_struct) && !is.null(rep_struct) && gen_year > 0 && gen_seas > 0 && gen_fleet > 0)
    {
        ngend <- dat_struct$Ngenders

        nlens <- dat_struct$N_lbinspop

        # get numbers-at-length, in thousands
        true_natlen <- sim_get_N_at_length(dat_struct,rep_struct,gen_year,gen_seas,gen_fleet)

        # get the most recent selectivity-at-length for this fleet
        true_lenselex <- sim_get_length_selex(dat_struct,rep_struct,gen_fleet)

        # check that the number of genders matches the number of rows
        if (dim(true_natlen)[1] == ngend && dim(true_lenselex)[1] == ngend)
        {
            # the numbers-at-len values only
            natlen   <- true_natlen

            # the selectivity-at-len values only
            lenselex <- true_lenselex

            # here they are, not normalized
            true_len_comp <- natlen * lenselex

            if (apply_error)
            {
                new_len_comp <- data.frame(matrix(0,nrow=dim(true_len_comp)[1],ncol=dim(true_len_comp)[2]))
                names(new_len_comp) <- names(true_len_comp)

                # AIEEEEEE!!! magic numbers selected from the aether
                nsamples <- floor(max(8,nlens)^1.65)

                for (i in 1:ngend)
                {
                    # generate a multinomial sample
                    sum_len_comp <- sum(true_len_comp[i,])
                    new_len_comp[i,] <- sum_len_comp * (rmultinom(1,nsamples,(true_len_comp[i,] / sum_len_comp)) / nsamples)
                }
            } else {
                new_len_comp <- true_len_comp
            }
        }
    }

    return(new_len_comp)
}
