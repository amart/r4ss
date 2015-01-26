##' this function generates a length composition observation given the expected values for the fleet in the data structure for the SS DAT file
##'
##' @param exp_dat_struct - DAT structure with the expected values
##' @param exp_rep_struct - structure returned by SS_output()
##' @param gen_year - year for the generated composition observation
##' @param gen_seas - season number for the generated composition observation
##' @param gen_fleet - fleet number for the generated composition observation
##' @param apply_error - apply error to the generated composition observation
##' @return generated length composition matrix with sample size
##' @export
##'

sim_generate_length_comp_from_expected <- function(exp_dat_struct=NULL,exp_rep_struct=NULL,gen_year=-1,gen_seas=-1,gen_fleet=-1,apply_error=FALSE)
{
    new_len_comp <- NULL

    if (!is.null(exp_dat_struct) && !is.null(exp_rep_struct) && gen_year > 0 && gen_seas > 0 && gen_fleet > 0)
    {
        # get expected length composition
        exp_lencomp <- subset(subset(subset(exp_dat_struct$lencomp,exp_dat_struct$lencomp$Yr == gen_year),Seas == gen_seas),FltSvy == gen_fleet)

        # check that one row is returned
        if (dim(exp_lencomp)[1] == 1)
        {
            ngend <- exp_dat_struct$Ngenders

            nbins <- exp_dat_struct$N_lbins

            # map the whole comp vector into ngend vectors
            true_len_comp <- matrix(0,nrow=ngend,ncol=nbins,dimnames=list(c(),exp_dat_struct$lbin_vector))
            len_comp_vec <- length(exp_lencomp)
            split_bin <- len_comp_vec - nbins
            if (ngend == 2)
            {
                true_len_comp[1,] <- t(exp_lencomp[(split_bin - nbins + 1):split_bin])
                true_len_comp[2,] <- t(exp_lencomp[(split_bin + 1):len_comp_vec])
            } else {
                true_len_comp[1,] <- t(exp_lencomp[(split_bin + 1):len_comp_vec])
            }

            # normalize
            true_len_comp <- true_len_comp / sum(true_len_comp)

            if (apply_error)
            {
                new_len_comp <- data.frame(matrix(0,nrow=ngend,ncol=nbins,dimnames=list(c(),exp_dat_struct$lbin_vector)))

                # AIEEEEEE!!! magic numbers selected from the aether
                nsamples <- floor(max(8,nbins)^1.75)

                for (i in 1:ngend)
                {
                    # generate a multinomial sample
                    sum_len_comp <- sum(true_len_comp[i,])
                    if (sum_len_comp > 0)
                    {
                        new_len_comp[i,] <- sum_len_comp * (rmultinom(1,nsamples,(true_len_comp[i,] / sum_len_comp)) / nsamples)
                    }
                }
            } else {
                new_len_comp <- true_len_comp
            }
        }
    }

    return(new_len_comp)
}
