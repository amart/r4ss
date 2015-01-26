##' this function generates an age composition observation given the expected values for the fleet in the data structure for the SS DAT file
##'
##' @param exp_dat_struct - DAT structure with the expected values
##' @param exp_rep_struct - structure returned by SS_output()
##' @param gen_year - year for the generated composition observation
##' @param gen_seas - season number for the generated composition observation
##' @param gen_fleet - fleet number for the generated composition observation
##' @param apply_error - apply error to the generated composition observation
##' @return generated age composition matrix with sample size
##' @export
##'

sim_generate_age_comp_from_expected <- function(exp_dat_struct=NULL,exp_rep_struct=NULL,gen_year=-1,gen_seas=-1,gen_fleet=-1,apply_error=FALSE)
{
    new_age_comp <- NULL

    if (!is.null(exp_dat_struct) && !is.null(exp_rep_struct) && gen_year > 0 && gen_seas > 0 && gen_fleet > 0)
    {
        # get expected age composition
        exp_agecomp <- subset(subset(subset(exp_dat_struct$agecomp,exp_dat_struct$agecomp$Yr == gen_year),Seas == gen_seas),FltSvy == gen_fleet)

        # check that one row is returned
        if (dim(exp_agecomp)[1] == 1)
        {
            ngend <- exp_dat_struct$Ngenders

            nbins <- exp_dat_struct$N_agebins

            # map the whole comp vector into ngend vectors
            true_age_comp <- matrix(0,nrow=ngend,ncol=nbins,dimnames=list(c(),exp_dat_struct$agebin_vector))
            len_comp_vec <- length(exp_agecomp)
            split_bin <- len_comp_vec - nbins
            if (ngend == 2)
            {
                true_age_comp[1,] <- t(exp_agecomp[(split_bin - nbins + 1):split_bin])
                true_age_comp[2,] <- t(exp_agecomp[(split_bin + 1):len_comp_vec])
            } else {
                true_age_comp[1,] <- t(exp_agecomp[(split_bin + 1):len_comp_vec])
            }

            # normalize
            true_age_comp <- true_age_comp / sum(true_age_comp)

            if (apply_error)
            {
                new_age_comp <- data.frame(matrix(0,nrow=ngend,ncol=nbins,dimnames=list(c(),exp_dat_struct$agebin_vector)))

                # AIEEEEEE!!! magic numbers selected from the aether
                nsamples <- floor(max(8,nbins)^1.75)

                for (i in 1:ngend)
                {
                    # generate a multinomial sample
                    sum_age_comp <- sum(true_age_comp[i,])
                    if (sum_age_comp > 0)
                    {
                        new_age_comp[i,] <- sum_age_comp * (rmultinom(1,nsamples,(true_age_comp[i,] / sum_age_comp)) / nsamples)
                    }
                }
            } else {
                new_age_comp <- true_age_comp
            }
        }
    }

    return(new_age_comp)
}
