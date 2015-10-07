##' this function generates an age composition observation for the fleet in the data structure for the SS DAT file
##'
##' @param dat_struct - DAT structure
##' @param rep_struct - structure returned by SS_output()
##' @param gen_year - year for the generated composition observation
##' @param gen_seas - season number
##' @param gen_fleet - fleet number
##' @param apply_error - apply error to the generated observation
##' @return generated age composition matrix with sample size
##' @export
##'

sim_generate_age_comp <- function(dat_struct=NULL,rep_struct=NULL,gen_year=-1,gen_seas=-1,gen_fleet=-1,apply_error=FALSE)
{
    new_age_comp <- NULL

    if (!is.null(dat_struct) && !is.null(rep_struct) && gen_year > 0 && gen_seas > 0 && gen_fleet > 0)
    {
        ngend <- dat_struct$Ngenders

        nages <- dat_struct$Nages

        # get numbers-at-age, in thousands
        true_natage   <- sim_get_N_at_age(dat_struct,rep_struct,gen_year,gen_seas,gen_fleet)

        # get the most recent selectivity-at-age for this fleet
        true_ageselex <- sim_get_age_selex(dat_struct,rep_struct,gen_fleet)

        # check that the number of genders matches the number of rows
        if (dim(true_natage)[1] == ngend && dim(true_ageselex)[1] == ngend)
        {
            # here they are, not normalized
            true_age_comp <- true_natage * true_ageselex

            # normalize
            sum_age_comp <- sum(true_age_comp)
            if (sum_age_comp > 0)
            {
                true_age_comp <- true_age_comp / sum_age_comp
            }

            if (apply_error)
            {
                new_age_comp <- data.frame(matrix(0,nrow=dim(true_age_comp)[1],ncol=dim(true_age_comp)[2]))
                names(new_age_comp) <- names(true_age_comp)

                nsamples <- sim_calculate_nsamples(nages)

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
