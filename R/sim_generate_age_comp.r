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
        true_ageselex <- sim_get_age_selex(rep_struct,gen_fleet)

        # check that the number of genders matches the number of rows
        if (dim(true_natage)[1] == ngend && dim(true_ageselex)[1] == ngend)
        {
            # the numbers-at-age values only
            natage   <- true_natage[,(-(dim(true_natage)[2] - nages - 1)):-1]

            # the selectivity-at-age values only
            ageselex <- true_ageselex[,(-(dim(true_ageselex)[2] - nages - 1)):-1]

            # here they are, not normalized
            true_age_comp <- natage * ageselex

            if (apply_error)
            {
                new_age_comp <- data.frame(matrix(0,nrow=dim(true_age_comp)[1],ncol=dim(true_age_comp)[2]))
                names(new_age_comp) <- names(true_age_comp)

                # AIEEEEEE!!! magic numbers selected from the aether
                nsamples <- floor(max(8,nages)^1.65)

                for (i in 1:ngend)
                {
                    # generate a multinomial sample
                    sum_age_comp <- sum(true_age_comp[i,])
                    new_age_comp[i,] <- sum_age_comp * (rmultinom(1,nsamples,(true_age_comp[i,] / sum_age_comp)) / nsamples)
                }
            } else {
                new_age_comp <- true_age_comp
            }
        }
    }

    return(new_age_comp)
}
