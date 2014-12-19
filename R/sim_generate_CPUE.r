##' this function generates a CPUE observation for the CPUE dataframe in the data structure for the SS DAT file
##'
##' @param dat_struct - DAT structure
##' @param rep_struct - structure returned by SS_output()
##' @param CPUE_year - year for the generated CPUE observation
##' @param CPUE_seas - season for the generated CPUE observation
##' @param CPUE_fleet - fleet number for the generated CPUE observation
##' @param apply_error - apply error to the generated observation (NOT IMPLEMENTED)
##' @return structure with generated CPUE_obs and CPUE_std_err
##' @export
##'

sim_generate_CPUE <- function(dat_struct=NULL,rep_struct=NULL,CPUE_year=-1,CPUE_seas=-1,CPUE_fleet=-1,apply_error=FALSE)
{
    new_CPUE_struct <- NULL

    if (!is.null(dat_struct) && !is.null(rep_struct) && CPUE_year > 0 && CPUE_seas > 0 && CPUE_fleet > 0)
    {
        CPUE_obs <- 0.0
        CPUE_q   <- 0.0

        ngend        <- dat_struct$Ngenders
        nages        <- dat_struct$Nages
        fleet_name   <- dat_struct$fleetname[CPUE_fleet]
        surveytiming <- dat_struct$fleetinfo1["surveytiming",fleet_name]

        # get numbers-at-age, in thousands
        true_natage <- sim_get_N_at_age(dat_struct,rep_struct,CPUE_year,CPUE_seas,CPUE_fleet)

        # get the most recent selectivity-at-age for this fleet
        true_ageselex <- sim_get_age_selex(dat_struct,rep_struct,CPUE_fleet)

        # check that the number of genders matches the number of rows
        if (dim(true_natage)[1] == ngend && dim(true_ageselex)[1] == ngend)
        {
            natage     <- true_natage

            ageselex   <- true_ageselex

            CPUE_units <- dat_struct$CPUEinfo[dat_struct$CPUEinfo$Fleet == CPUE_fleet,]$Units

            if (CPUE_units == 0)
            {
                # calculate total srv numbers

                # estimated survey abundance
                for (i in 1:ngend)
                {
                    CPUE_obs <- CPUE_obs + sum(natage[i,] * ageselex[i,])
                }
            } else
            if (CPUE_units == 1)
            {
                # calculate total srv biomass

                true_wtatage <- subset(rep_struct$wtatage,rep_struct$wtatage$fleet == ifelse(surveytiming == 0,0,-1))

                # get the most recent wt-at-age (the years are all negative, hence min())
                true_wtatage <- subset(true_wtatage,true_wtatage$yr == min(true_wtatage$yr))

                wtatage      <- true_wtatage[,(-(dim(true_wtatage)[2] - nages - 1)):-1]

                # estimated survey biomass
                for (i in 1:ngend)
                {
                    CPUE_obs <- CPUE_obs + sum(natage[i,] * ageselex[i,] * wtatage[i,])
                }
            }

            # estimated CPUE/survey index
            true_est <- subset(rep_struct$cpue,rep_struct$cpue$Fleet == CPUE_fleet)

            # get the most recent CPUE/survey index
            true_est <- subset(true_est,true_est$Yr == max(true_est$Yr))

            if (dim(true_est)[1] == 1)
            {
                CPUE_q <- true_est$Calc_Q
            }

            # now apply estimated catchability
            CPUE_obs <- CPUE_q * CPUE_obs
        }

        # get time series of CPUE obs and standard errors
        surveyobs       <- dat_struct$CPUE[dat_struct$CPUE$index == CPUE_fleet,]

        # generate the standard error
        CPUE_std_err    <- sample(surveyobs$se_log,1)


        new_CPUE_struct <- data.frame(obs=CPUE_obs,se_log=CPUE_std_err)
    }

    return(new_CPUE_struct)
}
