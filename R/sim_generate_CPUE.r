##' this function generates a CPUE observation for the CPUE dataframe in the data structure for the SS DAT file
##'
##' @param dat_struct - DAT structure to be edited
##' @param rep_struct - structure returned by SS_output()
##' @param CPUE_fleet - fleet number of the CPUE fleet
##' @param CPUE_fleet_name - fleet name of the CPUE fleet
##' @param CPUE_year - year for the generated CPUE observation
##' @return generated CPUE_obs and CPUE_std_err
##' @export
##'

sim_generate_CPUE <- function(dat_struct=NULL,rep_struct=NULL,CPUE_fleet=-1,CPUE_fleet_name="",CPUE_year=-1)
{
    new_CPUE_struct <- NULL

    if (!is.null(dat_struct) && !is.null(rep_struct) && CPUE_fleet > 0 && length(CPUE_fleet_name) > 0 && CPUE_year > 0)
    {
        ngend           <- dat_struct$Ngenders

        surveytiming    <- dat_struct$fleetinfo1["surveytiming",CPUE_fleet_name]

        # forecast numbers-at-age, in thousands
        forecast_natage <- subset(subset(subset(rep_struct$natage,rep_struct$natage$"Beg/Mid" == ifelse(surveytiming == 0,"B","M")),Yr == CPUE_year),Seas == CPUE_seas)


        # estimated srv selectivity-at-age
        srv_ageselex <- subset(rep_struct$ageselex,rep_struct$ageselex$fleet == CPUE_fleet)

        # get the most recent srv selectivity-at-age
        srv_ageselex <- subset(srv_ageselex,srv_ageselex$year == max(srv_ageselex$year))


        CPUE_obs <- 0.0
        CPUE_q   <- 0.0

        # check that the number of genders matches the number of rows
        if (dim(forecast_natage)[1] == ngend && dim(srv_ageselex)[1] == ngend)
        {
            # ick - hardcoded
            natage   <- forecast_natage[,-11:-1]
            ageselex <- srv_ageselex[,-7:-1]

            srv_units <- dat_struct$CPUEinfo[dat_struct$CPUEinfo$Fleet == CPUE_fleet,]$Units

            if (srv_units == 0)
            {
                # calculate total srv numbers

                # estimated survey abundance
                for (i in 1:ngend)
                {
                    CPUE_obs <- CPUE_obs + sum(natage[i,] * ageselex[i,])
                }
            } else
            if (srv_units == 1)
            {
                # calculate total srv biomass

                srv_wtatage <- subset(rep_struct$wtatage,rep_struct$wtatage$fleet == ifelse(surveytiming == 0,0,-1))

                # get the most recent wt-at-age
                srv_wtatage <- subset(srv_wtatage,srv_wtatage$yr == min(srv_wtatage$yr))


                # ick - hardcoded
                wtatage  <- srv_wtatage[,-6:-1]

                # estimated survey biomass
                for (i in 1:ngend)
                {
                    CPUE_obs <- CPUE_obs + sum(natage[i,] * ageselex[i,] * wtatage[i,])
                }
            }

            # estimated CPUE/survey index
            srv_est <- subset(rep_struct$cpue,rep_struct$cpue$Fleet == CPUE_fleet)

            # get the most recent CPUE/survey index
            srv_est <- subset(srv_est,srv_est$Yr == max(srv_est$Yr))

            if (dim(srv_est)[1] == 1)
            {
                CPUE_q <- srv_est$Calc_Q
            }

            # now apply estimated catchability
            CPUE_obs <- CPUE_q * CPUE_obs
        }

        # generate the standard error
        surveyobs       <- dat_struct$CPUE[dat_struct$CPUE$index == CPUE_fleet,]

        CPUE_std_err    <- sample(surveyobs$se_log,1)


        new_CPUE_struct <- data.frame(obs=CPUE_obs,se_log=CPUE_std_err)
    }

    return(new_CPUE_struct)
}
