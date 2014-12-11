##' this function adds a new line for a CPUE/survey observation in the CPUE data structure for the SS DAT file
##'
##' @param dat_struct - DAT structure to be edited
##' @param CPUE_year - year for the CPUE observation
##' @param CPUE_seas - season for the CPUE observation
##' @param CPUE_fleet - fleet for the CPUE observation
##' @param CPUE_obs - CPUE observation
##' @param CPUE_std_err - standard error for the CPUE observation
##' @return edited DAT structure
##' @export
##'

sim_add_CPUE <- function(dat_struct=NULL,CPUE_year=-1,CPUE_seas=-1,CPUE_fleet=-1,CPUE_obs=-999.0,CPUE_std_err=999.0)
{
    new_dat_struct <- NULL

    if (!is.null(dat_struct) && CPUE_year > 0 && CPUE_seas > 0 && CPUE_fleet > 0 && is.numeric(CPUE_obs) && CPUE_std_err > 0.0)
    {
        new_dat_struct <- dat_struct

        if (CPUE_fleet <= (dat_struct$Nfleet+dat_struct$Nsurveys) && CPUE_seas <= dat_struct$nseas)
        {
            new_dat_struct$CPUE   <- rbind(dat_struct$CPUE,c(CPUE_year,CPUE_seas,CPUE_fleet,CPUE_obs,CPUE_std_err))
            new_dat_struct$N_cpue <- nrow(new_dat_struct$CPUE)
        }
    }

    return(new_dat_struct)
}
