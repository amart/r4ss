##' this function adds a new line for an observation in the discard data structure for the SS DAT file
##'
##' @param dat_struct - DAT structure to be edited
##' @param dis_year - year for the discard observation
##' @param dis_seas - season for the discard observation
##' @param dis_fleet - fleet for the discard observation
##' @param dis_obs - discard observation
##' @param dis_std_err - standard error for the discard observation
##' @return edited DAT structure
##' @export
##'

sim_add_discard <- function(dat_struct=NULL,dis_year=-1,dis_seas=-1,dis_fleet=-1,dis_obs=-999.0,dis_std_err=999.0)
{
    new_dat_struct <- NULL

    if (!is.null(dat_struct) && dat_struct$N_discard_fleets > 0 && dis_year > 0 && dis_seas > 0 && dis_fleet > 0 && is.numeric(dis_obs) && dis_std_err > 0.0)
    {
        new_dat_struct <- dat_struct

        if (dis_fleet <= (dat_struct$Nfleet+dat_struct$Nsurveys) && dis_seas <= dat_struct$nseas)
        {
            new_dat_struct$discard_data <- rbind(dat_struct$discard_data,c(dis_year,dis_seas,dis_fleet,dis_obs,dis_std_err))
            new_dat_struct$N_discard    <- nrow(new_dat_struct$discard_data)
        }
    }

    return(new_dat_struct)
}
