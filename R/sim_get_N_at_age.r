##' this function retrieves the numbers-at-age for the year, season, and fleet
##'
##' @param dat_struct - DAT structure
##' @param rep_struct - structure returned by SS_output()
##' @param ret_year - year for the numbers-at-age
##' @param ret_seas - season for the numbers-at-age
##' @param fleet_num - fleet number
##' @return structure with numbers-at-age
##' @export
##'

sim_get_N_at_age <- function(dat_struct=NULL,rep_struct=NULL,ret_year=-1,ret_seas=-1,fleet_num=-1)
{
    new_N_at_age <- NULL

    if (!is.null(dat_struct) && !is.null(rep_struct) && ret_year > 0 && ret_seas > 0 && fleet_num > 0)
    {
        fleet_name   <- dat_struct$fleetname[fleet_num]

        surveytiming <- dat_struct$fleetinfo1["surveytiming",fleet_name]

        # get numbers-at-age, in thousands
        true_natage <- subset(subset(subset(rep_struct$natage,rep_struct$natage$"Beg/Mid" == ifelse(surveytiming == 0,"B","M")),Yr == ret_year),Seas == ret_seas)

        if (dim(true_natage)[1] > 0)
        {
            new_N_at_age <- true_natage
        }
    }

    return(new_N_at_age)
}


