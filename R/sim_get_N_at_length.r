##' this function retrieves the numbers-at-length for the year, season, and fleet
##'
##' @param dat_struct - DAT structure
##' @param rep_struct - structure returned by SS_output()
##' @param ret_year - year for the numbers-at-length
##' @param ret_seas - season for the numbers-at-length
##' @param fleet_num - fleet number
##' @return structure with numbers-at-length
##' @export
##'

sim_get_N_at_length <- function(dat_struct=NULL,rep_struct=NULL,ret_year=-1,ret_seas=-1,fleet_num=-1)
{
    new_N_at_length <- NULL

    if (!is.null(dat_struct) && !is.null(rep_struct) && ret_year > 0 && ret_seas > 0 && fleet_num > 0)
    {
        ngend        <- dat_struct$Ngenders

        fleet_name   <- dat_struct$fleetname[fleet_num]

        surveytiming <- dat_struct$fleetinfo1["surveytiming",fleet_name]

        # get numbers-at-length, in thousands
        true_natlen <- subset(subset(subset(rep_struct$natlen,rep_struct$natlen$"Beg/Mid" == ifelse(surveytiming == 0,"B","M")),Yr == ret_year),Seas == ret_seas)

        if (dim(true_natlen)[1] == ngend)
        {
            nlens <- dat_struct$N_lbinspop

            # leave off the indexing columns
            new_N_at_length <- true_natlen[,(-(dim(true_natlen)[2] - nlens)):-1]
        } else {
            # check if the numbers-at-age for that year are available
            new_N_at_age <- sim_get_N_at_age(dat_struct,rep_struct,ret_year,ret_seas,fleet_num)

            if (!is.null(new_N_at_age) && dim(new_N_at_age)[1] == ngend)
            {
                new_N_at_length <- matrix(0,nrow=ngend,ncol=nlens)

                for (i in 1:ngend)
                {
                    # rep_struct$ALK:  the age-length transition matrix
                    # dimensions dat_struct$N_lbinspop x (dat_struct$Nages + 1) x dat_struct$Ngenders
                    new_N_at_length[i,] <- as.matrix(new_N_at_age[i,],nrow=1,byrow=TRUE) %*% as.matrix(t(rep_struct$ALK[,,i]))
                }
            }
        }
    }

    return(new_N_at_length)
}


