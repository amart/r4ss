##' this function retrieves the most recent selectivity-at-age from the SS report file structure
##'
##' @param dat_struct - structure for DAT file
##' @param rep_struct - structure returned by SS_output()
##' @param fleet_num - fleet number
##' @return structure with selectivity-at-age
##' @export
##'

sim_get_age_selex <- function(dat_struct=NULL,rep_struct=NULL,fleet_num=-1)
{
    age_selex <- NULL

    if (!is.null(dat_struct) && !is.null(rep_struct) && fleet_num > 0)
    {
        # estimated selectivity-at-age
        fleet_age_selex <- subset(rep_struct$ageselex,rep_struct$ageselex$fleet == fleet_num)

        # get the selectivity-at-age vectors only
        fleet_age_selex <- subset(fleet_age_selex,fleet_age_selex$factor == "Asel")

        # get the most recent selectivity-at-age
        fleet_age_selex <- subset(fleet_age_selex,fleet_age_selex$year == max(fleet_age_selex$year))

        if (dim(fleet_age_selex)[1] == dat_struct$Ngenders)
        {
            nages     <- dat_struct$Nages

            age_selex <- fleet_age_selex[,(-(dim(fleet_age_selex)[2] - nages - 1)):-1]
        }
    }

    return(age_selex)
}
