##' this function retrieves the most recent selectivity-at-age from the SS report file structure
##'
##' @param rep_struct - structure returned by SS_output()
##' @param fleet_num - fleet number
##' @return structure with selectivity-at-age
##' @export
##'

sim_get_age_selex <- function(rep_struct=NULL,fleet_num=-1)
{
    age_selex <- NULL

    if (!is.null(rep_struct) && fleet_num > 0)
    {
        # estimated selectivity-at-age
        fleet_age_selex <- subset(rep_struct$ageselex,rep_struct$ageselex$fleet == fleet_num)

        if (dim(fleet_age_selex)[1] > 0)
        {
            # get the most recent selectivity-at-age
            age_selex <- subset(fleet_age_selex,fleet_age_selex$year == max(fleet_age_selex$year))
        }
    }

    return(age_selex)
}
