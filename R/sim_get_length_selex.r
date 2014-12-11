##' this function retrieves the most recent selectivity-at-length from the SS report file structure
##'
##' @param rep_struct - structure returned by SS_output()
##' @param fleet_num - fleet number
##' @return structure with selectivity-at-length
##' @export
##'

sim_get_length_selex <- function(rep_struct=NULL,fleet_num=-1)
{
    length_selex <- NULL

    if (!is.null(rep_struct) && fleet_num > 0)
    {
        # estimated selectivity-at-length
        fleet_len_selex <- subset(rep_struct$sizeselex,rep_struct$sizeselex$Fleet == fleet_num)

        if (dim(fleet_len_selex)[1] > 0)
        {
            # get the most recent selectivity-at-length
            length_selex <- subset(fleet_len_selex,fleet_len_selex$year == max(fleet_len_selex$year))
        }
    }

    return(length_selex)
}
