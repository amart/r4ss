##' this function retrieves the most recent selectivity-at-length from the SS report file structure
##'
##' @param dat_struct - structure for DAT file
##' @param rep_struct - structure returned by SS_output()
##' @param fleet_num - fleet number
##' @return structure with selectivity-at-length
##' @export
##'

sim_get_length_selex <- function(dat_struct=NULL,rep_struct=NULL,fleet_num=-1)
{
    length_selex <- NULL

    if (!is.null(dat_struct) && !is.null(rep_struct) && fleet_num > 0)
    {
        # estimated selectivity-at-length
        fleet_len_selex <- subset(rep_struct$sizeselex,rep_struct$sizeselex$Fleet == fleet_num)

        # get the selectivity-at-length vectors only
        fleet_len_selex <- subset(fleet_len_selex,fleet_len_selex$Factor == "Lsel")

        # get the most recent selectivity-at-length
        fleet_len_selex <- subset(fleet_len_selex,fleet_len_selex$year == max(fleet_len_selex$year))

        if (dim(fleet_len_selex)[1] == dat_struct$Ngenders)
        {
            nlens        <- dat_struct$N_lbinspop

            length_selex <- fleet_len_selex[,(-(dim(fleet_len_selex)[2] - nlens)):-1]
        }
    }

    return(length_selex)
}
