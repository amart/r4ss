##' this function sets the variable endyr in the data structure for the SS DAT file
##'
##' @param dat_struct - DAT structure to be edited
##' @param endyr - value to replace endyr
##' @return edited DAT structure
##' @export
##'

sim_set_endyr <- function(dat_struct=NULL,endyr=-1)
{
    new_dat_struct <- NULL

    if (!is.null(dat_struct))
    {
        new_dat_struct <- dat_struct

        if (endyr > 0)
        {
            new_dat_struct$endyr <- endyr
        }
    }

    return(new_dat_struct)
}
