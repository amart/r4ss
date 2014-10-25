##' this function adds new lines for catch in the catch dataframe in the data structure
##' for the SS DAT file
##'
##' @param dat_struct - DAT file data structure to be edited
##' @param catch_mat - catches for each season (rows) and fleet (cols) (matrix)
##' @param catch_year - year that the catches will be applied
##' @return edited DAT structure
##' @export
##'

sim_add_catch <- function(dat_struct=NULL,catch_mat=NULL,catch_year=-1)
{
    new_dat_struct <- NULL

    if (!is.null(dat_struct) && !is.null(catch_mat) && dat_struct$nseas > 0 && catch_year > 0)
    {
        new_dat_struct <- dat_struct

        if (dim(catch_mat)[1] == dat_struct$nseas && dim(catch_mat)[2] == dat_struct$Nfleet)
        {
            catch_proj_df          <- data.frame(cbind(catch_mat,catch_year,seq(1,dat_struct$nseas)))
            names(catch_proj_df)   <- names(dat_struct$catch)
            new_dat_struct$catch   <- rbind(dat_struct$catch,catch_proj_df)
            new_dat_struct$N_catch <- dat_struct$N_catch + dat_struct$nseas
        }
    }

    return(new_dat_struct)
}
