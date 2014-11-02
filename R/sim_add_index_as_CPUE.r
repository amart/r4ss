##' this function adds a new line for an index observation in the CPUE dataframe in the data structure for the SS DAT file
##'
##' @param dat_struct - DAT structure to be edited
##' @param index_year - year for the index observation
##' @param seas - season for the index observation
##' @param index_fleet - fleet for the index observation
##' @param index_obs - CPUE or index observation
##' @param index_std_err - standard error for the index observation
##' @return edited DAT structure
##' @export
##'

sim_add_index_as_CPUE <- function(dat_struct=NULL,index_year=-1,seas=-1,index_fleet=-1,index_obs=-999,index_std_err=999.0)
{
    new_dat_struct <- NULL

    if (!is.null(dat_struct) && index_year > 0 && seas > 0 && index_fleet > 0 && index_std_err > 0.0)
    {
        new_dat_struct <- dat_struct

        if (index_fleet <= (dat_struct$Nfleet+dat_struct$Nsurveys) && seas <= dat_struct$nseas)
        {
            new_dat_struct$CPUE   <- rbind(dat_struct$CPUE,c(index_year,seas,index_fleet,index_obs,index_std_err))
            new_dat_struct$N_cpue <- nrow(new_dat_struct$CPUE)
        }
    }

    return(new_dat_struct)
}
