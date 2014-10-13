# Run SS simulations
# ZTA, 2014-10-13
# R version 3.1.1, 32-bit

# this function adds a new line for an index observation in the CPUE dataframe in the data structure for the SS DAT file

sim_add_index_as_CPUE <- function(dat_struct=NULL,index_year=-1,seas=-1,index_fleet=-1,index_obs=-999,index_std_err=999.0)
{
    new_dat_struct <- NULL

    if (dat_struct != NULL && index_year > 0 && seas > 0 && index_fleet > 0 && index_std_err > 0.0)
    {
        new_dat_struct <- dat_struct

        if (index_fleet <= dat_struct$Nfleet && seas <= dat_struct$nseas)
        {
            new_dat_struct$CPUE   <- rbind(dat_struct$CPUE,c(index_year,seas,index_fleet,index_obs,index_std_err))
            new_dat_struct$N_cpue <- dat_struct$N_cpue + 1
        }
    }

    return(new_dat_struct)
}
