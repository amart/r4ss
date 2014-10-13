# Run SS simulations
# ZTA, 2014-10-13
# R version 3.1.1, 32-bit

# this function adds a new line for catch in the catch dataframe in the data structure for the SS DAT file

sim_add_catch <- function(dat_struct=NULL,catch_vec=NULL,seas=-1,catch_year=-1)
{
    new_dat_struct <- NULL

    if (!is.null(dat_struct) && !is.null(catch_vec) && seas > 0 && catch_year > 0)
    {
        new_dat_struct <- dat_struct

        if (seas <= dat_struct$nseas && length(catch_vec) == dat_struct$Nfleet)
        {
            new_dat_struct$catch   <- rbind(dat_struct$catch,c(catch_vec,catch_year,seas))
            new_dat_struct$N_catch <- dat_struct$N_catch + 1
        }
    }

    return(new_dat_struct)
}
