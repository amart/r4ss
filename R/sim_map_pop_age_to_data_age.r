##' this function maps a population age bins age composition observation to a data age bins age composition observation
##'
##' @param dat_struct - DAT structure
##' @param comp_matrix - age composition structure to be edited
##' @return edited age composition structure
##' @export
##'

sim_map_pop_age_to_data_age <- function(dat_struct=NULL,comp_matrix=NULL)
{
    new_comp_matrix <- NULL

    if (!is.null(dat_struct) && !is.null(comp_matrix) && dat_struct$Nages >= dat_struct$N_agebins)
    {
        ngend         <- dat_struct$Ngenders

        num_pop_bins  <- dat_struct$Nages + 1
        pop_vector    <- seq(0,dat_struct$Nages,1)

        num_data_bins <- dat_struct$N_agebins
        data_vector   <- dat_struct$agebin_vector

        new_comp_matrix <- sim_map_bins_to_bins(pop_vector,data_vector,comp_matrix)
    }

    return(new_comp_matrix)
}


