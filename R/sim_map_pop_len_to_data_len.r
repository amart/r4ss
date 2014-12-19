##' this function maps a population length bins length composition observation to a data length bins length composition observation
##'
##' @param dat_struct - DAT structure
##' @param comp_matrix - length composition structure to be edited
##' @return edited length composition structure
##' @export
##'

sim_map_pop_len_to_data_len <- function(dat_struct=NULL,comp_matrix=NULL)
{
    new_comp_matrix <- NULL

    if (!is.null(dat_struct) && !is.null(comp_matrix) && dat_struct$N_lbinspop >= dat_struct$N_lbins)
    {
        ngend <- dat_struct$Ngenders

        num_pop_bins  <- dat_struct$N_lbinspop
        pop_vector    <- dat_struct$lbin_vector_pop

        num_data_bins <- dat_struct$N_lbins
        data_vector   <- dat_struct$lbin_vector

        new_comp_matrix <- sim_map_bins_to_bins(pop_vector,data_vector,comp_matrix)
    }

    return(new_comp_matrix)
}


