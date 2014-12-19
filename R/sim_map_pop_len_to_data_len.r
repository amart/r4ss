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

    if (!is.null(dat_struct) && !is.null(comp_matrix))
    {
        new_comp_matrix <- comp_matrix
    }

    return(new_comp_matrix)
}


