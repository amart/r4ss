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

    if (!is.null(dat_struct) && !is.null(comp_matrix))
    {
        new_comp_matrix <- comp_matrix
    }

    return(new_comp_matrix)
}


