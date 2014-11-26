##' this function generates a length composition observation for the fleet in the data structure for the SS DAT file
##'
##' @param dat_struct - DAT structure
##' @param rep_struct - structure returned by SS_output()
##' @param gen_fleet - fleet number
##' @param gen_year - year for the generated composition observation
##' @param apply_error - apply error to the generated observation (NOT IMPLEMENTED)
##' @return generated age composition vector with sample size
##' @export
##'

sim_generate_length_comp <- function(dat_struct=NULL,rep_struct=NULL,gen_fleet=-1,gen_year=-1,apply_error=FALSE)
{
    new_len_comp_struct <- NULL

    return(new_len_comp_struct)
}
