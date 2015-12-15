##' this function maps values from population bins to values for data bins
##'
##' @param pop_vector - vector of bin values for input values
##' @param data_vector - vector of bin values for output values
##' @param comp_matrix - input values (matrix)
##' @return output values (matrix)
##' @export
##'

sim_map_bins_to_bins <- function(pop_vector=NULL,data_vector=NULL,comp_matrix=NULL)
{
    new_comp_matrix <- NULL

    if (!is.null(pop_vector) && !is.null(data_vector) && !is.null(comp_matrix))
    {
        num_pop_rows  <- dim(comp_matrix)[1]
        num_pop_bins  <- length(pop_vector)
        num_data_bins <- length(data_vector)

        if (num_pop_rows > 0 && num_pop_bins > 0 && num_data_bins > 0)
        {
            new_comp_matrix <- matrix(0.0,nrow=num_pop_rows,ncol=num_data_bins)

            # find the first population bin greater than or equal to the first data bin
            for (p_idx in 1:num_pop_bins)
            {
                if (pop_vector[p_idx] >= data_vector[1])
                {
                    break
                }
            }

            # sum the old bins into the new bins
            for (d_idx in 1:(num_data_bins-1))
            {
                lower_data_bin <- data_vector[d_idx]
                upper_data_bin <- data_vector[d_idx+1]

                for (local_p_idx in p_idx:num_pop_bins)
                {
                    if (pop_vector[local_p_idx] >= lower_data_bin && pop_vector[local_p_idx] < upper_data_bin)
                    {
                        for (j in 1:num_pop_rows)
                        {
                            new_comp_matrix[j,d_idx] <- new_comp_matrix[j,d_idx] + comp_matrix[j,local_p_idx]
                        }
                    } else {
                        break
                    }
                }

                p_idx <- local_p_idx
            }

            # if the last population bin is greater than or equal to the last data bin, then accumulate trailing tail
            if (pop_vector[p_idx] >= data_vector[num_data_bins])
            {
                for (local_p_idx in p_idx:num_pop_bins)
                {
                    for (j in 1:num_pop_rows)
                    {
                        new_comp_matrix[j,num_data_bins] <- new_comp_matrix[j,num_data_bins] + comp_matrix[j,local_p_idx]
                    }
                }
            }
        }
    }

    return(new_comp_matrix)
}
