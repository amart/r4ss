##' this function adds a new line for a length composition observation in the lencomp data structure for the SS DAT file
##'
##' @param dat_struct - DAT structure to be edited
##' @param comp_year - year for the length comp observation
##' @param comp_seas - season for the length comp observation
##' @param comp_fleet - fleet for the length comp observation
##' @param comp_Nsamp - sample size for the length comp observation
##' @param comp_gender - gender indicator for the length comp observation (0 combine M & F, 1 F only, 2 M only, 3 separate M & F)
##' @param comp_part - partition indicator for the length comp observation (0 combined, 1 discard, 2 retained)
##' @param comp_matrix - length comp observation matrix, by sex
##' @return edited DAT structure
##' @export
##'

sim_add_length_comp <- function(dat_struct=NULL,comp_year=-1,comp_seas=-1,comp_fleet=-1,comp_Nsamp=-1,comp_gender=-1,comp_part=-1,comp_matrix=NULL)
{
    new_dat_struct <- NULL

    if (!is.null(dat_struct) && comp_year > 0 && comp_seas > 0 && comp_fleet > 0 && comp_Nsamp > 0 && comp_gender >= 0 && comp_part >= 0 && !is.null(comp_matrix))
    {
        if (comp_fleet <= (dat_struct$Nfleet+dat_struct$Nsurveys) && comp_seas <= dat_struct$nseas)
        {
            new_dat_struct <- dat_struct

            ngend <- dat_struct$Ngenders

            # columns:  Yr Seas Fleet Gender Part Nsamp Data (females then males)

            # put both length comp vectors in one row if there are two sexes
            if (ngend == 2 && dim(comp_matrix)[1] == ngend)
            {
                if (comp_gender == 0)
                {
                    # sum M and F into F vector
                    comp_matrix[1,] <- comp_matrix[1,] + comp_matrix[2,]
                } else
                if (comp_gender == 1 || comp_gender == 2)
                {
                    # normalize each sex-specific vector
                    for (s in 1:2)
                    {
                        comp_sum <- sum(comp_matrix[s,])
                        if (comp_sum > 0)
                        {
                            comp_matrix[s,] <- comp_matrix[s,] / comp_sum
                        }
                    }
                }

                new_dat_struct$lencomp <- rbind(dat_struct$lencomp,c(comp_year,comp_seas,comp_fleet,comp_gender,comp_part,comp_Nsamp,as.vector(comp_matrix[1,]),as.vector(comp_matrix[2,])))
            } else {
                new_dat_struct$lencomp <- rbind(dat_struct$lencomp,c(comp_year,comp_seas,comp_fleet,comp_gender,comp_part,comp_Nsamp,as.vector(comp_matrix[1,])))
            }

            new_dat_struct$N_lencomp <- nrow(new_dat_struct$lencomp)
        }
    }

    return(new_dat_struct)
}


