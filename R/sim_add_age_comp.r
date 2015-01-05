##' this function adds a new line for an age composition observation in the agecomp data structure for the SS DAT file
##'
##' @param dat_struct - DAT structure to be edited
##' @param comp_year - year for the age comp observation
##' @param comp_seas - season for the age comp observation
##' @param comp_fleet - fleet for the age comp observation
##' @param comp_Nsamp - sample size for the age comp observation
##' @param comp_gender - gender indicator for the age comp observation (0 combine M & F, 1 F only, 2 M only, 3 separate M & F)
##' @param comp_part - partition indicator for the age comp observation (0 combined, 1 discard, 2 retained)
##' @param comp_matrix - age comp observation matrix, by sex
##' @return edited DAT structure
##' @export
##'

sim_add_age_comp <- function(dat_struct=NULL,comp_year=-1,comp_seas=-1,comp_fleet=-1,comp_Nsamp=-1,comp_gender=-1,comp_part=-1,comp_matrix=NULL)
{
    new_dat_struct <- NULL

    if (!is.null(dat_struct) && comp_year > 0 && comp_seas > 0 && comp_fleet > 0 && comp_Nsamp > 0 && comp_gender >= 0 && comp_part >= 0 && !is.null(comp_matrix))
    {
        if (comp_fleet <= (dat_struct$Nfleet+dat_struct$Nsurveys) && comp_seas <= dat_struct$nseas)
        {
            new_dat_struct <- dat_struct

            ngend <- dat_struct$Ngenders

            # columns:  Yr   Seas Fleet Gender Part Ageerr Lbin_lo Lbin_hi Nsamp Data (females then males)

            # put both age comp vectors in one row if there are two sexes
            if (ngend == 2 && dim(comp_matrix)[1] == ngend)
            {
                new_dat_struct$agecomp <- rbind(dat_struct$agecomp,c(comp_year,comp_seas,comp_fleet,comp_gender,comp_part,1,-1,-1,comp_Nsamp,as.vector(comp_matrix[1,]),as.vector(comp_matrix[2,])))
            } else {
                new_dat_struct$agecomp <- rbind(dat_struct$agecomp,c(comp_year,comp_seas,comp_fleet,comp_gender,comp_part,1,-1,-1,comp_Nsamp,as.vector(comp_matrix[1,])))
            }

            new_dat_struct$N_agecomp <- nrow(new_dat_struct$agecomp)
        }
    }

    return(new_dat_struct)
}


