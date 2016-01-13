##' this function gets the catch for the first forecast year from the Forecast-report.sso file
##'
##' @param forecast_report_path - full path for Forecast-report.sso file
##' @param num_fleets - number of fishing fleets, specified in the DAT file
##' @param num_seasons - number of seasons, specified in the DAT file
##' @param num_fc_years - number of forecast years, specified in the forecast.ss file
##' @return matrix of catch by fleet (columns) and seasons (rows)
##' @export
##'

sim_get_forecast_catch_by_fleet <- function(forecast_report_path,num_fleets=0,num_seasons=0,num_fc_years=0)
{
    catch_proj <- NULL

    forecast_report_filename <- file.path(forecast_report_path,"/Forecast-report.sso")

    # check if file exists - thanks, Ian
    temp_file_size <- file.info(forecast_report_filename)$size

    if(!is.na(temp_file_size) && temp_file_size > 0)
    {
        # read the forecast report file
        fc_table <- read.table(file=forecast_report_filename,col.names=1:200,fill=TRUE,quote="",colClasses="character",nrows=-1)
        num.rows.fct  <- dim(fc_table)[1]

        if (num.rows.fct > 0 && num_fleets > 0 && num_seasons > 0 && num_fc_years > 0)
        {
            # in which row are the column labels?
            label_row <- num.rows.fct - ((num_fc_years * num_seasons) + 1)

            # get the columns with "dead(B):_[N]"
            catch_cols <- which(startsWith(fc_table[label_row,],"dead(B):_") == TRUE)

            if (length(catch_cols) == num_fleets)
            {
                catch_proj <- matrix(0.0,nrow=num_seasons,ncol=num_fleets)

                for (i in 1:num_seasons)
                {
                    # get the forecast/projected catch for the following year for each season
                    proj_row  <- label_row + i

                    # get the catch vector for the next/projected year
                    catch_proj[i,] <- as.numeric(fc_table[proj_row,catch_cols])
                }
            }
        }
    }

    return(catch_proj)
}

