library("dplyr")
library("tidyr")
library("devtools")
library("testthat")

#' Read CSV
#' @param filename file name of data
#' @return Extracts csv data
#' \cr On error, message "file... does not exist" is issued.
#' @examples
#' fars_read("accident_2013.csv.bz2")
#' @importFrom dplyr tbl_df

fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}


#' Creates a file name following the prescribed format
#' @param year Year data was collected
#' @return File name using  the prescribed format
#' \cr concatenates accident_+year+.csv.bz2
#' @examples
#' make_filename(2015)

make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Filter by year
#' @param years calendar year of selection
#' @return Returns records of accidents for the selected year by month.
#' \cr On error, warning "invalid year" is issued
#' @examples
#' fars_read_years(2015)
#' @importFrom dplyr mutate
#' @importFrom dplyr select

fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Summary of accidents by year
#' @param years calender year of accidents to be retrieved
#' @return summary of accidents for the year by month
#' @examples
#' fars_summarize_years(2015)
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' @importFrom dplyr bind_rows

fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Map of accidents
#' @param state.num number of State
#' @param year year of data
#' @return Retruns map of selected state and location of accidents
#' \cr Issues error "invalid State number" if state number provided does not exist
#' \cr Issues message if no records are retrieved for the selection
#' @examples
#' fars_map_state(1,2015)
#' @importFrom maps map
#' @importFrom graphics points
#' @importFrom dplyr filter
#' @export

fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
#travis
