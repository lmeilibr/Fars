#' Read file
#'
#' This is a function that reads a file and transforms it on a data frame
#'
#' @param filename Name of the file to be read
#'
#' @importFrom dplyr tbl_df
#'
#' @importFrom readr read_csv
#'
#' @return This function returns the data as tbl_df(data)
#'
#' @examples
#' \dontrun{
#' fars_read()
#'}
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Create string with the filename and year
#'
#' This function creates a string composed by the prefix accident_ and year
#'
#' @param year An integer with the value of the year
#'
#' @return This function returns a string with the filename
#'
#' @examples
#' \dontrun{
#' make_filename(2013)
#' make_filename(2014)
#' make_filename(2015)
#'}
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read a range of years
#'
#' This function reads a range of years, from 2013 to 2015
#'
#' @param years A vector with the years value
#'
#' @importFrom dplyr mutate
#'
#' @importFrom dplyr select
#'
#' @note If the year doesn't exist, the function throws an error
#'
#' @return This function returns a data frame summarized by years and month
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013)
#' fars_read_years(c(2013,2014))
#' fars_read_years(c(2013,2014,2015))
#'}
#'
#' @export
fars_read_years <- function(years) {
      MONTH <- NULL;
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

#' Summarize years
#'
#' This function summarize the years, grouping the rows by year and month
#'
#' @inheritParams fars_read_years
#'
#' @import dplyr
#'
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2013)
#' fars_summarize_years(c(2013,2014)
#' fars_summarize_years(c(2013,2014,2015))
#'}
#'
#' @export
fars_summarize_years <- function(years) {
        MONTH <- NULL
        year <- NULL
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Plot accidents data by state num and year
#'
#' This function filters the data set by state number and year and plot the
#' results in a map
#'
#' @param state.num State number
#'
#' @inheritParams fars_read_years
#'
#' @inheritParams make_filename
#'
#' @importFrom dplyr filter
#'
#' @importFrom maps map
#'
#' @importFrom graphics points
#'
#' @return Map with the points where the accidents occured
#'
#' @examples
#' \dontrun{
#' fars_map_state(1,2013)
#' }

fars_map_state <- function(state.num, year) {
        STATE <- NULL
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
